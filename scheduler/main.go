package main

import (
	"context"
	"net/http"
	"os"

	"github.com/google/go-github/github"
	serr "github.com/koki/structurederrors"
	"github.com/satori/go.uuid"
	"github.com/sirupsen/logrus"

	gc "github.com/koki/ci-lang/scheduler/githubclient"
	l "github.com/koki/ci-lang/scheduler/log"
	p "github.com/koki/ci-lang/scheduler/pipelines"
	"github.com/koki/ci-lang/scheduler/util"
)

// WebhookSecretVar is the name of an environment variable.
// GitHub uses the secret value to sign webhook requests.
const WebhookSecretVar = "GITHUB_WEBHOOK_SECRET"
const UserAccessTokenVar = "GITHUB_USER_ACCESS_TOKEN"

// Config contains the global settings for the webhook server.
type Config struct {
	/*
		TODO:
		Both WebhookSecret and UserAccessToken will need to be per-user.
	*/

	// WebhookSecret is used to validate webhook requests.
	WebhookSecret []byte
	// UserAccessToken is used to set the status on a Pull Request.
	UserAccessToken  string
	PipelinesContext *p.Context
}

// HandleWebhook implements the GitHub Webhook receiver endpoint.
func (c *Config) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	requestID := GenerateRequestID()
	logger := &l.Logger{Context: map[string]interface{}{
		"request_id": requestID,
	}}
	log := logger.StartLog()
	defer log.Write()

	log["request_url"] = r.URL.String()
	log["request_header"] = r.Header

	payload, err := github.ValidatePayload(r, c.WebhookSecret)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "attempting to read request body")
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	defer r.Body.Close()

	event, err := github.ParseWebHook(github.WebHookType(r), payload)
	if err != nil {
		log["request_body"] = string(payload)
		log["error"] = serr.ContextualizeErrorf(err, "parsing webhook")
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	switch event := event.(type) {
	case *github.PushEvent:
		log["push_event"] = event
	case *github.PullRequestEvent:
		log["pull_event"] = event
		go c.HandlePullEvent(event, requestID, logger)
	default:
		log["body"] = string(payload)
		log["other_event"] = true
	}
}

func GenerateRequestID() string {
	return uuid.Must(uuid.NewV4()).String()
}

// HandlePullEvent triggers a pipeline for a GitHub pull request event.
func (c *Config) HandlePullEvent(event *github.PullRequestEvent, requestID string, logger *l.Logger) {
	repo := event.GetRepo()
	sha := util.FromStringPtr(event.GetPullRequest().Head.SHA)
	client := gc.ClientWithToken(c.UserAccessToken)
	SetGitHubStatus(client, repo, sha, "pending", logger)

	repoURL := util.FromStringPtr(repo.CloneURL)
	repoName := util.FromStringPtr(repo.Name)
	pod := c.PipelinesContext.BuildPipelinePod(requestID, repoName, repoURL, sha)
	success := c.PipelinesContext.RunPipeline(&pod, logger)

	if success {
		SetGitHubStatus(client, repo, sha, "success", logger)
	} else {
		SetGitHubStatus(client, repo, sha, "failure", logger)
	}
}

// SetGitHubStatus returns false on failure.
func SetGitHubStatus(client *github.Client, repo *github.Repository, sha string, status string, logger *l.Logger) bool {
	owner := repo.Owner.GetLogin()
	repoName := util.FromStringPtr(repo.Name)
	statusObj := github.RepoStatus{
		State:   util.StringPtr(status),
		Context: util.StringPtr("koki-ci"),
	}

	receivedStatus, response, err := client.Repositories.CreateStatus(
		context.Background(),
		owner, repoName, sha,
		&statusObj,
	)
	if err != nil {
		log := logger.StartLog()
		log["error"] = serr.ContextualizeErrorf(err, "setting status")
		log["received_status"] = receivedStatus
		log["response_status"] = response.Status
		log.Write()
		return false
	}

	return true
}

func ReadEnv() *Config {
	webhookSecret := os.Getenv(WebhookSecretVar)
	if len(webhookSecret) == 0 {
		logrus.Fatalf("%s is missing", WebhookSecretVar)
	}

	userAccessToken := os.Getenv(UserAccessTokenVar)
	if len(userAccessToken) == 0 {
		logrus.Fatalf("%s is missing", UserAccessTokenVar)
	}

	pContext := p.ReadEnv().BuildContext()

	return &Config{
		WebhookSecret:    []byte(webhookSecret),
		UserAccessToken:  userAccessToken,
		PipelinesContext: pContext,
	}
}

func main() {
	logrus.SetFormatter(&logrus.JSONFormatter{})
	config := ReadEnv()

	logrus.Println("server started")
	http.HandleFunc("/webhook", config.HandleWebhook)

	logrus.Fatal(http.ListenAndServe(":8080", nil))
}
