package handlewebhook

import (
	"net/http"
	"regexp"

	"github.com/google/go-github/github"
	serr "github.com/koki/structurederrors"

	gc "github.com/koki/ci-lang/scheduler/githubclient"
	"github.com/koki/ci-lang/scheduler/githubclient/repostatus"
	"github.com/koki/ci-lang/scheduler/kubeclient"
	"github.com/koki/ci-lang/scheduler/kubeclient/pods"
	l "github.com/koki/ci-lang/scheduler/log"
	ghpipelines "github.com/koki/ci-lang/scheduler/pipelines/github"
	secrets "github.com/koki/ci-lang/scheduler/secrets/github"
	"github.com/koki/ci-lang/scheduler/util"
)

type Context struct {
	Kube          *kubeclient.Context
	AccessToken   secrets.AccessTokenProvider
	WebhookSecret secrets.WebhookSecretProvider
}

var WebhookIDRegexp = regexp.MustCompile(`^/webhook/([^/]+)$`)

func GetRequestWebhookID(r *http.Request) string {
	matches := WebhookIDRegexp.FindStringSubmatch(r.URL.Path)
	if len(matches) == 0 {
		return ""
	}

	return matches[1]
}

// HandleWebhook implements the GitHub Webhook receiver endpoint.
func (c *Context) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	requestID := l.GenerateContextID()
	logger := &l.Logger{Context: map[string]interface{}{
		"request_id": requestID,
	}}
	log := logger.StartLog()
	defer log.Write()

	log["request_url"] = r.URL.String()
	log["request_path"] = r.URL.Path
	log["request_header"] = r.Header

	webhookID := GetRequestWebhookID(r)
	if len(webhookID) == 0 {
		log["error"] = "couldn't identify webhook ID from URL"
		http.Error(w, requestID, http.StatusBadRequest)
		return
	}

	log["webhook_id"] = webhookID
	webhookSecret, err := c.WebhookSecret.SecretForWebhookID(webhookID)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "fetching webhook secret")
		http.Error(w, requestID, http.StatusBadRequest)
		return
	}

	payload, err := github.ValidatePayload(r, []byte(webhookSecret.Secret))
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "validating webhook request")
		http.Error(w, requestID, http.StatusBadRequest)
		return
	}
	defer r.Body.Close()

	event, err := github.ParseWebHook(github.WebHookType(r), payload)
	if err != nil {
		log["request_body"] = string(payload)
		log["error"] = serr.ContextualizeErrorf(err, "parsing webhook request")
		http.Error(w, requestID, http.StatusBadRequest)
		return
	}

	switch event := event.(type) {
	case *github.PushEvent:
		log["push_event"] = event
	case *github.PullRequestEvent:
		log["pull_event"] = event

		// Handle the event asynchronously.
		go c.HandlePullEvent(webhookSecret, event, requestID, logger)
	default:
		log["body"] = string(payload)
		log["other_event"] = true
	}
}

// HandlePullEvent triggers a pipeline for a GitHub pull request event.
func (c *Context) HandlePullEvent(webhookSecret *secrets.WebhookSecret, event *github.PullRequestEvent, requestID string, logger *l.Logger) {
	repo := event.GetRepo()
	sha := util.FromStringPtr(event.GetPullRequest().Head.SHA)
	accessToken, err := c.AccessToken.AccessTokenForUser(webhookSecret.Login, webhookSecret.ID)
	if err != nil {
		logger.Err(err, "fetching access token")
		return
	}
	client := gc.ClientWithToken(accessToken)
	repostatus.SetGitHubStatus(client, repo, sha, "pending", logger)

	repoName := util.FromStringPtr(repo.Name)
	repoOwner := util.FromStringPtr(repo.Owner.Login)
	oauthSecretName := secrets.BuildOAuthSecretName(webhookSecret.Login, webhookSecret.ID)
	pod := ghpipelines.BuildPipelinePod(c.Kube.Env.Namespace, requestID, oauthSecretName, repoOwner, repoName, sha)
	success := pods.RunPod(c.Kube, &pod, logger)

	if success {
		repostatus.SetGitHubStatus(client, repo, sha, "success", logger)
	} else {
		repostatus.SetGitHubStatus(client, repo, sha, "failure", logger)
	}
}
