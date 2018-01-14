package main

import (
	"net/http"
	"os"

	"github.com/google/go-github/github"
	"github.com/koki/json"
	serr "github.com/koki/structurederrors"
	"github.com/sirupsen/logrus"

	l "github.com/koki/ci-lang/scheduler/log"
	p "github.com/koki/ci-lang/scheduler/pipelines"
)

// WebhookSecretVar is the name of an environment variable.
// GitHub uses the secret value to sign webhook requests.
const WebhookSecretVar = "GITHUB_WEBHOOK_SECRET"

// Config contains the global settings for the webhook server.
type Config struct {
	WebhookSecret    []byte
	PipelinesContext *p.Context
}

// HandleWebhook implements the GitHub Webhook receiver endpoint.
func (c *Config) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	log := l.StartLog()
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
		c.HandlePushEvent(event, w, log)
	case *github.PullRequestEvent:
		log["pull_event"] = event
		HandlePullEvent(event, w, log)
	default:
		log["body"] = string(payload)
		log["other_event"] = true
	}
}

// HandlePushEvent triggers a pipeline for a GitHub push event.
func (c *Config) HandlePushEvent(event *github.PushEvent, w http.ResponseWriter, log l.Log) {
	pod := c.PipelinesContext.BuildPipelinePod()
	success := c.PipelinesContext.RunPipeline(&pod, &l.Default)

	if success {
		header := w.Header()
		header.Set("Content-Type", "application/json")
		_, err := w.Write([]byte(`{"result": "success!"}`))
		if err != nil {
			log["error"] = serr.ContextualizeErrorf(err, "writing response")
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	} else {
		http.Error(w, "pipeline failed", http.StatusBadRequest)
	}
}

// HandlePullEvent triggers a pipeline for a GitHub pull request event.
func HandlePullEvent(event *github.PullRequestEvent, w http.ResponseWriter, log l.Log) {
	bb, err := json.Marshal(map[string]interface{}{"result": "we did it!"})
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "serializing result of handling push event")
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	header := w.Header()
	header.Set("Content-Type", "application/json")
	_, err = w.Write(bb)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "writing response")
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
}

func ReadEnv() *Config {
	webhookSecret := os.Getenv(WebhookSecretVar)
	if len(webhookSecret) == 0 {
		logrus.Fatalf("%s is missing", WebhookSecretVar)
	}

	pContext := p.ReadEnv().BuildContext()

	return &Config{
		WebhookSecret:    []byte(webhookSecret),
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
