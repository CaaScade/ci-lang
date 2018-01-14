package main

import (
	"net/http"
	"os"

	"github.com/google/go-github/github"
	"github.com/koki/json"
	serr "github.com/koki/structurederrors"
	"github.com/sirupsen/logrus"

	_ "k8s.io/client-go/kubernetes" // using this later
)

// WebhookSecretVar is the name of an environment variable.
// GitHub uses the secret value to sign webhook requests.
const WebhookSecretVar = "GITHUB_WEBHOOK_SECRET"

// Config contains the global settings for the webhook server.
type Config struct {
	WebhookSecret []byte
}

// HandleWebhook implements the GitHub Webhook receiver endpoint.
func (c *Config) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	log := StartLog()
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
		HandlePushEvent(event, w, log)
	case *github.PullRequestEvent:
		log["pull_event"] = event
		HandlePullEvent(event, w, log)
	default:
		log["body"] = string(payload)
		log["other_event"] = true
	}
}

// HandlePushEvent triggers a pipeline for a GitHub push event.
func HandlePushEvent(event *github.PushEvent, w http.ResponseWriter, log Log) {
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

// HandlePullEvent triggers a pipeline for a GitHub pull request event.
func HandlePullEvent(event *github.PullRequestEvent, w http.ResponseWriter, log Log) {
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

func main() {
	logrus.SetFormatter(&logrus.JSONFormatter{})
	webhookSecret := os.Getenv(WebhookSecretVar)
	if len(webhookSecret) == 0 {
		logrus.Fatalf("%s is missing", WebhookSecretVar)
	}

	config := &Config{
		WebhookSecret: []byte(webhookSecret),
	}

	logrus.Println("server started")
	http.HandleFunc("/webhook", config.HandleWebhook)

	logrus.Fatal(http.ListenAndServe(":8080", nil))
}

// Log is an unstructured log object used to generate JSON-formatted logs.
// Information is added to the Log over the course of its lifetime until
// its Write method is called (usually by a "defer" block).
type Log map[string]interface{}

// StartLog starts a new Log entry.
func StartLog() Log {
	return Log(map[string]interface{}{})
}

// Write actually commits the log entry (usually in a "defer" block).
func (l *Log) Write() {
	logrus.WithField("log", l).Info()
}
