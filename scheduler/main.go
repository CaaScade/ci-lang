package main

import (
	"net/http"
	"os"

	"github.com/sirupsen/logrus"

	"github.com/koki/ci-lang/scheduler/kubeclient"
	gs "github.com/koki/ci-lang/scheduler/secrets/github"

	"github.com/koki/ci-lang/scheduler/endpoints/github/handlewebhook"
	"github.com/koki/ci-lang/scheduler/endpoints/github/oauth"
	"github.com/koki/ci-lang/scheduler/endpoints/github/webhook"
)

const (
	BaseURLVar = "CI_BASE_URL"
)

// Context contains the global settings for the webhook server.
type Context struct {
	BaseURL string
	Kube    *kubeclient.Context
	OAuth   *oauth.Context
}

func BuildContext() *Context {
	baseURL := os.Getenv(BaseURLVar)
	if len(baseURL) == 0 {
		logrus.Fatalf("missing %s", BaseURLVar)
	}

	kubeContext := kubeclient.ReadEnv().BuildContext()
	oauthContext := oauth.ReadEnv().BuildContext()

	return &Context{
		BaseURL: baseURL,
		Kube:    kubeContext,
		OAuth:   oauthContext,
	}
}

func (c *Context) HandleWebhook(w http.ResponseWriter, r *http.Request) {
	secretProvider := &gs.KubeSecretProvider{
		Kube: c.Kube,
	}
	ctx := handlewebhook.Context{
		Kube:          c.Kube,
		AccessToken:   secretProvider,
		WebhookSecret: secretProvider,
	}
	ctx.HandleWebhook(w, r)
}

func (c *Context) HandleGitHubOAuth(w http.ResponseWriter, r *http.Request) {
	c.OAuth.HandleGitHubOAuth(c.Kube, w, r)
}

func (c *Context) HandleCreateWebhook(w http.ResponseWriter, r *http.Request) {
	ctx := webhook.Context{
		BaseURL: c.BaseURL,
		Store:   c.OAuth.Store,
		Kube:    c.Kube,
	}
	ctx.HandleCreateWebhook(w, r)
}

func main() {
	logrus.SetFormatter(&logrus.JSONFormatter{})
	c := BuildContext()

	mux := http.NewServeMux()
	mux.HandleFunc("/webhook/", c.HandleWebhook)
	mux.HandleFunc("/login", c.OAuth.Login)
	mux.HandleFunc("/oauth/github-callback", c.HandleGitHubOAuth)
	mux.HandleFunc("/create-webhook", c.HandleCreateWebhook)

	logrus.Println("server started")
	logrus.Fatal(http.ListenAndServe(":8080", mux))
}
