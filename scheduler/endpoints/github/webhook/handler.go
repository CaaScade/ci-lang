package webhook

import (
	"context"
	"fmt"
	"net/http"

	"github.com/google/go-github/github"
	"github.com/gorilla/sessions"
	gc "github.com/koki/ci-lang/scheduler/githubclient"
	"github.com/koki/ci-lang/scheduler/githubclient/cookie"
	"github.com/koki/ci-lang/scheduler/kubeclient"
	"github.com/koki/ci-lang/scheduler/kubeclient/secrets"
	l "github.com/koki/ci-lang/scheduler/log"
	gs "github.com/koki/ci-lang/scheduler/secrets/github"
	"github.com/koki/ci-lang/scheduler/util"
	serr "github.com/koki/structurederrors"
)

/*
This package is for the endpoint that creates a webhook
*/

type Context struct {
	BaseURL string
	Store   *sessions.CookieStore
	Kube    *kubeclient.Context
}

func (c *Context) BuildWebhookURL(webhookID string) string {
	return fmt.Sprintf("%s/webhook/%s", c.BaseURL, webhookID)
}

func (c *Context) HandleCreateWebhook(w http.ResponseWriter, r *http.Request) {
	requestID := l.GenerateContextID()
	logger := &l.Logger{Context: map[string]interface{}{
		"request_id": requestID,
	}}
	log := logger.StartLog()
	defer log.Write()

	cookieContext := cookie.Context{
		Store:  c.Store,
		Logger: logger,
	}
	session := cookieContext.GetSession(r)
	if session == nil {
		log["error"] = "please sign in"
		http.Error(w, "please sign in", http.StatusUnauthorized)
		return
	}
	log["session"] = fmt.Sprintf("%s:%d", session.Login, session.ID)

	webhookID := l.GenerateContextID()
	namespace := c.Kube.Env.Namespace
	secret, webhookSecret, err := gs.GenerateWebhookSecret(
		namespace,
		webhookID,
		session.Login,
		session.ID)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "generating webhook secret")
		http.Error(w, requestID, http.StatusInternalServerError)
		return
	}
	log["webhook_id"] = webhookID

	createdSecret := secrets.CreateOrUpdateSecret(namespace, c.Kube.Clients, secret, logger)
	if createdSecret == nil {
		log["error"] = "couldn't create webhook secret"
		http.Error(w, "couldn't create webhook secret", http.StatusInternalServerError)
		return
	}

	owner := r.URL.Query().Get("owner")
	repo := r.URL.Query().Get("repo")
	hook := &github.Hook{
		Name:   util.StringPtr("web"),
		Events: []string{"pull_request"},
		Config: map[string]interface{}{
			"url":    c.BuildWebhookURL(webhookID),
			"secret": webhookSecret,
		},
	}

	log["owner"] = owner
	log["repo"] = repo

	client := gc.ClientWithToken(session.AccessToken)
	_, _, err = client.Repositories.CreateHook(context.Background(), owner, repo, hook)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "creating webhook")
		http.Error(w, requestID, http.StatusInternalServerError)
		return
	}

	fmt.Fprintf(w, "created webhook!")
	log["result"] = "created webhook!"
}
