package oauth

import (
	"context"
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"net/http"
	"os"

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
	"golang.org/x/oauth2"
)

const (
	CookieName       = "oauth_state"
	ClientSecretVar  = "GITHUB_CLIENT_SECRET"
	ClientIDVar      = "GITHUB_CLIENT_ID"
	CookieAuthKeyVar = "COOKIE_AUTH_KEY"
)

type Context struct {
	Store    *sessions.CookieStore
	OAuthCfg *oauth2.Config
}

type Env struct {
	ClientID      string
	ClientSecret  string
	CookieAuthKey string
}

func ReadEnv() Env {
	id := os.Getenv(ClientIDVar)
	if len(id) == 0 {
		l.Default.Fatalf("%s is missing", ClientIDVar)
	}

	secret := os.Getenv(ClientSecretVar)
	if len(secret) == 0 {
		l.Default.Fatalf("%s is missing", ClientSecretVar)
	}

	cookieAuthKey := os.Getenv(CookieAuthKeyVar)
	if len(cookieAuthKey) == 0 {
		l.Default.Fatalf("%s is missing", CookieAuthKeyVar)
	}

	return Env{
		ClientID:      id,
		ClientSecret:  secret,
		CookieAuthKey: cookieAuthKey,
	}
}

func (e Env) BuildContext() *Context {
	oauthCfg := &oauth2.Config{
		ClientID:     e.ClientID,
		ClientSecret: e.ClientSecret,
		Endpoint: oauth2.Endpoint{
			AuthURL:  "https://github.com/login/oauth/authorize",
			TokenURL: "https://github.com/login/oauth/access_token",
		},
		RedirectURL: "",
		Scopes:      []string{"repo"},
	}

	return &Context{
		OAuthCfg: oauthCfg,
		Store:    sessions.NewCookieStore([]byte(e.CookieAuthKey)),
	}
}

func (c *Context) Login(rw http.ResponseWriter, r *http.Request) {
	requestID := l.GenerateContextID()
	logger := &l.Logger{Context: map[string]interface{}{
		"request_id": requestID,
	}}
	log := logger.StartLog()
	defer log.Write()

	session, err := c.Store.Get(r, CookieName)
	if err != nil {
		log["invalid_cookie"] = serr.ContextualizeErrorf(err, "need a new cookie")
	}

	b := make([]byte, 16)
	_, err = rand.Read(b)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "couldn't generate randomized state")
		return
	}

	state := base64.URLEncoding.EncodeToString(b)
	log["state"] = state
	session.Values["state"] = state
	_ = session.Save(r, rw) // We don't care about handling this error.

	url := c.OAuthCfg.AuthCodeURL(state)
	http.Redirect(rw, r, url, 302)
}

func (c *Context) HandleGitHubOAuth(kubeContext *kubeclient.Context, w http.ResponseWriter, r *http.Request) {
	requestID := l.GenerateContextID()
	logger := &l.Logger{Context: map[string]interface{}{
		"request_id": requestID,
	}}

	token, err := c.OAuthCallback(w, r, logger)
	if err != nil {
		// OAuthCallback handles its own errors.
		return
	}

	//
	// Use the token to fetch User info & save to the cookie.
	//

	client := github.NewClient(c.OAuthCfg.Client(context.Background(), token))
	user, _, err := client.Users.Get(r.Context(), "")
	if err != nil {
		logger.Err(err, "fetching user from GitHub")
		http.Error(w, requestID, http.StatusInternalServerError)
		return
	}

	if gc.IsValidUser(user) {
		// Save the session to the cookie.
		cookieCtx := cookie.Context{
			Store:  c.Store,
			Logger: logger,
		}
		cookieCtx.SetSession(w, r, &cookie.GitHubUserSession{
			Login:       util.FromStringPtr(user.Login),
			ID:          util.FromIntPtr(user.ID),
			AccessToken: token.AccessToken,
		})

		// Write the HTTP response.
		fmt.Fprint(w, closingPage)

		// Save the OAuth token in a Secret.
		namespace := kubeContext.Env.Namespace
		secret := gs.BuildOAuthSecret(namespace, user, token)
		_ = secrets.CreateOrUpdateSecret(
			namespace,
			kubeContext.Clients,
			secret,
			logger,
		)

	} else {
		logger.PlainErr(fmt.Errorf("invalid github user"))
		http.Error(w, "invalid github user", http.StatusBadRequest)
	}
}

const closingPage = `
<html>
  <head>
    <script type="text/javascript">
      setTimeout(function() {
        parent.close();
        window.location = "https://docs.koki.io/short/";
      }, 3000)
    </script>
  </head>
  <body>
    OAuth successful!
  </body>
</html>
`

// OAuthCallback is just the OAuth portion of the callback endpoint.
func (c *Context) OAuthCallback(rw http.ResponseWriter, r *http.Request, logger *l.Logger) (*oauth2.Token, error) {
	log := logger.StartLog()
	defer log.Write()

	//
	// Validate the request by checking against the random value in the session.
	//

	session, err := c.Store.Get(r, CookieName)
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "invalid cookie")
		http.Error(rw, "invalid cookie", http.StatusInternalServerError)
		return nil, err
	}

	queryState := r.URL.Query().Get("state")
	log["state"] = queryState
	sessionState := session.Values["state"]
	if queryState != sessionState {
		log["session_state"] = sessionState
		log["error"] = "session state doesn't match query state"
		http.Error(rw, "mismatched state between login and oauth callback", http.StatusUnauthorized)
		return nil, err
	}

	//
	// Extract the OAuth token.
	//

	token, err := c.OAuthCfg.Exchange(r.Context(), r.URL.Query().Get("code"))
	if err != nil {
		log["error"] = serr.ContextualizeErrorf(err, "extracting oauth token")
		http.Error(rw, "", http.StatusUnauthorized)
		return nil, err
	}

	if !token.Valid() {
		log["error"] = "invalid token"
		http.Error(rw, "invalid token", http.StatusUnauthorized)
		return nil, err
	}

	delete(session.Values, "state")
	_ = session.Save(r, rw) // Not a fatal error.

	return token, nil
}
