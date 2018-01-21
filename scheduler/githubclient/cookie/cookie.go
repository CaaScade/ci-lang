package cookie

import (
	"encoding/gob"
	"fmt"
	"net/http"

	"github.com/gorilla/sessions"
	l "github.com/koki/ci-lang/scheduler/log"
)

const (
	CookieName = "user"
	CookieKey  = "github"
)

type GitHubUserSession struct {
	Login       string
	ID          int
	AccessToken string
}

func init() {
	gob.Register(&GitHubUserSession{})
}

type Context struct {
	Store  *sessions.CookieStore
	Logger *l.Logger
}

func BuildContext(store *sessions.CookieStore, logger *l.Logger) *Context {
	return &Context{
		Store:  store,
		Logger: logger,
	}
}

func (c *Context) GetSession(r *http.Request) *GitHubUserSession {
	cookie, err := c.Store.Get(r, CookieName)
	if err != nil {
		c.Logger.Err(err, "invalid %s cookie", CookieName)
		return nil
	}

	if session, ok := cookie.Values[CookieKey].(*GitHubUserSession); ok {
		c.Logger.Info("%s:%d", session.Login, session.ID)
		fmt.Println(session.AccessToken)
		return session
	}

	return nil
}

func (c *Context) SetSession(w http.ResponseWriter, r *http.Request, session *GitHubUserSession) {
	c.Logger.Info("%s:%d", session.Login, session.ID)
	cookie, err := c.Store.New(r, CookieName)
	if err != nil {
		c.Logger.Err(err, "creating new %s cookie", CookieName)
		return
	}

	cookie.Values[CookieKey] = session
	err = cookie.Save(r, w)
	if err != nil {
		c.Logger.Err(err, "saving %s cookie", CookieName)
	}
}
