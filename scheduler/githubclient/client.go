package githubclient

import (
	"context"

	"github.com/google/go-github/github"
	"golang.org/x/oauth2"
)

func ClientWithToken(userAccessToken string) *github.Client {
	ctx := context.Background()
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: userAccessToken},
	)
	tc := oauth2.NewClient(ctx, ts)
	return github.NewClient(tc)
}

func IsValidUser(user *github.User) bool {
	if user == nil {
		return false
	}

	return user.ID != nil && user.Login != nil
}
