package repostatus

import (
	"context"

	"github.com/google/go-github/github"
	serr "github.com/koki/structurederrors"

	l "github.com/koki/ci-lang/scheduler/log"
	"github.com/koki/ci-lang/scheduler/util"
)

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
