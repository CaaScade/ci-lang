package github

import (
	"fmt"

	"k8s.io/api/core/v1"

	"github.com/koki/ci-lang/scheduler/pipelines"
)

func BuildPipelinePod(namespace, pipelineID, oauthSecretName, repoOwner, repoName, sha string) v1.Pod {
	name := fmt.Sprintf("github-pipeline-%s", pipelineID)
	gitEnv := []v1.EnvVar{
		v1.EnvVar{
			Name:  "GIT_REPO_OWNER",
			Value: repoOwner,
		},
		v1.EnvVar{
			Name:  "GIT_REPO_NAME",
			Value: repoName,
		},
		v1.EnvVar{
			Name:  "GIT_REVISION",
			Value: sha,
		},
		v1.EnvVar{
			Name: "GITHUB_OAUTH_TOKEN",
			ValueFrom: &v1.EnvVarSource{
				SecretKeyRef: &v1.SecretKeySelector{
					LocalObjectReference: v1.LocalObjectReference{
						Name: oauthSecretName,
					},
					Key: "accessToken",
				},
			},
		},
	}
	return pipelines.PipelineControllerPod(namespace, name, gitEnv)
}
