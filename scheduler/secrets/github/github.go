package github

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"strconv"
	"strings"

	"k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	"github.com/google/go-github/github"
	"golang.org/x/oauth2"

	"github.com/koki/ci-lang/scheduler/kubeclient"
	"github.com/koki/ci-lang/scheduler/kubeclient/secrets"
	"github.com/koki/ci-lang/scheduler/util"
	serr "github.com/koki/structurederrors"
)

type AccessTokenProvider interface {
	AccessTokenForUser(login string, id int) (string, error)
}

type WebhookSecret struct {
	Login  string
	ID     int
	Secret string
}

type WebhookSecretProvider interface {
	SecretForWebhookID(webhookID string) (*WebhookSecret, error)
}

type KubeSecretProvider struct {
	Kube *kubeclient.Context
}

func (k *KubeSecretProvider) SecretForWebhookID(webhookID string) (*WebhookSecret, error) {
	secretName := BuildWebhookSecretName(webhookID)
	secret, err := secrets.GetSecret(k.Kube, secretName)
	if err != nil {
		return nil, err
	}

	id, err := strconv.ParseInt(string(secret.Data["id"]), 10, 64)
	if err != nil {
		return nil, serr.ContextualizeErrorf(err, "non-integer GitHub ID for %s", secretName)
	}

	return &WebhookSecret{
		Login:  string(secret.Data["login"]),
		ID:     int(id),
		Secret: string(secret.Data["webhookSecret"]),
	}, nil
}

func (k *KubeSecretProvider) AccessTokenForUser(login string, id int) (string, error) {
	secretName := BuildOAuthSecretName(login, id)
	secret, err := secrets.GetSecret(k.Kube, secretName)
	if err != nil {
		return "", err
	}

	return string(secret.Data["accessToken"]), nil
}

func BuildOAuthSecret(namespace string, user *github.User, token *oauth2.Token) *v1.Secret {
	name := BuildOAuthSecretName(
		util.FromStringPtr(user.Login),
		util.FromIntPtr(user.ID))
	return &v1.Secret{
		ObjectMeta: metav1.ObjectMeta{
			Name:      name,
			Namespace: namespace,
		},
		StringData: map[string]string{
			"accessToken": token.AccessToken,
		},
	}
}

func BuildOAuthSecretName(login string, id int) string {
	return fmt.Sprintf("ci.koki.github.oauth-token.%s.%d", strings.ToLower(login), id)
}

func GenerateWebhookSecret(namespace, webhookID, login string, id int) (*v1.Secret, string, error) {
	name := BuildWebhookSecretName(webhookID)
	key, err := GenerateRandomBase64(24)
	if err != nil {
		return nil, "", serr.ContextualizeErrorf(err, "generating webhook secret")
	}

	return &v1.Secret{
		ObjectMeta: metav1.ObjectMeta{
			Name:      name,
			Namespace: namespace,
		},
		StringData: map[string]string{
			"login":         login,
			"id":            fmt.Sprintf("%d", id),
			"webhookSecret": key,
		},
	}, key, nil
}

func BuildWebhookSecretName(webhookID string) string {
	return fmt.Sprintf("ci.koki.github.webhook-secret.%s", webhookID)
}

func GenerateRandomBase64(length int) (string, error) {
	b := make([]byte, length)
	_, err := rand.Read(b)
	if err != nil {
		return "", serr.ContextualizeErrorf(err, "couldn't generate random string")
	}

	return base64.StdEncoding.EncodeToString(b), nil
}
