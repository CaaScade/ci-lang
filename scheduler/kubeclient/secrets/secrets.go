package secrets

import (
	"k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"

	"github.com/koki/ci-lang/scheduler/kubeclient"
	l "github.com/koki/ci-lang/scheduler/log"
	serr "github.com/koki/structurederrors"
)

func CreateOrUpdateSecret(namespace string, client *kubernetes.Clientset, secret *v1.Secret, logger *l.Logger) *v1.Secret {
	newSecret, err := client.CoreV1().Secrets(namespace).Create(secret)
	if errors.IsAlreadyExists(err) {
		newSecret, err := client.CoreV1().Secrets(namespace).Update(secret)
		if err != nil {
			logger.Err(err, "updating secret")
			return nil
		}

		return newSecret
	}

	if err != nil {
		logger.Err(err, "creating secret")
		return nil
	}

	return newSecret
}

func GetSecret(c *kubeclient.Context, name string) (*v1.Secret, error) {
	namespace := c.Env.Namespace
	secret, err := c.Clients.CoreV1().Secrets(namespace).Get(name, metav1.GetOptions{})
	if err != nil {
		return nil, serr.ContextualizeErrorf(err, "getting secret %s:%s", namespace, name)
	}

	return secret, nil
}
