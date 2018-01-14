package pipelines

import (
	"os"

	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/kubernetes/typed/core/v1"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"

	l "github.com/koki/ci-lang/scheduler/log"
)

type Context struct {
	Env          Env
	Clients      *kubernetes.Clientset
	ClientConfig *rest.Config
}

type Env struct {
	MasterURL  string
	KubeConfig string
	Namespace  string
}

// ReadEnv gets the environment variables for the k8s client.
func ReadEnv() Env {
	return Env{
		MasterURL:  os.Getenv("K8S_MASTER_URL"),
		KubeConfig: os.Getenv("KUBECONFIG"),
		Namespace:  os.Getenv("NAMESPACE"),
	}
}

func (e Env) BuildContext() *Context {
	cfg, err := clientcmd.BuildConfigFromFlags(e.MasterURL, e.KubeConfig)
	if err != nil {
		l.Default.FatalErr(err, "building k8s config")
	}

	clientset, err := kubernetes.NewForConfig(cfg)
	if err != nil {
		l.Default.FatalErr(err, "creating k8s client")
	}

	return &Context{
		Env:          e,
		Clients:      clientset,
		ClientConfig: cfg,
	}
}

func (c *Context) Pods() v1.PodInterface {
	return c.Clients.CoreV1().Pods(c.Env.Namespace)
}
