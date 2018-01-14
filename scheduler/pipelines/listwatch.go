package pipelines

import (
	"fmt"

	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/apimachinery/pkg/watch"
	"k8s.io/client-go/tools/cache"

	uu "github.com/satori/go.uuid"
)

func PipelineControllerLabelSelector(pipelineID uu.UUID) string {
	return fmt.Sprintf("ci-component=pipeline-controller, ci-pipeline-id=%s", pipelineID.String())
}

func (c *Context) PipelinesWatch(labelSelector string) *cache.ListWatch {
	listFunc := func(options metav1.ListOptions) (runtime.Object, error) {
		options.LabelSelector = labelSelector
		return c.Clients.CoreV1().Pods(c.Env.Namespace).List(options)
	}
	watchFunc := func(options metav1.ListOptions) (watch.Interface, error) {
		options.Watch = true
		options.LabelSelector = labelSelector
		return c.Clients.CoreV1().Pods(c.Env.Namespace).Watch(options)
	}
	return &cache.ListWatch{ListFunc: listFunc, WatchFunc: watchFunc}
}

func (c *Context) PipelineWatch(name string) *cache.ListWatch {
	selector := "metadata.name=" + name
	listFunc := func(options metav1.ListOptions) (runtime.Object, error) {
		options.FieldSelector = selector
		return c.Clients.CoreV1().Pods(c.Env.Namespace).List(options)
	}
	watchFunc := func(options metav1.ListOptions) (watch.Interface, error) {
		options.Watch = true
		options.FieldSelector = selector
		return c.Clients.CoreV1().Pods(c.Env.Namespace).Watch(options)
	}
	return &cache.ListWatch{ListFunc: listFunc, WatchFunc: watchFunc}
}
