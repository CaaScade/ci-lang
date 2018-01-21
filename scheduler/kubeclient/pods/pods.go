package pods

import (
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/apimachinery/pkg/watch"
	"k8s.io/client-go/tools/cache"

	kc "github.com/koki/ci-lang/scheduler/kubeclient"
)

func ListWatch(c *kc.Context, labelSelector string) *cache.ListWatch {
	listFunc := func(options metav1.ListOptions) (runtime.Object, error) {
		options.LabelSelector = labelSelector
		return c.Pods().List(options)
	}
	watchFunc := func(options metav1.ListOptions) (watch.Interface, error) {
		options.Watch = true
		options.LabelSelector = labelSelector
		return c.Pods().Watch(options)
	}
	return &cache.ListWatch{ListFunc: listFunc, WatchFunc: watchFunc}
}

func Watch(c *kc.Context, name string) *cache.ListWatch {
	selector := "metadata.name=" + name
	listFunc := func(options metav1.ListOptions) (runtime.Object, error) {
		options.FieldSelector = selector
		return c.Pods().List(options)
	}
	watchFunc := func(options metav1.ListOptions) (watch.Interface, error) {
		options.Watch = true
		options.FieldSelector = selector
		return c.Pods().Watch(options)
	}
	return &cache.ListWatch{ListFunc: listFunc, WatchFunc: watchFunc}
}
