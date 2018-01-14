package pods

import (
	"fmt"
	"time"

	"k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/util/runtime"
	"k8s.io/apimachinery/pkg/util/wait"
	"k8s.io/client-go/tools/cache"
	"k8s.io/client-go/util/workqueue"

	kc "github.com/koki/ci-lang/scheduler/kubeclient"
	l "github.com/koki/ci-lang/scheduler/log"
)

type WorkQueue struct {
	Indexer  cache.Indexer
	Queue    workqueue.RateLimitingInterface
	Informer cache.Controller
}

func BuildWorkQueue(c *kc.Context, podWatcher *cache.ListWatch, logger *l.Logger) *WorkQueue {
	queue := workqueue.NewRateLimitingQueue(workqueue.DefaultControllerRateLimiter())

	indexer, informer := cache.NewIndexerInformer(
		podWatcher, &v1.Pod{}, 0, cache.ResourceEventHandlerFuncs{
			AddFunc: func(obj interface{}) {
				key, err := cache.MetaNamespaceKeyFunc(obj)
				if err == nil {
					queue.Add(key)
				} else {
					logger.Err(err, "pod added")
				}
			},
			UpdateFunc: func(old interface{}, new interface{}) {
				key, err := cache.MetaNamespaceKeyFunc(new)
				if err == nil {
					queue.Add(key)
				} else {
					logger.Err(err, "pod updated")
				}
			},
			DeleteFunc: func(obj interface{}) {
				key, err := cache.DeletionHandlingMetaNamespaceKeyFunc(obj)
				if err == nil {
					queue.Add(key)
				} else {
					logger.Err(err, "pod deleted")
				}
			},
		}, cache.Indexers{})

	return &WorkQueue{
		Indexer:  indexer,
		Queue:    queue,
		Informer: informer,
	}
}

type Worker func(key string, pod *v1.Pod) error
type Controller struct {
	WorkQueue *WorkQueue
	Worker    Worker
	Logger    *l.Logger
}

func BuildController(c *kc.Context, podWatcher *cache.ListWatch, worker Worker, logger *l.Logger) *Controller {
	wq := BuildWorkQueue(c, podWatcher, logger)
	return &Controller{
		WorkQueue: wq,
		Worker:    worker,
		Logger:    logger,
	}
}

func (c *Controller) Run(stopCh chan struct{}) {
	wq := c.WorkQueue
	defer runtime.HandleCrash()
	defer wq.Queue.ShutDown() // Stop the workers at the end.

	c.Logger.Info("Starting to process Pods.")
	go wq.Informer.Run(stopCh)

	// Wait for all caches to sync before work begins.
	if !cache.WaitForCacheSync(stopCh, wq.Informer.HasSynced) {
		err := fmt.Errorf("Timed out waiting for caches to sync")
		runtime.HandleError(err)
		c.Logger.PlainErr(err)
		return
	}

	go wait.Until(c.RunWorker, time.Second, stopCh)

	<-stopCh
	c.Logger.Info("Done processing Pods.")
}

func (c *Controller) RunWorker() {
	for c.FeedNextItem() {
	}
}

func (c *Controller) FeedNextItem() bool {
	key, quit := c.WorkQueue.Queue.Get()
	if quit {
		return false
	}

	defer c.WorkQueue.Queue.Done(key)

	err := c.ProcessKey(key.(string))
	c.HandleError(err, key)
	return true
}

func (c *Controller) ProcessKey(key string) error {
	obj, exists, err := c.WorkQueue.Indexer.GetByKey(key)
	if err != nil {
		c.Logger.Err(err, "fetching object %s from store")
		return err
	}

	if !exists {
		c.Worker(key, nil)
	} else {
		c.Worker(key, obj.(*v1.Pod))
	}

	return nil
}

func (c *Controller) HandleError(err error, key interface{}) {
	if err == nil {
		c.WorkQueue.Queue.Forget(key)
		return
	}

	if c.WorkQueue.Queue.NumRequeues(key) < 5 {
		c.Logger.Err(err, "syncing pod %v, will retry", key)
		c.WorkQueue.Queue.AddRateLimited(key)
		return
	}

	c.WorkQueue.Queue.Forget(key)
	runtime.HandleError(err)
	c.Logger.Err(err, "dropping pod %q out of the queue", key)
}
