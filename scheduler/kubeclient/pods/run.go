package pods

import (
	"k8s.io/api/core/v1"

	kc "github.com/koki/ci-lang/scheduler/kubeclient"
	l "github.com/koki/ci-lang/scheduler/log"
)

func RunPod(c *kc.Context, pod *v1.Pod, logger *l.Logger) bool {
	podName := pod.Name
	_, err := c.Pods().Create(pod)
	if err != nil {
		logger.Err(err, "creating pipeline pod")
		return false
	}

	podSuccessCh := make(chan bool)
	podWatcher := Watch(c, podName)
	controller := BuildController(c, podWatcher, func(key string, pod *v1.Pod) error {
		// Check if the ci-lang container finished.
		if len(pod.Status.ContainerStatuses) > 0 {
			containerStatus := pod.Status.ContainerStatuses[0]
			state := containerStatus.State
			if state.Terminated != nil {
				if state.Terminated.ExitCode == 0 {
					podSuccessCh <- true
				} else {
					podSuccessCh <- false
				}
			}
		}

		return nil
	}, logger)

	stopCh := make(chan struct{})
	go controller.Run(stopCh)

	podSuccess := <-podSuccessCh
	close(stopCh)

	if podSuccess {
		err := c.Pods().Delete(podName, nil)
		if err != nil {
			logger.Err(err, "deleting successful pod")
		}
	}

	return podSuccess
}
