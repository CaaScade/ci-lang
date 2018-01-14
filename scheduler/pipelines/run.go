package pipelines

import (
	"fmt"

	"k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"

	l "github.com/koki/ci-lang/scheduler/log"
)

const PipelineJobLabelKey = "ci-pipeline-component"
const PipelineJobLabelValue = "pipeline-controller"

func (c *Context) RunPipeline(pod *v1.Pod, logger *l.Logger) bool {
	podName := pod.Name
	_, err := c.Pods().Create(pod)
	if err != nil {
		logger.Err(err, "creating pipeline pod")
		return false
	}

	podSuccessCh := make(chan bool)
	podWatcher := c.PipelineWatch(podName)
	controller := c.BuildController(podWatcher, func(key string, pod *v1.Pod) error {
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

func (c *Context) BuildPipelinePod(pipelineID string) v1.Pod {
	name := fmt.Sprintf("pipeline-%s", pipelineID)
	return PipelineControllerPod(c.Env.Namespace, name)
}

func PipelineControllerContainer() v1.Container {
	return v1.Container{
		Command: []string{"ci-lang-exe"},
		Env: []v1.EnvVar{
			v1.EnvVar{
				Name:  "DOCKER_HOST",
				Value: "http://localhost:2375",
			},
			v1.EnvVar{
				Name:  "CI_WORKSPACE_DIR",
				Value: "/workspace",
			},
		},
		Image:           "ublubu/stackapp",
		ImagePullPolicy: v1.PullAlways,
		Name:            "ci-lang",
		VolumeMounts: []v1.VolumeMount{
			v1.VolumeMount{
				MountPath: "/workspace",
				Name:      "workspace",
			},
		},
		WorkingDir: "/opt/app",
	}
}

func DockerInDockerContainer() v1.Container {
	privileged := true
	return v1.Container{
		Image:           "docker:dind",
		Name:            "dind",
		SecurityContext: &v1.SecurityContext{Privileged: &privileged},
		VolumeMounts: []v1.VolumeMount{
			v1.VolumeMount{
				MountPath: "/var/lib/docker",
				Name:      "docker-graph-storage",
			},
		},
	}
}

func PipelineControllerPod(namespace, name string) v1.Pod {
	pipelineController := PipelineControllerContainer()
	dind := DockerInDockerContainer()
	gracePeriod := int64(10)
	spec := v1.PodSpec{
		Containers:                    []v1.Container{pipelineController, dind},
		RestartPolicy:                 v1.RestartPolicyNever,
		TerminationGracePeriodSeconds: &gracePeriod,
		Volumes: []v1.Volume{
			v1.Volume{
				Name: "docker-graph-storage",
				VolumeSource: v1.VolumeSource{
					EmptyDir: &v1.EmptyDirVolumeSource{},
				},
			},
			v1.Volume{
				Name: "workspace",
				VolumeSource: v1.VolumeSource{
					EmptyDir: &v1.EmptyDirVolumeSource{},
				},
			},
		},
	}

	return v1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      name,
			Namespace: namespace,
			Labels: map[string]string{
				PipelineJobLabelKey: PipelineJobLabelValue,
			},
		},
		Spec: spec,
	}
}
