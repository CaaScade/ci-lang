package pipelines

import (
	"k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

const PipelineJobLabelKey = "ci-pipeline-component"
const PipelineJobLabelValue = "pipeline-controller"

func PipelineControllerContainer(extraEnv []v1.EnvVar) v1.Container {
	env := []v1.EnvVar{
		v1.EnvVar{
			Name:  "DOCKER_HOST",
			Value: "http://localhost:2375",
		},
		v1.EnvVar{
			Name:  "CI_WORKSPACE_DIR",
			Value: "/workspace",
		},
	}
	if len(extraEnv) > 0 {
		env = append(env, extraEnv...)
	}
	return v1.Container{
		Command:         []string{"ci-lang-exe"},
		Env:             env,
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
			v1.VolumeMount{
				MountPath: "/workspace",
				Name:      "workspace",
			},
		},
	}
}

func PipelineControllerPod(namespace, name string, extraEnv []v1.EnvVar) v1.Pod {
	pipelineController := PipelineControllerContainer(extraEnv)
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
