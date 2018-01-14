package pipelines

import (
	"k8s.io/api/core/v1"
)

// WorkerGen is a generator of workers.
type WorkerGen func() Worker

type Worker func(key string, pod *v1.Pod) error
