Web server accepts webhook.
Kubernetes controller loop watches Pods.
Pipeline scheduler runs and watches Pipelines.

Web server triggers Pipeline scheduler to create a new pipeline.
Pipeline scheduler subscribes to updates on the pipeline Pod.
* It registers a subscription channel with the controller.
Scheduler responds when Pod succeeds or fails.

Extra:
* Scheduler connects to Pod directly to track its progress/health.


