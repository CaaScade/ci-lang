# How to support multiple source-control providers

A bundle of endpoints per provider. e.g. bundled GitHub endpoints.
Logically structured as separate servers, but they can share resources.

### What if multiple source-control providers are used for a single pipeline?

# Architect for maintainability / refactorability rather than functional flexibility.

Combine atomic units of functionality.
Provide tools that make combination an easy process.

## GitHub integration

"Services"
* OAuth sign-in
  * /login endpoint
  * /callback endpoint
* Cookie-based read/write of OAuth token & user info
* Write OAuth token to Secret
* Build Pod spec for pipeline job
* Run Pod for pipeline job
* Register webhook
* Write revision status
* Handle webhook request

GitHub client: per-request
* user requests
* webhook requests

Kubernetes client: per-server
* create secrets
  * GitHub OAuth2 tokens
* run Pods


## Package Structure

* main
* githubclient
  * pullevent - parse pull events
  * repostatus - set repo status
  * webhook - set repo webhook
  * oauth - perform oauth and return results
  * cookie - get and set session in cookie
* kubeclient
  * secret - creating secrets
  * pod - running pod jobs
* secrets
  * github - create spec for github oauth secret
* pipelines
  * github - create spec for github pipeline job
* endpoints
  * github
    * oauth - oauth + storing info in cookie
    * webhook - get session info, create webhook
    * handlewebhook - create spec for github pipeline job, run it, set repo status
