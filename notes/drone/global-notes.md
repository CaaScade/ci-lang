# Drone

Plain Drone step definition:
* Identifier for a Docker Image
* Environment variables
* Commands to execute in the container


Plugin Drone step definition:
* Identifier for plugin
* plugin-defined arguments (string and []string observed, probably any YAML)
outputs: (presumably) writes files, makes network requests
^ example: build Docker image and publish to image repo


Meta-definition for Drone steps:
* Filter whether the step is executed

Drone step black-box type:
type Step = (Filesystem, [Service]) -> (ExitCode, Filesystem, [Service], [ExternalRequest])
In words: The step needs a Filesystem and some set of Services and produces
an ExitCode, maybe some ExternalRequests, and possibly modified state of
the Filesystem and Services.

`when` clause behaves like this:
when condition step = if condition then step else noop

where noop is the no-op step

## Argument Types (for generating Steps)

Drone pipeline is a sequence of steps (:: [Step]).
The YAML template for each Step is something like this:
(Some Arguments) -> Step

Typing these as String doesn't completely hold up because of the built-in
string interpolations.

### for any pipeline step
group :: Group -- For same-machine parallelizing steps

### for plain images
image: String -- Docker image name
environment :: [(String, String)]
commands :: [String]
volumes :: [Volume] -- mounting a host volume
^ This is another avenue for I/O in the plugin step, which affects the
  `Step` type.

### plugin-specific
files :: [String] -- file globs
repo :: String -- Docker hub repo
secrets :: [SecretRef] -- names of secrets stored in Drone
dockerfile :: String -- single file
tag :: [String] -- Docker image tags to publish
from :: String -- email address
recipients :: [String] -- email addresses
depth :: Int -- git clone depth
tags :: Bool -- should git clone tags
recursive :: Bool -- should git clone submodules

## Global Vars (for `when` and interpolations)
matrix :: Dictionary
branch :: String -- version control branch
repo :: String -- source repo
event :: String -- triggering event type
status :: Success/Failure

`when` uses includes/excludes and lists of values for each

## Examples

* https://github.com/drone/drone/blob/master/.drone.yml
* https://gist.github.com/ndelitski/a314e090d6615f10884d
* https://github.com/scrapinghub/portia/blob/master/.drone.yml
* https://github.com/go-gitea/gitea/blob/master/.drone.yml
* https://github.com/criticalstack/brotop/blob/master/.drone.yml
* https://github.com/vmware/harbor/blob/master/.drone.yml
