## Builds base image used for `stack image container`
build-base:
	@docker build -t ublubu/stackbase -f Dockerfile.base .

## Builds app using stack-native.yaml
build-stack-native: build-base
	@stack --stack-yaml stack-native.yaml build
	@stack --stack-yaml stack-native.yaml image container

push: build-stack-native
	@docker push ublubu/stackapp
