.PHONY: all
all:
	stack --stack-yaml stack-lts-12.6.yaml build --fast
	stack --stack-yaml stack-lts-13.6.yaml build --fast
