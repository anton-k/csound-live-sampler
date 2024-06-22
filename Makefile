.PHONY: build run test

build:
	stack build

test:
	stack test

run:
	stack run -- write-csd --config example/config.yaml --output tmp.csd

repl-client:
	stack ghci csound-live-sampler-osc-client
