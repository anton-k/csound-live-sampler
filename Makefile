.PHONY: build run test

build:
	stack build

test:
	stack test

run:
	stack runhaskell scripts/Playlist.hs

#	stack run -- example/config.yaml

