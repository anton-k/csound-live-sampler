.PHONY: build run test

build:
	stack build

test:
	stack test

run:
	stack run -- example/config.yaml
	# stack runhaskell scripts/Playlist.hs


