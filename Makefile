.PHONY: build run test

build:
	stack build

test:
	stack test

run:
	stack run -- write-csd --config example/config.yaml --output tmp.csd
	# stack runhaskell scripts/Playlist.hs


