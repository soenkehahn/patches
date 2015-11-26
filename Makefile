server: js
	stack setup
	stack build

js:
	(cd client ; hpack && stack setup)
	(cd client ; hpack && stack build)

dev:
	stack exec -- patches --port 8080
