dev:
	stack exec -- patches --port 8080

server: js
	stack setup
	stack build

js:
	(cd client ; stack setup)
	(cd client ; stack build)
