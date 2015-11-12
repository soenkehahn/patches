server: js
	stack setup
	stack build

js:
	(cd client ; stack setup)
	(cd client ; stack build)
