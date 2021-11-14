all:
	@stack build
	@stack exec free-monads-exe

docker:
	@docker build -t pindaroso/free-monads .
