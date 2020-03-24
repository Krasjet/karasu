.PHONY: run run-kamome run-karasu build-karasu build-kamome debug ghci clean

run-app:
	docker-compose up

run: build-kamome run-karasu

run-kamome:
	@ln -snf kamome/dist static
	@cd kamome && yarn start

run-karasu:
	@stack run

build-karasu:
	@stack build

build-kamome:
	@cd kamome && yarn build
	@ln -snf kamome/dist static

debug:
	@stack run karasu-debug

ghci:
	@stack ghci

clean:
	rm -rf ./db
	rm -rf ./docs
	rm -rf ./view
	rm -rf ./backup
	rm -rf ./cache
	rm -rf ./.tmp
	rm -rf ./kamome/dist/*

build-docker:
	docker build -t karasu .

run-docker:
	docker run -it -v $(shell pwd)/db:/app/db/ karasu
