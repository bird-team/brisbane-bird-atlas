# Main instructions
## build and deploy book
all: build deploy

# Define variables
ifdef ComSpec
	RM=del /F /Q
	RMDIR=rmdir
	PATHSEP2=\\
	MV=MOVE
else
	RM=rm -f
	RMDIR=rm -rf
	PATHSEP2=/
	MV=mv
endif

# Individual instructions
## clean compiled book
clean:
	@$(RMDIR) _book
	@$(RMDIR) _bookdown_files

## reset book to orginal text -- warning: this will reset all the book pages
reset_pages: clean
	@$(MV) index.Rmd index.Rmd.bck
	@$(RM) *.Rmd
	@$(MV) index.Rmd.bck index.Rmd

## add new pages for species in file
update_pages:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/initialize_book.R FALSE" \
	&& docker cp bba:/tmp/_bookdown.yml . \
	&& docker exec bba sh -c "zip -r rmd.zip *.Rmd" \
	&& docker cp bba:/tmp/rmd.zip . \
	&& unzip -o rmd.zip \
	&& rm rmd.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# book commands
## generate initial book with no text (warning: this will reset all the pages)
init:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/initialize_book.R TRUE" \
	&& docker cp bba:/tmp/_bookdown.yml . \
	&& docker exec bba sh -c "zip -r rmd.zip *.Rmd" \
	&& docker cp bba:/tmp/rmd.zip . \
	&& unzip -o rmd.zip \
	&& rm rmd.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build assets
assets:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/create_assets.R" \
	&& docker exec bba sh -c "cd assets; zip -r maps.zip maps" \
	&& docker exec bba sh -c "cd assets; zip -r widgets.zip widgets" \
	&& docker exec bba sh -c "cd assets; zip -r graphs.zip graphs" \
	&& docker cp bba:/tmp/assets/maps.zip assets \
	&& docker cp bba:/tmp/assets/widgets.zip assets \
	&& docker cp bba:/tmp/assets/graphs.zip assets \
	&& cd assets \
	&& unzip -o maps.zip \
	&& unzip -o widgets.zip \
	&& unzip -o graphs.zip \
	&& rm maps.zip \
	&& rm widgets.zip \
	&& rm graphs.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build book
book:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "rm -rf /tmp/_book" \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/build_book.R" \
	&& docker cp bba:/tmp/_book . || true
	@docker stop -t 1 bba || true && docker rm bba || true

## deploy book
deploy: book
	echo "1"
	@set -e
	echo "2"
	echo "SLUG=${TRAVIS_REPO_SLUG}"
	echo "BRANCH=${TRAVIS_BRANCH}"
	@if [ -z "${GITHUB_PAT}" ]; then exit 0; fi;
	echo "3"
	@if [ "${TRAVIS_BRANCH}" != "master" ]; then exit 0; fi;
	echo "4"
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	echo "5"
	@git config --global user.name "bird-team-bot"
	echo "6"
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	echo "7"
	@cd book-output \
	&& echo "8" \
	&& cp -r ../_book/* ./ \
	&& echo "9"
	&& git add --all * \
	&& echo "10" \
	&& git commit -m"Automagic book update" \
	&& echo "11" \
	&& git push -q origin gh-pages

# docker container commands
## pull image
pull:
	@docker pull 'brisbanebirdteam/build-env:latest'

## remove image
rm:
	@docker image rm 'brisbanebirdteam/build-env:latest'

## spin up container
run:
	@docker run --name=bba -dt 'brisbanebirdteam/build-env:latest'

## kill container
stop:
	@docker stop -t 1 bba || true && docker rm bba || true

.PHONY: clean init data update build deploy reset assets
