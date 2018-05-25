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
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/build_book.R" \
	&& docker cp bba:/tmp/_book . || true
	@docker stop -t 1 bba || true && docker rm bba || true

## deploy book
deploy: build
	@set -e
	@[ -z "${GITHUB_PAT}" ] && exit 0
	@[ "${TRAVIS_BRANCH}" != "master" ] && exit 0
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	@git config --global user.name "bird-team-bot"
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	@cd book-output
	@cp -r ../_book/* ./
	@git add --all *
	@git commit -m"Automagic book update" || true
	@git push -q origin gh-pages

# docker container commands
## spin up container
run:
	@docker run --name=bba -dt 'brisbanebirdteam/build-env:latest'

## kill container
stop:
	@docker stop -t 1 bba || true && docker rm bba || true

.PHONY: clean init data update build deploy reset assets
