# Main instructions
## build and deploy book
all: deploy

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
	&& rm rmd.zip \
	&& rm -f README.Rmd || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build assets
# rebuild assets locally
assets:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/create_assets.R" \
	&& docker exec bba sh -c "cd assets; zip -r maps.zip maps" \
	&& docker exec bba sh -c "cd assets; zip -r widgets.zip widgets" \
	&& docker exec bba sh -c "cd assets; zip -r graphs.zip graphs" \
	&& docker exec bba sh -c "cd assets; zip -r tables.zip tables" \
	&& docker cp bba:/tmp/assets/maps.zip assets \
	&& docker cp bba:/tmp/assets/widgets.zip assets \
	&& docker cp bba:/tmp/assets/graphs.zip assets \
	&& docker cp bba:/tmp/assets/tables.zip assets \
	&& cd assets \
	&& unzip -o maps.zip \
	&& unzip -o widgets.zip \
	&& unzip -o graphs.zip \
	&& unzip -o tables.zip \
	&& rm maps.zip \
	&& rm widgets.zip \
	&& rm graphs.zip \
	&& rm tables.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# pull assets from online storage
pull_assets:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/pull_assets.R" \
	&& docker cp bba:/tmp/assets/maps.zip assets \
	&& docker cp bba:/tmp/assets/widgets.zip assets \
	&& docker cp bba:/tmp/assets/graphs.zip assets \
	&& docker cp bba:/tmp/assets/tables.zip assets \
	&& cd assets \
	&& unzip -o maps.zip \
	&& unzip -o widgets.zip \
	&& unzip -o graphs.zip \
	&& unzip -o tables.zip \
	&& rm maps.zip \
	&& rm widgets.zip \
	&& rm graphs.zip \
	&& rm tables.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# push assets to online storage
push_assets:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker cp "$(HOME)/.Renviron" bba:/root/.Renviron \
	&& docker exec bba sh -c "cd assets; zip -r maps.zip maps" \
	&& docker exec bba sh -c "cd assets; zip -r widgets.zip widgets" \
	&& docker exec bba sh -c "cd assets; zip -r graphs.zip graphs" \
	&& docker exec bba sh -c "cd assets; zip -r tables.zip tables" \
  && docker exec bba sh -c "Rscript /tmp/code/scripts/push_assets.R" \
  && docker exec bba sh -c "rm assets/maps.zip" \
  && docker exec bba sh -c "rm assets/widgets.zip" \
  && docker exec bba sh -c "rm assets/graphs.zip" \
  && docker exec bba sh -c "rm assets/tables.zip" || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build book
book:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest' \
	&& docker cp . bba:/tmp/ \
	&& docker exec bba sh -c "rm -rf /tmp/_book" \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/build_book.R" \
	&& docker cp bba:/tmp/_book . || true
	@docker stop -t 1 bba || true && docker rm bba || true

## backup book
backup:
	@set -e
	@if [ -z "${GITLAB_PAT}" ]; then exit 0; fi;
	@if [ "${TRAVIS_BRANCH}" != "master" ]; then exit 0; fi;
	@git fetch --unshallow
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	@git config --global user.name "bird-team-bot"
	@git remote add backup https://bird-team-bot:${GITLAB_PAT}@gitlab.com/bird-team/brisbane-bird-atlas-backup.git
	@git add --all *
	@git commit -m "Automagic backup"
	@git push -q backup master
	@git remote remove backup

## deploy book
deploy: pull_assets book
	@set -e
	@if [ -z "${GITHUB_PAT}" ]; then exit 0; fi;
	@if [ "${TRAVIS_BRANCH}" != "master" ]; then exit 0; fi;
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	@git config --global user.name "bird-team-bot"
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	@cd book-output \
	&& cp -r ../_book/* ./ \
	&& git add --all * \
	&& git commit -m"Automagic book update" \
	&& git push -q origin gh-pages

# docker container commands
## pull image
pull_image:
	@docker pull 'brisbanebirdteam/build-env:latest'

## remove image
rm_image:
	@docker image rm 'brisbanebirdteam/build-env:latest'

## start container
start_container:
	@docker run --name=bba -w /tmp -dt 'brisbanebirdteam/build-env:latest'

## kill container
stop_container:
	@docker stop -t 1 bba || true && docker rm bba || true

.PHONY: clean init data update build deploy reset assets
