# Define variables
ifdef OS
	PATHSEP2 = //
	USRHOME = $(USERPROFILE)
else
	ifeq ($(shell uname), Linux)
		PATHSEP2=/
		USRHOME=$(HOME)
	endif
endif

# Main instructions
## build and deploy book
all: pull_image pull_assets book_pdf book_website

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
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/initialize_book.R FALSE" \
	&& docker cp bba:$(PATHSEP2)tmp/_bookdown.yml . \
	&& docker exec bba sh -c "zip -r rmd.zip *.Rmd" \
	&& docker cp bba:$(PATHSEP2)tmp/rmd.zip . \
	&& unzip -o rmd.zip \
	&& rm rmd.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# book commands
## generate initial book with no text (warning: this will reset all the pages)
init:
	docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/initialize_book.R TRUE" \
	&& docker cp bba:$(PATHSEP2)tmp/_bookdown.yml . \
	&& docker exec bba sh -c "zip -r rmd.zip *.Rmd" \
	&& docker cp bba:$(PATHSEP2)tmp/rmd.zip . \
	&& unzip -o rmd.zip \
	&& rm rmd.zip \
	&& rm -f README.Rmd || true
	@docker stop -t 1 bba || true && docker rm bba || true

## generate sampling grid (warning: this will reset all grid names)
grid:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript code/scripts/create_grid.R" \
	&& docker exec bba sh -c "cd data && zip -r grid.zip grid" \
	&& docker cp bba:$(PATHSEP2)tmp/data/grid.zip data \
	&& cd data \
	&& unzip -o grid.zip \
	&& rm grid.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

## backup assets
backup_assets:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp ./assets bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "zip -r assets_backup.zip assets" \
	&& docker cp bba:$(PATHSEP2)tmp/assets_backup.zip . || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build assets
# rebuild badges
badges:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/create_badges.R" \
	&& docker exec bba sh -c "cd assets; zip -r badges.zip badges" \
	&& docker cp bba:$(PATHSEP2)tmp/assets/badges.zip assets \
	&& cd assets \
	&& unzip -o badges.zip \
	&& rm badges.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# rebuild assets locally
assets: backup_assets badges
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(USRHOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/create_assets.R" \
	&& docker exec bba sh -c "cd assets; zip -r maps.zip maps" \
	&& docker exec bba sh -c "cd assets; zip -r widgets.zip widgets" \
	&& docker exec bba sh -c "cd assets; zip -r graphs.zip graphs" \
	&& docker exec bba sh -c "cd assets; zip -r tables.zip tables" \
	&& docker exec bba sh -c "cd assets; zip -r surveyor-sheets.zip surveyor-sheets" \
	&& docker cp bba:$(PATHSEP2)tmp/assets/maps.zip assets \
	&& docker cp bba:$(PATHSEP2)tmp/assets/widgets.zip assets \
	&& docker cp bba:$(PATHSEP2)tmp/assets/graphs.zip assets \
	&& docker cp bba:$(PATHSEP2)tmp/assets/tables.zip assets \
	&& docker cp bba:$(PATHSEP2)tmp/assets/surveyor-sheets.zip assets \
	&& docker cp bba:$(PATHSEP2)tmp/tictoclog.txt tictoclog.txt \
	&& cd assets \
	&& unzip -o maps.zip \
	&& unzip -o widgets.zip \
	&& unzip -o graphs.zip \
	&& unzip -o tables.zip \
	&& unzip -o surveyor-sheets.zip \
	&& rm maps.zip \
	&& rm widgets.zip \
	&& rm graphs.zip \
	&& rm tables.zip \
	&& rm surveyor-sheets.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# pull assets from online storage
pull_assets:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/pull_assets.R" \
	&& docker cp bba:$(PATHSEP2)tmp/assets/assets-maps.zip assets/maps.zip \
	&& docker cp bba:$(PATHSEP2)tmp/assets/assets-widgets.zip assets/widgets.zip \
	&& docker cp bba:$(PATHSEP2)tmp/assets/assets-graphs.zip assets/graphs.zip \
	&& docker cp bba:$(PATHSEP2)tmp/assets/assets-tables.zip assets/tables.zip \
	&& docker cp bba:$(PATHSEP2)tmp/assets/assets-surveyor-sheets.zip assets/surveyor-sheets.zip \
	&& cd assets \
	&& unzip -o maps.zip \
	&& unzip -o widgets.zip \
	&& unzip -o graphs.zip \
	&& unzip -o tables.zip \
	&& unzip -o surveyor-sheets.zip \
	&& rm maps.zip \
	&& rm widgets.zip \
	&& rm graphs.zip \
	&& rm tables.zip \
	&& rm surveyor-sheets.zip || true
	@docker stop -t 1 bba || true && docker rm bba || true

# push assets to online storage
test_access:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(USRHOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
	&& docker exec bba sh -c "ls -la /root/.Renviron" \
	&& docker exec bba sh -c "cd ~ && pwd" \
	&& docker exec bba sh -c "ping -c3 www.google.com" \
	&& docker exec bba sh -c "R -e 'f=tempfile();writeLines(\"this is a test2\", f);piggyback::pb_upload(file=f, repo=\"bird-team/brisbane-bird-atlas\", tag=\"v.0.0.1\", name=\"new-test5.txt\")'" || true
	@docker stop -t 1 bba || true && docker rm bba || true

# push assets to online storage
push_assets:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(USRHOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
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

# push surveyor sheets to online storage
push_surveyor_sheets:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(USRHOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
	&& docker exec bba sh -c "cd assets; zip -r surveyor-sheets.zip surveyor-sheets" \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/push_surveyor_sheets.R" \
	&& docker exec bba sh -c "rm assets/surveyor-sheets.zip" || true
	@docker stop -t 1 bba || true && docker rm bba || true

## remove surveyor sheets
rm_gh_surveyor_sheets:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(USRHOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/rm_gh_surveyor_sheets.R" || true
	@docker stop -t 1 bba || true && docker rm bba || true

## build book
book_pdf:
	@mkdir -p _book \
	&& docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/build_book_pdf.R" \
	&& docker cp bba:$(PATHSEP2)tmp/_book/brisbane-bird-atlas.pdf _book || true
	@docker stop -t 1 bba || true && docker rm bba || true
	@ls -la _book/brisbane-bird-atlas.pdf

book_website:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/build_book_website.R" \
	&& docker cp bba:$(PATHSEP2)tmp/_book . || true
	@docker stop -t 1 bba || true && docker rm bba || true

## backup book
backup:
	@set -e
	@if [ -z "${GITLAB_PAT}" ]; then exit 0; fi;
	@if [ "${CIRCLE_BRANCH}" != "master" ]; then exit 0; fi;
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	@git config --global user.name "bird-team-bot"
	@git remote add backup https://bird-team-bot:${GITLAB_PAT}@gitlab.com/${CIRCLE_PROJECT_USERNAME}/${CIRCLE_PROJECT_REPONAME}-backup.git
	@git push -q backup master
	@git remote remove backup

## deploy book
deploy_book_website:
	@set -e
	@if [ -z "${GITHUB_PAT}" ]; then exit 0; fi;
	@if [ "${CIRCLE_BRANCH}" != "master" ]; then exit 0; fi;
	@git config --global user.email "jeff.o.hanson+bot@gmail.com"
	@git config --global user.name "bird-team-bot"
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${CIRCLE_PROJECT_USERNAME}/${CIRCLE_PROJECT_REPONAME}.git book-output
	@cd book-output \
	&& cp -r ../_book/* ./ \
	&& git add --all * \
	&& git commit -m"Automagic book update" \
	&& git push -q origin gh-pages

deploy_book_pdf:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest \
	&& docker cp . bba:$(PATHSEP2)tmp/ \
	&& docker cp "$(HOME)/.Renviron" bba:$(PATHSEP2)root/.Renviron \
	&& docker exec bba sh -c "Rscript /tmp/code/scripts/push_book_pdf.R" \
	&& docker cp bba:$(PATHSEP2)tmp/_book . || true
	@docker stop -t 1 bba || true && docker rm bba || true
	@ls -la _book/brisbane-bird-atlas.pdf

# docker container commands
## pull image
pull_image:
	@docker pull brisbanebirdteam/build-env:latest

## remove image
rm_image:
	@docker image rm brisbanebirdteam/build-env:latest

## start container
start_container:
	@docker run --name=bba -w $(PATHSEP2)tmp -dt brisbanebirdteam/build-env:latest

## kill container
stop_container:
	@docker stop -t 1 bba || true && docker rm bba || true

.PHONY: clean init data update build deploy reset assets badges
