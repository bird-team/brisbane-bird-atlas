#### Main instructions
# build and deploy book
all: build deploy

#### Define variables
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

#### Individual instructions
# clean compiled book
clean:
	@$(RMDIR) _book
	@$(RMDIR) _bookdown_files

# reset book to orginal text -- warning: this will reset all the book pages
reset: clean
	@$(MV) index.Rmd index.Rmd.bck
	@$(RM) *.Rmd
	@$(MV) index.Rmd.bck index.Rmd

# generate initial book with no text (warning: this will reset all the pages)
init: data/* code/initialize_book.R
	docker run --name=brisbanebird -dt 'brisbanebirdteam/docker:latest' \
	&& docker cp . brisbanebird:/tmp/ \
	&& docker exec brisbanebird sh -c "cd /tmp; Rscript code/initialize_book.R TRUE" \
	&& docker cp brisbanebird:/tmp/*.Rmd . || true
	docker stop -t 1 brisbanebird || true && docker rm brisbanebird || true

# update graphs in existing book pages with graphs in template file
update: data/* code/initialize_book.R
	docker run --name=brisbanebird -dt 'brisbanebirdteam/docker:latest' \
	&& docker cp . brisbanebird:/tmp/ \
	&& docker exec brisbanebird sh -c "cd /tmp; Rscript code/initialize_book.R FALSE" \
	&& docker cp brisbanebird:/tmp/*.Rmd . || true
	docker stop -t 1 brisbanebird || true && docker rm brisbanebird || true

# build book
build:
	echo "TODO"

# deploy book to website
deploy:
	echo "TODO"

.PHONY: clean init data update build deploy reset
