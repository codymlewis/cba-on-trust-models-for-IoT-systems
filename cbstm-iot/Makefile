PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean docs build install

deps:
	R -e "\
	r = getOption('repos'); \
	r['CRAN'] = 'https://cran.csiro.au/'; \
	options(repos = r); \
	rm(r); \
	if (!require(devtools)) { \
	install.packages('devtools'); \
	};\
	install.packages(c('ggplot2', 'styler', 'R6'), dependencies=TRUE)"

check:
	R -e "devtools::check()"

docs:
	R -e "devtools::document()"

build:
	R -e "devtools::build()"

style:
	R -e "styler::style_pkg(indent_by=4)"

ref:
	R CMD Rd2pdf .

gif:
	./mapgif

install:
	R -e "devtools::install()"

clean:
	$(RM) -r images;\
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
