PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean docs build install

new-test:
	R -e "usethis::use_test('$(name)')"

test:
	R -e "devtools::test()"

deps:
	R -e "\
	r = getOption('repos'); \
	r['CRAN'] = 'https://cran.csiro.au/'; \
	options(repos = r); \
	rm(r); \
	if (!require(devtools)) { \
	install.packages('devtools'); \
	};\
	install.packages(c('png', 'ggplot2', 'tcltk2', 'styler', 'R6'), dependencies=TRUE)"

check:
	R -e "devtools::check()"

ci-test: deps all test

docs:
	R -e "devtools::document()"

build:
	R -e "devtools::build()"

atts:
	R -e "Rcpp::compileAttributes()"

style:
	R -e "styler::style_pkg(indent_by=8)"

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
