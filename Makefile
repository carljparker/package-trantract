
SOURCES      := $(wildcard trantract/R/*.R)
DOCS         := $(wildcard trantract/man/*.Rd)
VIGNETTE     := trantract/inst/doc/trantract.Rmd
VIGNETTE_SRC := trantract/vignettes/trantract.Rmd 

trantract_1.0.tar.gz: $(DOCS) $(SOURCES) $(VIGNETTE)
	R CMD build trantract

$(DOCS): $(SOURCES)
	Rscript -e 'library( roxygen2 ); roxygenize( "trantract" )'

install: trantract_1.0.tar.gz
	Rscript -e 'install.packages( "./trantract", repos = NULL, type = "source" )'

$(VIGNETTE): $(VIGNETTE_SRC) 
	Rscript -e 'library( devtools ); setwd( "trantract" ); build_vignettes()'

