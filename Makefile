
SOURCES      := $(wildcard trantract/R/*.R)
DOCS         := $(wildcard trantract/man/*.Rd)
VIGNETTE     := trantract/inst/doc/trantract.Rmd
VIGNETTE_SRC := trantract/vignettes/trantract.Rmd 
CSS          := trantract/vignettes/styles.css

VER := 1.0

#
# Do not take a dependency on the vignettes here.
# The vignettes depend on package functionality, and the vignettes
# are tested when they are built and they are tested before any new
# changes are built into the package . . . :-O . . . Therefore, you
# sometimes need to rebuild the package first and then rebuild then
# rebuild the vignettes.
# 
trantract_$(VER).tar.gz: $(DOCS) $(SOURCES) 
	R CMD build trantract

$(DOCS): $(SOURCES)
	Rscript -e 'library( roxygen2 ); roxygenize( "trantract" )'

$(VIGNETTE_SRC): $(SOURCES) $(CSS)
	Rscript -e 'library( devtools ); setwd( "trantract" ); build_vignettes()'

$(VIGNETTE): $(VIGNETTE_SRC) 
	Rscript -e 'library( devtools ); setwd( "trantract" ); build_vignettes()'

install: trantract_1.0.tar.gz
	Rscript -e 'install.packages( "./trantract", repos = NULL, type = "source" )'

