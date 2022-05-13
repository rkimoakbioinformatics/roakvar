## roakvar

roakvar is an R wrapper for the Python-based genomic variant analysis platform, OakVar, bringing the full power of genomic variant analysis to the R environment.

## Installation

You can install the development version of roakvar from https://github.com/rkimoakbioinformatics/roakvar with:

``` r
# install.packages("devtools")
devtools::install_github("rkimoakbioinformatics/roakvar")
```

As roakvar is a wrapper for OakVar, OakVar should be installed as well. Follow the instruction at https://oakvar.readthedocs.io to install OakVar.

## Example

These are basic examples which show you how to solve common problems:

``` r
library(roakvar)
# installs OakVar system modules.
ov.module.installbase()
# lists all downloadable OakVar modules.
ov.module.ls(available=TRUE)
# installs the ClinVar annotation module.
ov.module.install("clinvar")
# installs the VCF format reporting module.
ov.module.install("vcfreporter")
# creates an example variant file.
ov.new.exampleinput()
# runs an example analysis with ClinVar and generates a VCF file with annotated variants.
ov.run(inputs=list("exampleinput"), annotators=list("clinvar"), reports=list("vcf"))
# views the analysis result on the default browser.
ov.gui(result="exampleinput.sqlite")
```

