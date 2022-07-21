# Roakvar

Roakvar is an R wrapper for the Python-based genomic variant analysis platform, OakVar, bringing the full power of genomic variant analysis to the R environment.

Teaser video: https://www.youtube.com/watch?v=sEQh6J0QUL4

## Installation

* R side: Roakvar has been submitted to CRAN. Meanwhile, install with:

``` r
# install.packages("devtools")
devtools::install_github("rkimoakbioinformatics/roakvar")
```

* Python side: As roakvar is a wrapper for OakVar, OakVar should be installed as well. See the instruction at https://oakvar.readthedocs.io for the details of installing OakVar, but basically:

``` bash
pip install oakvar
```

## Example

Annotate a VCF file with ClinVar and read the result into a data frame:

```r
module.install(modules=list("clinvar", "pandasreporter")) # install the ClinVar and pandas reporter modules.
result = roakvar::run(inputs="input.vcf", annotators="clinvar", reports="pandas") # Do the analysis and get the result into a variable.
df = result$pandas # Get the data frame.
```

These are basic examples which show you how to solve common problems:

``` r
library(roakvar)
# installs OakVar system modules.
module.installbase()
# lists all downloadable OakVar modules.
module.ls(available=TRUE)
# installs the ClinVar annotation module.
module.install("clinvar")
# installs the VCF format reporting module.
module.install("vcfreporter")
# creates an example variant file.
new.exampleinput()
# runs an example analysis with ClinVar and generates a VCF file with annotated variants.
run(inputs=list("exampleinput"), annotators=list("clinvar"), reports=list("vcf"))
# views the analysis result on the default browser.
gui(result="exampleinput.sqlite")
```

Roakvar's R functions matches OakVar's CLI commands. See https://docs.oakvar.com for OakVar CLI commands.
