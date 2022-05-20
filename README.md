## Roakvar

Roakvar is an R wrapper for the Python-based genomic variant analysis platform, OakVar, bringing the full power of genomic variant analysis to the R environment.

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
ov.module.install(modules=list("clinvar", "pandasreporter")) # install the ClinVar and pandas reporter modules.
result = ov.run(inputs="input.vcf", annotators="clinvar", reports="pandas") # Do the analysis and get the result into a variable.
df = result$pandas # Get the data frame.
```

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

Roakvar's R functions correspond to OakVar's CLI commands one-to-one. See https://oakvar.readthedocs.io for OakVar CLI commands.
