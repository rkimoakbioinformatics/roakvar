% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roakvar.R
\name{run}
\alias{run}
\title{run}
\usage{
run(
  inputs = list(),
  annotators = list(),
  annotators_replace = list(),
  excludes = list(),
  run_name = NULL,
  output_dir = NULL,
  startat = NULL,
  endat = NULL,
  skip = NULL,
  confpath = NULL,
  confs = NULL,
  verbose = NULL,
  reports = list(),
  genome = NULL,
  cleandb = FALSE,
  newlog = NULL,
  note = NULL,
  mp = NULL,
  forcedinputformat = NULL,
  temp_files = NULL,
  writeadmindb = NULL,
  jobid = NULL,
  show_version = NULL,
  separatesample = NULL,
  unique_variants = NULL,
  primary_transcript = list("mane"),
  clean = NULL,
  do_not_change_status = NULL,
  module_option = NULL,
  system_option = list(),
  quiet = TRUE,
  concise_report = NULL,
  package = NULL,
  filtersql = NULL,
  includesample = NULL,
  excludesample = NULL,
  filter = NULL,
  filterpath = NULL,
  md = NULL,
  mapper_name = list(),
  postaggregators = list(),
  vcf2vcf = FALSE
)
}
\value{
A string, a named list, or a dataframe. Output of reporters
}
\description{
Run OakVar on input files.
}
\examples{
# Annotate the input file `input` with ClinVar and COSMIC modules 
# and make a VCF-format report of annotated variants.
#roakvar::run.input(inputs="input", annotators=list("clinvar", "cosmic"), reports="vcf")

}
