#' ov.run
#'
#' Run a job
#'
#' @param inputs Input file(s). One or more variant files in a supported format like VCF.  See the -i/--input-format flag for supported formats. In the special case where you want to add annotations to an existing OakVar analysis, provide the output sqlite database from the previous run as input instead of a variant input file.
#' @param annotators Annotator module names or directories. If --package is used also, annotator modules defined with -a will be added.
#' @param annotators_replace Annotator module names or directories. If --package is used also, annotator modules defined with -A will replace those defined with --package. -A has priority over -a.
#' @param excludes annotators to exclude
#' @param run_name name of oakvar run
#' @param output_dir directory for output files
#' @param startat starts at given stage
#' @param endat ends after given stage.
#' @param skip skips given stage(s).
#' @param conf path to a conf file
#' @param confs configuration string
#' @param verbose verbose
#' @param reports Reporter types or reporter module directories
#' @param genome reference genome of input. OakVar will lift over to hg38 if needed.
#' @param cleandb deletes the existing result database and creates a new one.
#' @param newlog deletes the existing log file and creates a new one.
#' @param note note will be written to the run status file (.status.json)
#' @param mp number of processes to use to run annotators
#' @param forcedinputformat Force input format
#' @param temp_files Leave temporary files after run is complete.
#' @param writeadmindb Write job information to admin db after job completion
#' @param jobid Job ID for server version
#' @param show_version Shows OakVar version.
#' @param separatesample Separate variant results by sample
#' @param unique_variants Set to get only unique variants in output
#' @param primary_transcript "mane" for MANE transcripts as primary transcripts, or a path to a file of primary transcripts. MANE is default.
#' @param clean_run Deletes all previous output files for the job and generate new ones.
#' @param do_not_change_status Job status in status.json will not be changed
#' @param module_option Module-specific option in module_name.key=value syntax. For example, --module-option vcfreporter.type=separate
#' @param system_option System option in key=value syntax. For example, --system-option modules_dir=/home/user/oakvar/modules
#' @param silent Runs silently.
#' @param concise_report Generate concise reports with default columns defined by each annotation module
#' @param package Use package
#' @param filtersql Filter SQL
#' @param includesample Sample IDs to include
#' @param excludesample Sample IDs to exclude
#' @param filter ==SUPPRESS==
#' @param filterpath Path to a filter file
#' @param md Specify the root directory of OakVar modules (annotators, etc)
#' @param mapper_name Mapper module name or mapper module directory
#' @param postaggregators Postaggregators to run. Additionally, tagsampler, casecontrol, varmeta, and vcfinfo will automatically run depending on conditions.
#' @export
ov.run <- function(
  inputs=NULL,
  annotators=list(),
  annotators_replace=list(),
  excludes=list(),
  run_name=NULL,
  output_dir=NULL,
  startat=NULL,
  endat=NULL,
  skip=NULL,
  conf="oc.yml",
  confs=NULL,
  verbose=NULL,
  reports=list(),
  genome=NULL,
  cleandb=FALSE,
  newlog=NULL,
  note=NULL,
  mp=NULL,
  forcedinputformat=NULL,
  temp_files=NULL,
  writeadmindb=NULL,
  jobid=NULL,
  show_version=NULL,
  separatesample=NULL,
  unique_variants=NULL,
  primary_transcript=list(),
  clean_run=NULL,
  do_not_change_status=NULL,
  module_option=NULL,
  system_option=NULL,
  silent=NULL,
  concise_report=NULL,
  package=NULL,
  filtersql=NULL,
  includesample=NULL,
  excludesample=NULL,
  filter=NULL,
  filterpath=NULL,
  md=NULL,
  mapper_name=list(),
  postaggregators=list()
) {
  if (typeof(inputs) == "character") {
    inputs = list(inputs)
  }
  if (typeof(annotators) == "character") {
    annotators = list(annotators)
  }
  if (typeof(annotators_replace) == "character") {
    annotators_replace = list(annotators_replace)
  }
  if (typeof(excludes) == "character") {
    excludes = list(excludes)
  }
  if (typeof(skip) == "character") {
    skip = list(skip)
  }
  if (typeof(reports) == "character") {
    reports = list(reports)
  }
  if (typeof(module_option) == "character") {
    module_option = list(module_option)
  }
  if (typeof(system_option) == "character") {
    system_option = list(system_option)
  }
  if (typeof(includesample) == "character") {
    includesample = list(includesample)
  }
  if (typeof(excludesample) == "character") {
    excludesample = list(excludesample)
  }
  if (typeof(mapper_name) == "character") {
    mapper_name = list(mapper_name)
  }
  if (typeof(postaggregators) == "character") {
    postaggregators = list(postaggregators)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cli_run$fn_run(
    list(
      inputs=inputs,
      annotators=annotators,
      annotators_replace=annotators_replace,
      excludes=excludes,
      run_name=run_name,
      output_dir=output_dir,
      startat=startat,
      endat=endat,
      skip=skip,
      conf=conf,
      confs=confs,
      verbose=verbose,
      reports=reports,
      genome=genome,
      cleandb=cleandb,
      newlog=newlog,
      note=note,
      mp=mp,
      forcedinputformat=forcedinputformat,
      temp_files=temp_files,
      writeadmindb=writeadmindb,
      jobid=jobid,
      show_version=show_version,
      separatesample=separatesample,
      unique_variants=unique_variants,
      primary_transcript=primary_transcript,
      clean_run=clean_run,
      do_not_change_status=do_not_change_status,
      module_option=module_option,
      system_option=system_option,
      silent=silent,
      concise_report=concise_report,
      package=package,
      filtersql=filtersql,
      includesample=includesample,
      excludesample=excludesample,
      filter=filter,
      filterpath=filterpath,
      md=md,
      mapper_name=mapper_name,
      postaggregators=postaggregators
    )
  )
  return(ret)
}

#' ov.report
#'
#' Generate reports from a job
#'
#' @param dbpath Path to aggregator output
#' @param reporttypes report types
#' @param filterpath Path to filter file
#' @param filter ==SUPPRESS==
#' @param filtersql Filter SQL
#' @param filtername Name of filter (stored in aggregator output)
#' @param filterstring ==SUPPRESS==
#' @param savepath Path to save file
#' @param confpath path to a conf file
#' @param module_name report module name
#' @param nogenelevelonvariantlevel Use this option to prevent gene level result from being added to variant level result.
#' @param confs Configuration string
#' @param inputfiles Original input file path
#' @param separatesample Write each variant-sample pair on a separate line
#' @param output_dir directory for output files
#' @param do_not_change_status Job status in status.json will not be changed
#' @param silent Suppress output to STDOUT
#' @param system_option System option in key=value syntax. For example, --system-option modules_dir=/home/user/oakvar/modules
#' @param module_option Module-specific option in module_name.key=value syntax. For example, --module-option vcfreporter.type=separate
#' @param concise_report Generate concise report with default columns defined by annotation modules
#' @param includesample Sample IDs to include
#' @param excludesample Sample IDs to exclude
#' @param package Use filters and report types in a package
#' @param md Specify the root directory of OakVar modules (annotators, etc)
#' @export
ov.report <- function(
  dbpath=NULL,
  reporttypes=list(),
  filterpath=NULL,
  filter=NULL,
  filtersql=NULL,
  filtername=NULL,
  filterstring=NULL,
  savepath=NULL,
  confpath=NULL,
  module_name=NULL,
  nogenelevelonvariantlevel=FALSE,
  confs="{}",
  inputfiles=NULL,
  separatesample=FALSE,
  output_dir=NULL,
  do_not_change_status=FALSE,
  silent=FALSE,
  system_option=NULL,
  module_option=NULL,
  concise_report=FALSE,
  includesample=NULL,
  excludesample=NULL,
  package=NULL,
  md=NULL
) {
  if (is.null(dbpath) || dbpath == "") {
    print("dbpath is required. returning.")
  }
  if (typeof(reporttypes) == "character") {
    reporttypes = list(reporttypes)
  }
  if (typeof(inputfiles) == "character") {
    inputfiles = list(inputfiles)
  }
  if (typeof(system_option) == "character") {
    system_option = list(system_option)
  }
  if (typeof(module_option) == "character") {
    module_option = list(module_option)
  }
  if (typeof(includesample) == "character") {
    includesample = list(includesample)
  }
  if (typeof(excludesample) == "character") {
    excludesample = list(excludesample)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cli_report$run_reporter(
    list(
      dbpath=dbpath,
      reporttypes=reporttypes,
      filterpath=filterpath,
      filter=filter,
      filtersql=filtersql,
      filtername=filtername,
      filterstring=filterstring,
      savepath=savepath,
      confpath=confpath,
      module_name=module_name,
      nogenelevelonvariantlevel=nogenelevelonvariantlevel,
      confs=confs,
      inputfiles=inputfiles,
      separatesample=separatesample,
      output_dir=output_dir,
      do_not_change_status=do_not_change_status,
      silent=silent,
      system_option=system_option,
      module_option=module_option,
      concise_report=concise_report,
      includesample=includesample,
      excludesample=excludesample,
      package=package,
      md=md
    )
  )
  return(ret)
}

#' ov.gui
#'
#' @param servermode Runs in multiuser mode
#' @param headless do not open the OakVar web page
#' @param http_only Force not to accept https connection
#' @param debug Console echoes exceptions written to log file.
#' @param result Path to a OakVar result SQLite file
#' @param webapp Name of OakVar webapp module to run
#' @param port Port number for OakVar graphical user interface
#' @param noguest Disables guest mode
#' @export
ov.gui <- function(
  servermode=FALSE,
  headless=FALSE,
  http_only=FALSE,
  debug=FALSE,
  result=NULL,
  webapp=NULL,
  port=NULL,
  noguest=FALSE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cli_gui$fn_gui(
    list(
      servermode=servermode,
      headless=headless,
      http_only=http_only,
      debug=debug,
      result=result,
      webapp=webapp,
      port=port,
      noguest=noguest
    )
  )
  return(ret)
}

#' ov.version
#'
#' @param to "stdout" to print. "return" to return
#' @export
ov.version <- function(
  to="stdout"
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cli_version$fn_version(
    list(
      to=to
    )
  )
  return(ret)
}

#' ov.feedback
#'
#' @export
ov.feedback <- function(
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cli_feedback$fn_feedback(
    list(
    )
  )
  return(ret)
}

