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
  ret <- oakvar$cmd_run$fn_run(
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
  ret <- oakvar$cmd_report$run_reporter(
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
#' @param noguest Diasbles guest mode
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
  ret <- oakvar$cmd_gui$fn_gui(
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

#' ov.module.ls
#'
#' @param pattern Regular expression for module names
#' @param available Include available modules
#' @param types Only list modules of certain types
#' @param include_hidden Include hidden modules
#' @param tags Only list modules of given tag(s)
#' @param quiet Only list module names
#' @param raw_bytes Machine readable data sizes
#' @param md Specify the root directory of OakVar modules
#' @param fmt Output format. tabular or json
#' @param to stdout to print / return to return
#' @export
ov.module.ls <- function(
  pattern=".*",
  available=FALSE,
  types=list(),
  include_hidden=FALSE,
  tags=list(),
  quiet=FALSE,
  raw_bytes=FALSE,
  md=NULL,
  fmt="tabular",
  to="stdout"
) {
  if (typeof(types) == "character") {
    types = list(types)
  }
  if (typeof(tags) == "character") {
    tags = list(tags)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_ls(
    list(
      pattern=pattern,
      available=available,
      types=types,
      include_hidden=include_hidden,
      tags=tags,
      quiet=quiet,
      raw_bytes=raw_bytes,
      md=md,
      fmt=fmt,
      to=to
    )
  )
  return(ret)
}

#' ov.module.install
#'
#' Install modules
#'
#' @param modules Modules to install. May be regular expressions.
#' @param force Install module even if latest version is already installed
#' @param force_data Download data even if latest data is already installed
#' @param yes Proceed without prompt
#' @param skip_dependencies Skip installing dependencies
#' @param private Install a private module
#' @param skip_data Skip installing data
#' @param md Specify the root directory of OakVar modules
#' @export
ov.module.install <- function(
  modules=NULL,
  force=FALSE,
  force_data=FALSE,
  yes=FALSE,
  skip_dependencies=FALSE,
  private=FALSE,
  skip_data=FALSE,
  md=NULL
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  if (typeof(modules) == "character") {
    modules = list(modules)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_install(
    list(
      modules=modules,
      force=force,
      force_data=force_data,
      yes=yes,
      skip_dependencies=skip_dependencies,
      private=private,
      skip_data=skip_data,
      md=md
    )
  )
  return(ret)
}

#' ov.module.uninstall
#'
#' @param modules Modules to uninstall
#' @param yes Proceed without prompt
#' @param md Specify the root directory of OakVar modules
#' @export
ov.module.uninstall <- function(
  modules=NULL,
  yes=FALSE,
  md=NULL
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  if (typeof(modules) == "character") {
    modules = list(modules)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_uninstall(
    list(
      modules=modules,
      yes=yes,
      md=md
    )
  )
  return(ret)
}

#' ov.module.update
#'
#' @param modules Modules to update.
#' @param y Proceed without prompt
#' @param strategy Dependency resolution strategy. "consensus" will attemp to resolve dependencies. "force" will install the highest available version. "skip" will skip modules with constraints.
#' @param install_pypi_dependency Try to install non-OakVar package dependency with pip
#' @param md Specify the root directory of OakVar modules
#' @export
ov.module.update <- function(
  modules=NULL,
  y=FALSE,
  strategy="consensus",
  install_pypi_dependency=TRUE,
  md=NULL
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  if (typeof(modules) == "character") {
    modules = list(modules)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_update(
    list(
      modules=modules,
      y=y,
      strategy=strategy,
      install_pypi_dependency=install_pypi_dependency,
      md=md
    )
  )
  return(ret)
}

#' ov.module.info
#'
#' @param module Module to get info about
#' @param local Include local info
#' @param md Specify the root directory of OakVar modules
#' @param to "print" to stdout / "return" to return
#' @export
ov.module.info <- function(
  module=NULL,
  local=FALSE,
  md=NULL,
  to="stdout"
) {
  if (is.null(module) || module == "") {
    print("module is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_info(
    list(
      module=module,
      local=local,
      md=md,
      to=to
    )
  )
  return(ret)
}

#' ov.module.installbase
#'
#' @param force Overwrite existing modules
#' @param force_data Download data even if latest data is already installed
#' @param install_pypi_dependency Try to install non-OakVar package dependency with pip
#' @param md Specify the root directory of OakVar modules
#' @export
ov.module.installbase <- function(
  force=FALSE,
  force_data=FALSE,
  install_pypi_dependency=TRUE,
  md=NULL
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_module_installbase(
    list(
      force=force,
      force_data=force_data,
      install_pypi_dependency=install_pypi_dependency,
      md=md
    )
  )
  return(ret)
}

#' ov.config.md
#'
#' @param directory sets modules directory.
#' @export
ov.config.md <- function(
  directory=NULL
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_config_md(
    list(
      directory=directory
    )
  )
  return(ret)
}

#' ov.config.system
#'
#' @param fmt Format of output. json or yaml.
#' @param to "stdout" to print. "return" to return
#' @export
ov.config.system <- function(
  fmt="yaml",
  to="stdout"
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_config_system(
    list(
      fmt=fmt,
      to=to
    )
  )
  return(ret)
}

#' ov.config.cravat
#'
#' @param fmt Format of output. json or yaml.
#' @param to "stdout" to print. "return" to return
#' @export
ov.config.cravat <- function(
  fmt="yaml",
  to="stdout"
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_config_cravat(
    list(
      fmt=fmt,
      to=to
    )
  )
  return(ret)
}

#' ov.new.exampleinput
#'
#' @param directory Directory to make the example input file in
#' @export
ov.new.exampleinput <- function(
  directory="."
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_new_exampleinput(
    list(
      directory=directory
    )
  )
  return(ret)
}

#' ov.new.annotator
#'
#' @param annotator_name Annotator name
#' @param md Specify the root directory of OakVar modules
#' @export
ov.new.annotator <- function(
  annotator_name="annotator",
  md=NULL
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_new_annotator(
    list(
      annotator_name=annotator_name,
      md=md
    )
  )
  return(ret)
}

#' ov.store.publish
#'
#' @param module module to publish
#' @param data publishes module with data.
#' @param code publishes module without data.
#' @param user user to publish as. Typically your email.
#' @param password password for the user. Enter at prompt if missing.
#' @param force_yes overrides yes to overwrite question
#' @param overwrite overwrites a published module/version
#' @param md Specify the root directory of OakVar modules
#' @export
ov.store.publish <- function(
  module=NULL,
  data=FALSE,
  code=FALSE,
  user=NULL,
  password=NULL,
  force_yes=FALSE,
  overwrite=FALSE,
  md=NULL
) {
  if (is.null(module) || module == "") {
    print("module is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_publish(
    list(
      module=module,
      data=data,
      code=code,
      user=user,
      password=password,
      force_yes=force_yes,
      overwrite=overwrite,
      md=md
    )
  )
  return(ret)
}

#' ov.store.newaccount
#'
#' @param username use your email as your username.
#' @param password this is your password.
#' @export
ov.store.newaccount <- function(
  username=NULL,
  password=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(password) || password == "") {
    print("password is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_newaccount(
    list(
      username=username,
      password=password
    )
  )
  return(ret)
}

#' ov.store.changepassword
#'
#' @param username username
#' @param current_password current password
#' @param new_password new password
#' @export
ov.store.changepassword <- function(
  username=NULL,
  current_password=NULL,
  new_password=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(current_password) || current_password == "") {
    print("current_password is required. returning.")
  }
  if (is.null(new_password) || new_password == "") {
    print("new_password is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_changepassword(
    list(
      username=username,
      current_password=current_password,
      new_password=new_password
    )
  )
  return(ret)
}

#' ov.store.resetpassword
#'
#' @param username username
#' @export
ov.store.resetpassword <- function(
  username=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_resetpassword(
    list(
      username=username
    )
  )
  return(ret)
}

#' ov.store.verifyemail
#'
#' @param username username
#' @export
ov.store.verifyemail <- function(
  username=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_verifyemail(
    list(
      username=username
    )
  )
  return(ret)
}

#' ov.store.checklogin
#'
#' @param username username
#' @param password password
#' @export
ov.store.checklogin <- function(
  username=NULL,
  password=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(password) || password == "") {
    print("password is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_store_checklogin(
    list(
      username=username,
      password=password
    )
  )
  return(ret)
}

#' ov.util.test
#'
#' @param rundir Directory for output
#' @param modules Name of module(s) to test. (e.g. gnomad)
#' @param mod_types Type of module(s) to test (e.g. annotators)
#' @param to stdout to print / return to return
#' @export
ov.util.test <- function(
  rundir=NULL,
  modules=NULL,
  mod_types=NULL,
  to="stdout"
) {
  if (typeof(modules) == "character") {
    modules = list(modules)
  }
  if (typeof(mod_types) == "character") {
    mod_types = list(mod_types)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_test$fn_util_test(
    list(
      rundir=rundir,
      modules=modules,
      mod_types=mod_types,
      to=to
    )
  )
  return(ret)
}

#' ov.util.updateresult
#'
#' @param dbpath path to a result db file or a directory
#' @param recursive recursive operation
#' @param backup backup original copy with .bak extension
#' @export
ov.util.updateresult <- function(
  dbpath=NULL,
  recursive=FALSE,
  backup=FALSE
) {
  if (is.null(dbpath) || dbpath == "") {
    print("dbpath is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_util$fn_util_updateresult(
    list(
      dbpath=dbpath,
      recursive=recursive,
      backup=backup
    )
  )
  return(ret)
}

#' ov.util.sendgui
#'
#' @param path Path to result database
#' @param user User who will own the job. Defaults to single user default user.
#' @export
ov.util.sendgui <- function(
  path=NULL,
  user="default"
) {
  if (is.null(path) || path == "") {
    print("path is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_util$fn_util_addjob(
    list(
      path=path,
      user=user
    )
  )
  return(ret)
}

#' ov.util.mergesqlite
#'
#' @param path Path to result database
#' @param outpath Output SQLite file path
#' @export
ov.util.mergesqlite <- function(
  path=NULL,
  outpath=NULL
) {
  if (is.null(path) || path == "") {
    print("path is required. returning.")
  }
  if (is.null(outpath) || outpath == "") {
    print("outpath is required. returning.")
  }
  if (typeof(path) == "character") {
    path = list(path)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_util$fn_util_mergesqlite(
    list(
      path=path,
      outpath=outpath
    )
  )
  return(ret)
}

#' ov.util.filtersqlite
#'
#' @param paths Path to result database
#' @param out Output SQLite file folder
#' @param suffix Suffix for output SQLite files
#' @param filterpath Path to a filter JSON file
#' @param filtersql Filter SQL
#' @param includesample Sample IDs to include
#' @param excludesample Sample IDs to exclude
#' @export
ov.util.filtersqlite <- function(
  paths=NULL,
  out=".",
  suffix="filtered",
  filterpath=NULL,
  filtersql=NULL,
  includesample=NULL,
  excludesample=NULL
) {
  if (is.null(paths) || paths == "") {
    print("paths is required. returning.")
  }
  if (typeof(paths) == "character") {
    paths = list(paths)
  }
  if (typeof(includesample) == "character") {
    includesample = list(includesample)
  }
  if (typeof(excludesample) == "character") {
    excludesample = list(excludesample)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_util$fn_util_filtersqlite(
    list(
      paths=paths,
      out=out,
      suffix=suffix,
      filterpath=filterpath,
      filtersql=filtersql,
      includesample=includesample,
      excludesample=excludesample
    )
  )
  return(ret)
}

#' ov.util.showsqliteinfo
#'
#' @param paths SQLite result file paths
#' @param fmt Output format. text / json / yaml
#' @param to Output to. stdout / return
#' @export
ov.util.showsqliteinfo <- function(
  paths=NULL,
  fmt="text",
  to="stdout"
) {
  if (is.null(paths) || paths == "") {
    print("paths is required. returning.")
  }
  if (typeof(paths) == "character") {
    paths = list(paths)
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_util$fn_util_showsqliteinfo(
    list(
      paths=paths,
      fmt=fmt,
      to=to
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
  ret <- oakvar$cmd_admin$fn_version(
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
  help="==SUPPRESS=="
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$cmd_admin$fn_feedback(
    list(
      help=help
    )
  )
  return(ret)
}

