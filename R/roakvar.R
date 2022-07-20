#' run
#'
#' Run OakVar on input files.
#'
#'
#' @return A string, a named list, or a dataframe. Output of reporters
#'
#' @export
#'
#' @examples
#' # Annotate the input file `input` with ClinVar and COSMIC modules 
#' # and make a VCF-format report of annotated variants.
#' #roakvar::run.input(inputs="input", annotators=list("clinvar", "cosmic"), reports="vcf")
#'
run <- function(
  inputs=list(),
  annotators=list(),
  annotators_replace=list(),
  excludes=list(),
  run_name=NULL,
  output_dir=NULL,
  startat=NULL,
  endat=NULL,
  skip=NULL,
  confpath=NULL,
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
  primary_transcript=list('mane'),
  clean=NULL,
  do_not_change_status=NULL,
  module_option=NULL,
  system_option=list(),
  quiet=TRUE,
  concise_report=NULL,
  package=NULL,
  filtersql=NULL,
  includesample=NULL,
  excludesample=NULL,
  filter=NULL,
  filterpath=NULL,
  md=NULL,
  mapper_name=list(),
  postaggregators=list(),
  vcf2vcf=FALSE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$run(
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
      confpath=confpath,
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
      clean=clean,
      do_not_change_status=do_not_change_status,
      module_option=module_option,
      system_option=system_option,
      quiet=quiet,
      concise_report=concise_report,
      package=package,
      filtersql=filtersql,
      includesample=includesample,
      excludesample=excludesample,
      filter=filter,
      filterpath=filterpath,
      md=md,
      mapper_name=mapper_name,
      postaggregators=postaggregators,
      vcf2vcf=vcf2vcf
    )
  )
  if (!is.null(ret$pandas)) {
    ret$pandas = reticulate::py_to_r(ret$pandas)
  }
  if (!is.null(ret$r)) {
    ret$r = reticulate::py_to_r(ret$r)
  }
  return(ret)
}

#' report
#'
#' Generate reports from a job
#'
#'
#' @return A string, a named list, or a dataframe. Output of reporters
#'
#' @export
#'
#' @examples
#' # Generate a CSV-format report file from the job result file example.sqlite
#' #roakvar::report(dbpath="example.sqlite", reports="csv")
#'
report <- function(
  dbpath=NULL,
  reports=list(),
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
  quiet=TRUE,
  system_option=NULL,
  module_option=NULL,
  concise_report=FALSE,
  includesample=NULL,
  excludesample=NULL,
  package=NULL,
  md=NULL,
  level=NULL
) {
  if (is.null(dbpath) || dbpath == "") {
    print("dbpath is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$report(
    list(
      dbpath=dbpath,
      reports=reports,
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
      quiet=quiet,
      system_option=system_option,
      module_option=module_option,
      concise_report=concise_report,
      includesample=includesample,
      excludesample=excludesample,
      package=package,
      md=md,
      level=level
    )
  )
  if (!is.null(ret$pandas)) {
    ret$pandas = reticulate::py_to_r(ret$pandas)
  }
  if (!is.null(ret$r)) {
    ret$r = reticulate::py_to_r(ret$r)
  }
  return(ret)
}

#' module.installbase
#'
#' installs base modules.
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Install OakVar system modules
#' #roakvar::module.installbase()
#'
module.installbase <- function(
  force=NULL,
  force_data=FALSE,
  md=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_installbase(
    list(
      force=force,
      force_data=force_data,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.install
#'
#' Installs OakVar modules.
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Install the ClinVar module without confirmation
#' #roakvar::module.install(modules="clinvar", force=True)
#'
module.install <- function(
  modules=NULL,
  force=FALSE,
  force_data=FALSE,
  yes=FALSE,
  skip_dependencies=FALSE,
  private=FALSE,
  skip_data=FALSE,
  md=NULL,
  to="return",
  quiet=TRUE
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_install(
    list(
      modules=modules,
      force=force,
      force_data=force_data,
      yes=yes,
      skip_dependencies=skip_dependencies,
      private=private,
      skip_data=skip_data,
      md=md,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.pack
#'
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Pack a module "mymodule" into one zip file for its code and another zip file for its data.
#' #roakvar::store.pack(module="mymodule")
#'
module.pack <- function(
  module=NULL,
  outdir=".",
  quiet=TRUE
) {
  if (is.null(module) || module == "") {
    print("module is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_pack(
    list(
      module=module,
      outdir=outdir,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.update
#'
#' updates modules.
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Update the ClinVar module without confirmation
#' #roakvar::module.update(modules="clinvar", force=True)
#'
module.update <- function(
  modules=NULL,
  y=FALSE,
  strategy="consensus",
  md=NULL,
  quiet=TRUE
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_update(
    list(
      modules=modules,
      y=y,
      strategy=strategy,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.uninstall
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Uninstall the ClinVar module without confirmation
#' #roakvar::module.uninstall(modules="clinvar", force=True)
#'
module.uninstall <- function(
  modules=NULL,
  yes=FALSE,
  md=NULL,
  quiet=TRUE
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_uninstall(
    list(
      modules=modules,
      yes=yes,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.info
#'
#'
#' @return A named list. Information of the queried module
#'
#' @export
#'
#' @examples
#' # Get the information of the ClinVar module
#' #roakvar::module.info(module="clinvar")
#'
module.info <- function(
  module=NULL,
  local=FALSE,
  md=NULL,
  fmt="json",
  to="return",
  quiet=TRUE
) {
  if (is.null(module) || module == "") {
    print("module is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_info(
    list(
      module=module,
      local=local,
      md=md,
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' module.ls
#'
#' lists modules.
#'
#'
#' @return A named list. List of modules
#'
#' @export
#'
#' @examples
#' # Get the list of all installed modules
#' #roakvar::module.ls()
#' # Get the list of all available modules
#' #roakvar::module.ls(available=TRUE)
#' # Get the list of all available modules of the type "converter"
#' #roakvar::module.ls(available=TRUE, types="converter")
#'
module.ls <- function(
  pattern=".*",
  available=FALSE,
  types=list(),
  include_hidden=FALSE,
  tags=list(),
  nameonly=FALSE,
  raw_bytes=FALSE,
  md=NULL,
  fmt=NULL,
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$module_ls(
    list(
      pattern=pattern,
      available=available,
      types=types,
      include_hidden=include_hidden,
      tags=tags,
      nameonly=nameonly,
      raw_bytes=raw_bytes,
      md=md,
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' gui
#'
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' # Launch OakVar GUI
#' #roakvar::gui()
#' # Launch OakVar Interactive Result Viewer for the OakVar analysis file example.sqlite
#' #roakvar::gui(result="example.sqlite")
#'
gui <- function(
  servermode=FALSE,
  headless=FALSE,
  http_only=FALSE,
  debug=FALSE,
  result=NULL,
  webapp=NULL,
  port=NULL,
  noguest=FALSE,
  quiet=TRUE,
  to="return"
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$gui(
    list(
      servermode=servermode,
      headless=headless,
      http_only=http_only,
      debug=debug,
      result=result,
      webapp=webapp,
      port=port,
      noguest=noguest,
      quiet=quiet,
      to=to
    )
  )
  return(ret)
}

#' config.user
#'
#'
#' @return A named list. OakVar user config information
#'
#' @export
#'
#' @examples
#' # Get the named list of the OakVar user configuration
#' #roakvar::config.user()
#'
config.user <- function(
  fmt="json",
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$config_user(
    list(
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' config.system
#'
#'
#' @return A named list. OakVar system config information
#'
#' @export
#'
#' @examples
#' # Get the named list of the OakVar system configuration
#' #roakvar::config.system()
#'
config.system <- function(
  fmt="json",
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$config_system(
    list(
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' new.exampleinput
#'
#'
#' @return A string. Location of the example input file
#'
#' @export
#'
#' @examples
#' # Create an example input file in the current working directory
#' #roakvar::new.exampleinput()
#' # Create an example input file at /home/user1/
#' #roakvar::new.exampleinput(directory="/home/user1")
#'
new.exampleinput <- function(
  directory=".",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$new_exampleinput(
    list(
      directory=directory,
      quiet=quiet
    )
  )
  return(ret)
}

#' new.annotator
#'
#'
#' @return A string. Location of the new annotator module
#'
#' @export
#'
#' @examples
#' # Create an annotator template at the OakVar modules directory/annotators/annotatortest
#' #roakvar::new.annotator(annotator_name="annotatortest")
#'
new.annotator <- function(
  annotator_name="exampleannotator",
  md=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$new_annotator(
    list(
      annotator_name=annotator_name,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.create
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Create a store account
#' #roakvar::store.createaccount(email="user1", password="password")
#'
store.account.create <- function(
  email=NULL,
  pw=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_create(
    list(
      email=email,
      pw=pw,
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.delete
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Create a store account
#' #roakvar::store.deleteaccount(email="user1", password="password")
#'
store.account.delete <- function(
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_delete(
    list(
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.change
#'
#'
#' @return A string. Response from the store server
#'
#' @export
#'
#' @examples
#' # Change the password of a store account
#' #roakvar::store.changepassword(email="user1", 
#' # current_password="password", new_password="newpassword")
#'
store.account.change <- function(
  newpw=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_change(
    list(
      newpw=newpw,
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.check
#'
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Check if the current is logged in the OakVar Store nor not. 
#' #roakvar::store.checklogin(email="user1", password="password")
#'
store.account.check <- function(
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_check(
    list(
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.login
#'
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' # Log in to the OakVar Store
#' #roakvar::store.account.login(email="user1", pw="password")
#'
store.account.login <- function(
  email=NULL,
  pw=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_login(
    list(
      email=email,
      pw=pw,
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.logout
#'
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' # Log out from the OakVar Store
#' #roakvar::store.account.logout()
#'
store.account.logout <- function(
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_logout(
    list(
      quiet=quiet
    )
  )
  return(ret)
}

#' store.account.reset
#'
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Ask the store to send an email to reset the password of a store account
#' #roakvar::store.account.reset(email="user1")
#'
store.account.reset <- function(
  quiet=TRUE,
  email=NULL
) {
  if (is.null(email) || email == "") {
    print("email is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_account_reset(
    list(
      quiet=quiet,
      email=email
    )
  )
  return(ret)
}

#' store.register
#'
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Publish "customannot" module to the store
#' #roakvar::store.publish(module="customannot", 
#' # code_url="https://test.com/customannot__1.0.0__code.zip", 
#' # data_url="https://test.com/customannot__1.0.0__data.zip")
#'
store.register <- function(
  module_name=NULL,
  md=NULL,
  quiet=TRUE,
  code_url=NULL,
  data_url=NULL,
  overwrite=FALSE
) {
  if (is.null(module_name) || module_name == "") {
    print("module_name is required. returning.")
  }
  if (is.null(code_url) || code_url == "") {
    print("code_url is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_register(
    list(
      module_name=module_name,
      md=md,
      quiet=quiet,
      code_url=code_url,
      data_url=data_url,
      overwrite=overwrite
    )
  )
  return(ret)
}

#' store.fetch
#'
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Fetch the store information
#' #roakvar::store.fetch()
#'
store.fetch <- function(
  quiet=TRUE,
  email=NULL,
  pw=NULL,
  clean=FALSE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_fetch(
    list(
      quiet=quiet,
      email=email,
      pw=pw,
      clean=clean
    )
  )
  return(ret)
}

#' store.url
#'
#'
#' @return character
#'
#' @export
#'
#' @examples
#' # Returns the URL of the OakVar store.
#' #roakvar::store.account.url()
#'
store.url <- function(
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_url(
    list(
      quiet=quiet
    )
  )
  return(ret)
}

#' store.oc.publish
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Publish a module to the OpenCRAVAT store
#' #roakvar::store.oc.publish(module="clinvar", user="user1", password="password", code=TRUE)
#'
store.oc.publish <- function(
  module=NULL,
  data=FALSE,
  code=FALSE,
  user=NULL,
  password=NULL,
  force_yes=FALSE,
  overwrite=FALSE,
  md=NULL,
  quiet=TRUE
) {
  if (is.null(module) || module == "") {
    print("module is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_oc_publish(
    list(
      module=module,
      data=data,
      code=code,
      user=user,
      password=password,
      force_yes=force_yes,
      overwrite=overwrite,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' store.oc.newaccount
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Create a developer account at the OpenCRAVAT store
#' #roakvar::store.oc.newaccount(username="user1", password="password")
#'
store.oc.newaccount <- function(
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
  ret <- oakvar$store_oc_newaccount(
    list(
      username=username,
      password=password
    )
  )
  return(ret)
}

#' store.oc.changepw
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Change the password of a developer account at the OpenCRAVAT store
#' #roakvar::store.oc.changepw(username="user1", cur_pw="password", new_pw="newpassword")
#'
store.oc.changepw <- function(
  username=NULL,
  cur_pw=NULL,
  new_pw=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(cur_pw) || cur_pw == "") {
    print("cur_pw is required. returning.")
  }
  if (is.null(new_pw) || new_pw == "") {
    print("new_pw is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_oc_changepw(
    list(
      username=username,
      cur_pw=cur_pw,
      new_pw=new_pw
    )
  )
  return(ret)
}

#' store.oc.resetpw
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Reset the password of a developer account at the OpenCRAVAT store
#' #roakvar::store.oc.resetpw(username="user1")
#'
store.oc.resetpw <- function(
  username=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_oc_resetpw(
    list(
      username=username
    )
  )
  return(ret)
}

#' store.oc.verifyemail
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Sends a verification email for a developer account at the OpenCRAVAT store
#' #roakvar::store.oc.verifyemail(username="user1")
#'
store.oc.verifyemail <- function(
  username=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$store_oc_verifyemail(
    list(
      username=username
    )
  )
  return(ret)
}

#' store.oc.checklogin
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not.
#'
#' @export
#'
#' @examples
#' # Check the username and password of a developer account at the OpenCRAVAT store
#' #roakvar::store.oc.checklogin(username="user1")
#'
store.oc.checklogin <- function(
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
  ret <- oakvar$store_oc_checklogin(
    list(
      username=username,
      password=password
    )
  )
  return(ret)
}

#' util.test
#'
#' Test modules
#'
#'
#' @return A named list. Field result is a named list showing the test result for each module. Fields num_passed and num_failed show the number of passed and failed modules.
#'
#' @export
#'
#' @examples
#' # Test the ClinVar module
#' #roakvar::util.test(modules="clinvar")
#' # Test the ClinVar and the COSMIC modules
#' #roakvar::util.test(modules=list("clinvar", "cosmic"))
#'
util.test <- function(
  rundir=NULL,
  modules=NULL,
  mod_types=NULL,
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$util_test(
    list(
      rundir=rundir,
      modules=modules,
      mod_types=mod_types,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' util.addjob
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Add a result file to the job list of a user
#' #roakvar::util.addjob(path="example.sqlite", user="user1")
#'
util.addjob <- function(
  path=NULL,
  user="default"
) {
  if (is.null(path) || path == "") {
    print("path is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$util_addjob(
    list(
      path=path,
      user=user
    )
  )
  return(ret)
}

#' util.mergesqlite
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Merge two OakVar analysis result files into one SQLite file
#' #roakvar::util.mergesqlite(path=list("example1.sqlite", "example2.sqlite"), outpath="merged.sqlite")
#'
util.mergesqlite <- function(
  path=NULL,
  outpath=NULL
) {
  if (is.null(path) || path == "") {
    print("path is required. returning.")
  }
  if (is.null(outpath) || outpath == "") {
    print("outpath is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$util_mergesqlite(
    list(
      path=path,
      outpath=outpath
    )
  )
  return(ret)
}

#' util.sqliteinfo
#'
#'
#' @return A named list. Information of a job SQLite file
#'
#' @export
#'
#' @examples
#' # Get the named list of the information of an analysis result file
#' #roakvar::util.sqliteinfo(paths="example.sqlite")
#'
util.sqliteinfo <- function(
  paths=NULL,
  fmt="json",
  to="return"
) {
  if (is.null(paths) || paths == "") {
    print("paths is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$util_sqliteinfo(
    list(
      paths=paths,
      fmt=fmt,
      to=to
    )
  )
  return(ret)
}

#' util.filtersqlite
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Filter an analysis result file with an SQL filter set
#' #roakvar::util.filtersqlite(paths="example.sqlite", 
#' #  filtersql='base__so=="MIS" and gnomad__af>0.01')
#' # Filter two analysis result files with a filter definition file
#' #roakvar::util.filtersqlite(paths=list("example1.sqlite", 
#' #  "example2.sqlite"), filterpath="filter.json")
#'
util.filtersqlite <- function(
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
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$util_filtersqlite(
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

#' version
#'
#'
#' @return A string. OakVar version
#'
#' @export
#'
#' @examples
#' # Get the version of the installed OakVar
#' #roakvar::version()
#'
version <- function(
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$version(
    list(
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' issue
#'
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' # Open the Issues page of the OakVar GitHub website
#' #roakvar::issue()
#'
issue <- function(
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$issue(
    list(
      quiet=quiet
    )
  )
  return(ret)
}

#' system.setup
#'
#' Sets up OakVar system
#'
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Set up OakVar with defaults
#' #roakvar::system.setup()
#' # Set up OakVar with a setup file
#' #roakvar::system.setup(setup_file="setup.yml")
#'
system.setup <- function(
  setup_file=NULL,
  clean=FALSE,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$system_setup(
    list(
      setup_file=setup_file,
      clean=clean,
      quiet=quiet
    )
  )
  return(ret)
}

#' system.md
#'
#' displays or changes OakVar modules directory.
#'
#'
#' @return A string. OakVar modules directory
#'
#' @export
#'
#' @examples
#' # Get the OakVar modules directory
#' #roakvar::system.md()
#' # Set the OakVar modules directory to /home/user1/.oakvar/modules
#' #roakvar::system.md(directory="/home/user1/.oakvar/modules")
#'
system.md <- function(
  directory=NULL,
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$system_md(
    list(
      directory=directory,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' system.config
#'
#'
#' @return A named list. System config information
#'
#' @export
#'
#' @examples
#' # Get named list of the OakVar system configuration
#' #roakvar::system.config()
#' # Get the OakVar system configuration in YAML text
#' #roakvar::system.config(fmt="yaml")# Print to stdout the OakVar system configuration in YAML text
#' #roakvar::system.config(fmt="yaml", to="stdout")
#'
system.config <- function(
  fmt="json",
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$system_config(
    list(
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' system.check
#'
#'
#' @return A boolean. true if no problem or false if not.
#'
#' @export
#'
#' @examples
#' # Check if OakVar is set up correctly.
#' #roakvar::system.check()
#'
system.check <- function(
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$system_check(
    list(
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}


#' load.result
#'
#' @param dbpath OakVar result database file
#' @param level Level to fetch result for. 'variant', 'gene', 'sample', or 'mapping', for example
#' @param quiet Run quietly
#'
#' @return A list. Fetched result
#'
#' @export
#'
#' @examples
#' # Fetch the variant level result from the result database exampleinput.sqlite.
#' #roakvar::load.result("exampleinput.sqlite", level='variant')
#'
load.result = function(dbpath, level='variant', quiet=TRUE) {
  ret = roakvar::report(dbpath=dbpath, reports="r", level=level, quiet=quiet)
  ret = ret$r
  return(ret)
}
