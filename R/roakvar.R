#' ov.module.installbase
#'
#' installs base modules.
#'
#' @param force Overwrite existing modules
#' @param force_data Download data even if latest data is already installed
#' @param md Specify the root directory of OakVar modules
#' @param quiet suppress stdout output
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Install OakVar system modules
#' ov.module.installbase()
#'
ov.module.installbase <- function(
  force=FALSE,
  force_data=FALSE,
  md=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_module_installbase(
    list(
      force=force,
      force_data=force_data,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.module.install
#'
#' Installs OakVar modules.
#'
#' @param modules Modules to install. May be regular expressions.
#' @param force Install module even if latest version is already installed
#' @param force_data Download data even if latest data is already installed
#' @param yes Proceed without prompt
#' @param skip_dependencies Skip installing dependencies
#' @param private Install a private module
#' @param skip_data Skip installing data
#' @param md Specify the root directory of OakVar modules
#' @param to 'stdout' to print. 'return' to return
#' @param quiet suppress stdout output
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Install the ClinVar module
#' ov.module.install(modules="clinvar")
#' # Install the ClinVar and the COSMIC modules
#' ov.module.install(modules=list("clinvar", "cosmic")
#' # Re-install the ClinVar module overwriting the already installed copy
#' ov.module.install(modules="clinvar", force=TRUE)
#'
ov.module.install <- function(
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
  ret <- oakvar$ov_module_install(
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

#' ov.module.update
#'
#' updates modules.
#'
#' @param modules Modules to update.
#' @param y Proceed without prompt
#' @param strategy Dependency resolution strategy. "consensus" will attempt to resolve dependencies. "force" will install the highest available version. "skip" will skip modules with constraints.
#' @param md Specify the root directory of OakVar modules
#' @param quiet suppress stodout output
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Update the ClinVar module
#' ov.module.update(modules="clinvar")
#' # Update all the installed modules
#' ov.module.update()
#'
ov.module.update <- function(
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
  ret <- oakvar$ov_module_update(
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

#' ov.module.uninstall
#'
#' @param modules Modules to uninstall
#' @param yes Proceed without prompt
#' @param md Specify the root directory of OakVar modules
#' @param quiet Run quietly
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Uninstall the ClinVar module
#' ov.module.uninstall(modules="clinvar")# Uninstall the ClinVar and the COSMIC modules
#' ov.module.uninstall(modules=("clinvar", "cosmic")
#'
ov.module.uninstall <- function(
  modules=NULL,
  yes=FALSE,
  md=NULL,
  quiet=TRUE
) {
  if (is.null(modules) || modules == "") {
    print("modules is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_module_uninstall(
    list(
      modules=modules,
      yes=yes,
      md=md,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.module.info
#'
#' @param module Module to get info about
#' @param local Include local info
#' @param md Specify the root directory of OakVar modules
#' @param fmt format of module information data. json or yaml
#' @param to "stdout" to stdout / "return" to return
#' @param quiet Run quietly
#'
#' @return A named list. Information of the queried module
#'
#' @export
#'
#' @examples
#' # Get the information of the ClinVar module
#' ov.module.info(module="clinvar")
#'
ov.module.info <- function(
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
  ret <- oakvar$ov_module_info(
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

#' ov.module.ls
#'
#' lists modules.
#'
#' @param pattern Regular expression for module names
#' @param available Include available modules
#' @param types Only list modules of certain types
#' @param include_hidden Include hidden modules
#' @param tags Only list modules of given tag(s)
#' @param nameonly Only list module names
#' @param raw_bytes Machine readable data sizes
#' @param md Specify the root directory of OakVar modules
#' @param fmt Output format. tabular or json
#' @param to stdout to print / return to return
#' @param quiet Run quietly
#'
#' @return A named list. List of modules
#'
#' @export
#'
#' @examples
#' # Get the list of all installed modules
#' ov.module.ls()
#' # Get the list of all available modules
#' ov.module.ls(available=TRUE)
#' # Get the list of all available modules of the type "converter"
#' ov.module.ls(available=TRUE, types="converter")
#'
ov.module.ls <- function(
  pattern=".*",
  available=FALSE,
  types=list(),
  include_hidden=FALSE,
  tags=list(),
  nameonly=FALSE,
  raw_bytes=FALSE,
  md=NULL,
  fmt="json",
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_module_ls(
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

#' ov.new.exampleinput
#'
#' @param directory Directory to make the example input file in
#' @param quiet Run quietly
#'
#' @return A string. Location of the example input file
#'
#' @export
#'
#' @examples
#' # Create an example input file in the current working directory
#' ov.new.exampleinput()
#' # Create an example input file at /home/user1/
#' ov.new.exampleinput(directory="/home/user1")
#'
ov.new.exampleinput <- function(
  directory=".",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_new_exampleinput(
    list(
      directory=directory,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.new.annotator
#'
#' @param annotator_name Annotator name
#' @param md Specify the root directory of OakVar modules
#' @param quiet No print to stdout
#'
#' @return A string. Location of the new annotator module
#'
#' @export
#'
#' @examples
#' # Create an annotator template at the OakVar modules directory/annotators/annotatortest
#' ov.new.annotator(annotator_name="annotatortest")
#'
ov.new.annotator <- function(
  annotator_name="exampleannotator",
  md=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_new_annotator(
    list(
      annotator_name=annotator_name,
      md=md,
      quiet=quiet
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
#' @param quiet Run quietly
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Publish "customannot" module to the store
#' ov.store.publish(module="customannot", user="user1", password="password")
#'
ov.store.publish <- function(
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
  ret <- oakvar$ov_store_publish(
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

#' ov.store.createaccount
#'
#' @param username use your email as your username.
#' @param password this is your password.
#' @param quiet Run quietly
#'
#' @return A string. Response from the store server
#'
#' @export
#'
#' @examples
#' # Create a store account
#' ov.store.newaccount(username="user1", password="password")
#'
ov.store.createaccount <- function(
  username=NULL,
  password=NULL,
  quiet=TRUE
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(password) || password == "") {
    print("password is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_store_createaccount(
    list(
      username=username,
      password=password,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.store.changepassword
#'
#' @param username username
#' @param current_password current password
#' @param new_password new password
#' @param quiet Run quietly
#'
#' @return A string. Response from the store server
#'
#' @export
#'
#' @examples
#' # Change the password of a store account
#' ov.store.changepassword(username="user1", current_password="password", new_password="newpassword")
#'
ov.store.changepassword <- function(
  username=NULL,
  current_password=NULL,
  new_password=NULL,
  quiet=TRUE
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
  ret <- oakvar$ov_store_changepassword(
    list(
      username=username,
      current_password=current_password,
      new_password=new_password,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.store.resetpassword
#'
#' @param quiet Run quietly
#' @param username username
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Ask the store to send an email to reset the password of a store account
#' ov.store.resetpassword(username="user1")
#'
ov.store.resetpassword <- function(
  quiet=TRUE,
  username=NULL
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_store_resetpassword(
    list(
      quiet=quiet,
      username=username
    )
  )
  return(ret)
}

#' ov.store.verifyemail
#'
#' @param username username
#' @param quiet Run quietly
#'
#' @return `NULL`
#'
#' @export
#'
#' @examples
#' # Ask the store to send an email to verify the email of a user account
#' ov.store.verifyemail(username="user1")
#'
ov.store.verifyemail <- function(
  username=NULL,
  quiet=TRUE
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_store_verifyemail(
    list(
      username=username,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.store.checklogin
#'
#' @param username username
#' @param password password
#' @param quiet Run quietly
#'
#' @return A boolean. A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Check if the login information of a user is correct
#' ov.store.checklogin(username="user1", password="password")
#'
ov.store.checklogin <- function(
  username=NULL,
  password=NULL,
  quiet=TRUE
) {
  if (is.null(username) || username == "") {
    print("username is required. returning.")
  }
  if (is.null(password) || password == "") {
    print("password is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_store_checklogin(
    list(
      username=username,
      password=password,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.util.test
#'
#' Test modules
#'
#' @param rundir Directory for output
#' @param modules Name of module(s) to test. (e.g. gnomad)
#' @param mod_types Type of module(s) to test (e.g. annotators)
#' @param to stdout to print / return to return
#' @param quiet Run quietly
#'
#' @return A named list. Field result is a named list showing the test result for each module. Fields num_passed and num_failed show the number of passed and failed modules.
#'
#' @export
#'
#' @examples
#' # Test the ClinVar module
#' ov.util.test(modules="clinvar")
#' # Test the ClinVar and the COSMIC modules
#' ov.util.test(modules=list("clinvar", "cosmic"))
#'
ov.util.test <- function(
  rundir=NULL,
  modules=NULL,
  mod_types=NULL,
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_util_test(
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

#' ov.util.addjob
#'
#' @param path Path to result database
#' @param user User who will own the job. Defaults to single user default user.
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Add a result file to the job list of a user
#' ov.util.addjob(path="example.sqlite", user="user1")
#'
ov.util.addjob <- function(
  path=NULL,
  user="default"
) {
  if (is.null(path) || path == "") {
    print("path is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_util_addjob(
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
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Merge two OakVar analysis result files into one SQLite file
#' ov.util.mergesqlite(path=list("example1.sqlite", "example2.sqlite"), outpath="merged.sqlite")
#'
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
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_util_mergesqlite(
    list(
      path=path,
      outpath=outpath
    )
  )
  return(ret)
}

#' ov.util.sqliteinfo
#'
#' @param paths SQLite result file paths
#' @param fmt Output format. text / json / yaml
#' @param to Output to. stdout / return
#'
#' @return A named list. Information of a job SQLite file
#'
#' @export
#'
#' @examples
#' # Get the named list of the information of an analysis result file
#' ov.util.sqliteinfo(paths="example.sqlite")
#'
ov.util.sqliteinfo <- function(
  paths=NULL,
  fmt="json",
  to="return"
) {
  if (is.null(paths) || paths == "") {
    print("paths is required. returning.")
  }
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_util_sqliteinfo(
    list(
      paths=paths,
      fmt=fmt,
      to=to
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
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Filter an analysis result file with an SQL filter set
#' ov.util.filtersqlite(paths="example.sqlite", filtersql='base__so=="MIS" and gnomad__af>0.01')# Filter two analysis result files with a filter definition file
#' ov.util.filtersqlite(paths=list("example1.sqlite", "example2.sqlite"), filterpath="filter.json")
#'
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
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_util_filtersqlite(
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

#' ov.system.setup
#'
#' Sets up OakVar system
#'
#' @param setup_file setup file to use
#' @param quiet Run quietly
#'
#' @return A boolean. TRUE if successful, FALSE if not
#'
#' @export
#'
#' @examples
#' # Set up OakVar with defaults
#' ov.system.setup()
#' # Set up OakVar with a setup file
#' ov.system.setup(setup_file="setup.yml")
#'
ov.system.setup <- function(
  setup_file=NULL,
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_system_setup(
    list(
      setup_file=setup_file,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.system.md
#'
#' displays or changes OakVar modules directory.
#'
#' @param directory sets modules directory.
#' @param to 'stdout' to print. 'return' to return.
#' @param quiet Run quietly
#'
#' @return A string. OakVar modules directory
#'
#' @export
#'
#' @examples
#' # Get the OakVar modules directory
#' ov.system.md()
#' # Set the OakVar modules directory to /home/user1/.oakvar/modules
#' ov.system.md(directory="/home/user1/.oakvar/modules")
#'
ov.system.md <- function(
  directory=NULL,
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_system_md(
    list(
      directory=directory,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

#' ov.system.config
#'
#' @param fmt Format of output. json or yaml.
#' @param to "stdout" to print. "return" to return
#' @param quiet Run quietly
#'
#' @return A named list. System config information
#'
#' @export
#'
#' @examples
#' # Get named list of the OakVar system configuration
#' ov.system.config()
#' # Get the OakVar system configuration in YAML text
#' ov.system.config(fmt="yaml")# Print to stdout the OakVar system configuration in YAML text
#' ov.system.config(fmt="yaml", to="stdout")
#'
ov.system.config <- function(
  fmt="json",
  to="return",
  quiet=TRUE
) {
  oakvar = reticulate::import("oakvar")
  ret <- oakvar$ov_system_config(
    list(
      fmt=fmt,
      to=to,
      quiet=quiet
    )
  )
  return(ret)
}

