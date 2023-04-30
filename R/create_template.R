

#' Function used to generate RStudio project template
#'
#' Default settings should cover base level project
#' based on https://best-practice-and-impact.github.io/qa-of-code-guidance/intro.html
#' with optional changelog and unit testing framework for larger
#' projects
#' @keywords internal
#' @export
create_template <- function(path, ...) {

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  dots <- list(...)

  # setup file to capture output
  con <- file(file.path(path, "template_build.log"), open = "at")
  sink(con, append = TRUE, type = "output")
  sink(con, append = TRUE, type = "message")

  cli::cli_h1("Provisioning DHSC template")

  # set project path for usethis
  usethis::local_project(path, force = TRUE)

  # create a package compatible version of the project name
  # this is needed as most helper packages will try to
  # install the project and fail if the name is not compatible
  clean_path <- make.names(path, allow_ = FALSE)

  data <- list(
    project_name = path
  )

  # create skeleton first as otherwise functions prompt to overwrite
  cli::cli_h2("Creating file skeleton")

  # copy across template files and structure
  copy_folder("skeleton", ".", data)

  # create input directory
  usethis::use_directory("input", ignore = TRUE)
  copy_folder(file.path("skeleton", "input"), "input", data)

  # create output directory
  usethis::use_directory("output", ignore = TRUE)
  copy_folder(file.path("skeleton", "output"), "output", data)

  # create R directory
  usethis::use_directory("R")

  # add back in below when example code ready
  # if (dots$inc_example) {
  #   copy_folder(file.path("skeleton", "R"), "R", data)
  # }

  # ignore non-package compliant files/folders
  usethis::use_build_ignore(
    c("main.R", "README.Rmd", "template_build.log")
  )

  # add code of conduct
  usethis::use_template(
    template = "CODE_OF_CONDUCT.md",
    save_as = "CODE_OF_CONDUCT.md",
    data = list(contact = "datascience@dhsc.gov.uk"),
    package = "usethis"
  )
  usethis::use_build_ignore(c("CODE_OF_CONDUCT.md"))

  # add dummy package.R for documentation
  usethis::use_template(
    template = "packagename-package.R",
    save_as = file.path("R", paste0(clean_path, "-package.R")),
    package = "usethis"
  )

  # create nice project settings
  # similar to use_blank_slate and use_rstudio but also includes
  # newline and package building options
  usethis::use_template(
    template = "packagename.Rproj",
    save_as = file.path(paste0(path, ".Rproj")),
    package = "DHSCtools",
    ignore = TRUE
  )
  usethis::use_git_ignore(".Rproj.user")
  usethis::use_build_ignore(".Rproj.user")

  if (dots$inc_howto) {
    # copy across how to quarto document
    usethis::use_template(
      template = "template_howto.qmd",
      save_as = "template_howto.qmd",
      package = "DHSCtools",
      ignore = TRUE
    )

    usethis::use_build_ignore("^template_howto*", escape = FALSE)
  }

  cli::cli_text("...done")

  cli::cli_h2("Adding DESCRIPTION and LICENSE")
  # add a Description file using simplified author and specifying MIT license
  usethis::use_description(
    fields = list(
      Package = clean_path,
      `Authors@R` = 'person("First", "Last", , "email@dhsc.gov.uk", role = c("aut", "cre"))',
      License = "MIT + file LICENSE"
    ),
    check_name = FALSE,
    roxygen = TRUE
  )

  # GDS use the MIT licence for code so we will as well
  # see https://gds-way.cloudapps.digital/manuals/licensing.html#use-mit
  usethis::use_mit_license(
    copyright_holder = "Crown Copyright (Department of Health and Social Care)"
  )

  cli::cli_text("...done")

  # add a news file as a change log if required
  if (dots$inc_news) {
    cli::cli_h2("Adding NEWS.md for change log")
    usethis::use_news_md(open = FALSE)
    cli::cli_text("...done")
  }

  # add a testthat framework for unit test
  if (dots$inc_testthat) {
    cli::cli_h2("Adding unit testing framework")
    usethis::use_testthat()
    cli::cli_text("...done")
  }

  cli::cli_h2("Setting up pkgdown for html documentation")
  # set up package down to help create documentation
  # use with interactive to stop function trying to open yaml config
  rlang::with_interactive(
    usethis::use_pkgdown(),
    value = FALSE
  )

  # write_todo(
  #   "If using GitHub, run usethis::use_pkgdown_github_pages() to create pages"
  # )
  #
  # write_todo(
  #   "Run pkgdown::build_site() to build html documentation"
  # )

  cli::cli_text("...done")

  # initialise git for the project
  # needs to be done after the skeleton to prevent overwriting base .gitignore
  cli::cli_h2("Configuring git")
  init_git()

  cli::cli_text("...done")

  # cli::cli_h2("Cleaning up")
  # cli::cli_text("...done")

  # close all open sinks
  closeAllConnections()

}


#' Initialise git for the new project
#'
#' Replicate behaviour from usethis::use_git but without
#' adding to .gitignore (as part of template), asking to
#' commit (as performed after all files added), or trying to
#' restart R (as need to complete provisioning first)
#'
#' @keywords internal
init_git <- function() {

  repo <- tryCatch(
    gert::git_find(),
    error = function(e) NULL
  )

  if (is.null(repo)) {
    usethis::ui_done("Initialising Git repo")
    gert::git_init()
  }

  # get git config for project
  dat <- gert::git_config()
  username <- dat$value[tolower(dat$name) == tolower("user.name")]
  useremail <- dat$value[tolower(dat$name) == tolower("user.email")]

  # report on git settings
  usethis::ui_todo("Check Git config correct")
  usethis::ui_info(sprintf("Name: %s", username))
  usethis::ui_info(sprintf("Email: %s", useremail))

  # add in hook for readme
  usethis::use_git_hook(
    "pre-commit",
    readLines(
      system.file(
        "templates",
        "readme-rmd-pre-commit.sh",
        package = "usethis"
      ),
      encoding = "UTF-8",
      warn = FALSE
    )
  )

}


#' Copy across template files from specific folder
#'
#' Copies all files from within a folder of inst/templates into the
#' newly created project removing the first character of the name (needed so
#' that .gitignore does not function)
#'
#'@keywords internal
copy_folder <- function(path_template, path_file, data) {
  # note all files and directories are returned with the exception
  # of those starting with a .
  source_folder <-
    fs::path_package(
      "DHSCtools",
      "templates",
      path_template
    )

  file_names <- setdiff(
    list.files(source_folder, full.names = FALSE, recursive = FALSE),
    list.dirs(source_folder, full.names = FALSE, recursive = FALSE)
  )

  for (file_name in file_names) {
    print(file.path(path_template, file_name))

    # strip leading character off template file name
    usethis::use_template(
      template = file.path(path_template, file_name),
      save_as = file.path(path_file, substring(file_name, 2)),
      data = data,
      package = "DHSCtools"
    )
  }
}
