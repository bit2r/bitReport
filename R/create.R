#' Create a Distill website
#'
#' Create a basic skeleton for a Distill website or blog. Use the `create_website()`
#' function for a website and the `create_blog()` function for a blog.
#'
#' @param dir Directory for website
#' @param title Title of website
#' @param gh_pages Configure the site for publishing using [GitHub
#'   Pages](https://pages.github.com/)
#' @param edit Open site index file or welcome post in an editor.
#'
#' @note The `dir` and `title` parameters are required (they will be prompted for
#'   interactively if they are not specified).
#'
#' @examples
#' \dontrun{
#' library(bitReport)
#' 
#' create_website("mysite", "My Site")
#' }
#' @export
#' 
create_website <- function(dir, title, gh_pages = FALSE, edit = interactive()) {
  params <- do_create_website(dir, title, gh_pages, edit, "website")
  render_website(params$dir, "website")
  
  invisible(NULL)
}

render_website <- function (file, type, target_path, data = list()) 
{
  message("Creating ", file.path(target_path, file))
  template <- system.file(file.path("website", type, file), package = "bitReport")
  template <- paste(readLines(template, encoding = "UTF-8"), 
                    collapse = "\n")
  output <- whisker::whisker.render(template, data)
  if (!dir_exists(target_path)) 
    dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
  writeLines(output, file.path(target_path, file), useBytes = TRUE)
}

do_create_website <- function(dir, title, gh_pages, edit, type) {
  
  # prompt for arguments if we need to
  if (missing(dir)) {
    if (interactive())
      dir <- readline(sprintf("Enter directory name for %s: ", type))
    else
      stop("dir argument must be specified", call. = FALSE)
  }
  if (missing(title)) {
    if (interactive())
      title <- readline(sprintf("Enter a title for the %s: ", type))
    else
      stop("title argument must be specified", call. = FALSE)
  }
  
  # ensure dir exists
  message("Creating website directory ", dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  # copy template files
  render_website_template <- function(file, data = list()) {
    render_website(file, type, dir, data)
  }
  render_website_template("_site.yml", data = list(
    name = basename(dir),
    title = title,
    output_dir = if (gh_pages) "docs" else "_site"
  ))
  render_website_template("index.Rmd", data = list(title = title, gh_pages = gh_pages))
  render_website_template("about.Rmd")
  render_website_template("theme.css")
  render_website_template("footer.html")  
  
  # if this is for gh-pages then create .nojekyll
  if (gh_pages) {
    nojekyll <- file.path(dir, ".nojekyll")
    message("Creating ", nojekyll, " for gh-pages")
    file.create(nojekyll)
  }
  
  # if we are running in RStudio then create Rproj
  if (have_rstudio_project_api())
    rstudioapi::initializeProject(dir)
  
  if (edit)
    edit_file(file.path(dir, "index.Rmd"))
  
  invisible(list(
    dir = dir,
    title = title
  ))
}


