
# Load required packages --------------------------------------------------
library(tidyverse)


# Function definitions ----------------------------------------------------

#' Download data from NHSD website
#'
#' Function scrapes an NHSD website for a file with a displayed name
#' that matches a regular expression and then downloads it.
#'
#' @param source_url The URL of the NHSD web page.
#' @param file_regex A regular expression string to match the displayed name.
#' @param file_path The path to download the file to.
#'
#' @return The path to the downloaded file.
#'
#' @export
#'
download_nhsd_data <- function(source_url, file_regex, file_path) {
  # config DHSC proxies settings - not strictly necessary in this instance
  DHSCtools::configure_curl_proxy()

  # get the list of links
  file_links <- scrape_nhsd_links(source_url)

  # create raw output directory
  download_folder <- fs::dir_create(file.path("input", "raw"))

  # get link to first file matching regular expression
  file_url <- file_links %>%
    filter(str_detect(link_name, file_regex)) %>%
    pull(url) %>%
    first()

  # download raw file
  raw_path <- download_file(file_url, download_folder)

  fs::file_copy(raw_path, file_path)

  return(file_path)
}

#' Extract file links from an NHSE webpage
#'
#' Code looks for file links in the "nhsd-m-download-card"
#' class div of the webpage.
#'
#' @param url The URL of the NHSD page to scrape.
#'
#' @return A tibble containing displayed file names and links.
#'
#' @noRd
scrape_nhsd_links <- function(url) {
  logger$info("Scraping page %s", url)

  # use the polite package to scrape the webpage
  html <- polite::bow(url = url) %>%
    polite::scrape()

  # get the relevant elements based on class
  html_files <- html %>%
    rvest::html_elements("div.nhsd-m-download-card")

  # extract the file link names and urls
  df <-
    tibble(
      link_name = html_files %>%
        rvest::html_element("p.nhsd-t-heading-xs") %>%
        rvest::html_text(trim = TRUE),
      url = html_files %>%
        rvest::html_element("a") %>%
        rvest::html_attr("href")
    ) %>%
    filter(!is.na(link_name))

  return(df)
}


#' Download a file from a URL
#'
#' Downloads a file from a URL to a folder keeping the same
#' file name. Checks the extension on the file being downloaded
#' to correctly handle download mode.
#'
#' @param file_url URL of file to be downloaded.
#' @param folder_path The path of folder to save file to.
#'
#' @return The output path the file was downloaded to.
#'
#' @noRd
download_file <- function(file_url, folder_path) {
  # GOTCHA - check extension of file and if not text use binary mode
  if (tools::file_ext(file_url) %in% c("txt", "csv", "tsv")) {
    download_mode = "w"
  } else {
    download_mode = "wb"
  }

  output_path <- file.path(folder_path, basename(file_url))

  logger$info("Downloading %s to %s", file_url, output_path)

  download.file(
    file_url,
    output_path,
    mode = download_mode,
    quiet = TRUE
  )

  return(output_path)
}
