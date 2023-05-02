
# Load required packages --------------------------------------------------
library(tidyverse)
library(tidyxl)
library(unpivotr)
library(writexl)
library(lubridate)


# Function definitions ----------------------------------------------------

#' Read and tidy attendance data from summary spreadsheet
#'
#' Function reads in data from Table 2a of the spreadsheet.
#'
#' @param file_path The file path to the summary data file.
#' Should include the file extension (.xlsx)
#'
#' @return A tibble with the sheet data.
#' @export
#'
load_attendance_data <- function(file_path) {

  # Reading excel data with tidyxl and manipulating the output with unpivotr and dplyr
  logger$info("Reading table 2a from %s", file_path)

  # read spreadsheet as cells using tidyxl
  df_raw <- xlsx_cells(
    file_path, sheets = "Table 2a",
    include_blank_cells = FALSE
  )

  # get row of table title
  title_row <- df_raw %>%
    filter(
      str_detect(character, "^Table 2a")
    ) %>%
    pull(row) %>%
    first()

  # get row of table footer
  footer_row <- df_raw %>%
    filter(
      str_detect(character, "^Source: NHS England$")
    ) %>%
    pull(row) %>%
    first()

  # use unpivotr to tidy data
  # start with headers
  df <- df_raw %>%
    filter(row > title_row, row < footer_row) %>%
    behead('up', super_heading) %>%
    behead('up', heading) %>%
    behead('up', hidden_row)

  # sort out headings and weekdays
  df <- df %>%
    arrange(col, row) %>%
    fill(heading, .direction = "down") %>%
    behead('left', weekday)

  # convert data to tibble
  df <- df %>%
    select(row, data_type, heading, character, numeric, date) %>%
    spatter(heading)

  # enforce data types and column names
  df <- df %>%
    mutate(
      date = as.Date(Date),
      appointments = as.numeric(`Total Count of Appointments`),
      attended = as.numeric(Attended),
      dna = as.numeric(`Did Not Attend`),
      unk = as.numeric(Unknown1)
    ) %>%
    select(date, appointments, attended, dna, unk)

  return(df)
}



#' Read and tidy nims data from csv
#'
#' @param file_path The path to the csv file to read.
#' Should include the file extension (.csv)
#'
#' @return A tibble with the data.
#' @export
#'
load_nims_data <- function(file_path) {

  logger$info("Reading file %s ", file_path)

  # read in data as characters
  df_raw <- read_csv(
    file_path,
    col_types = cols(.default = "c")
  )

  # enforce data types and column names
  df <- df_raw %>%
    mutate(
      date = as_date(parse_date_time(Date, "my")),
      area_type = Type,
      nhs_code = `NHS Area Code`,
      ons_code = `ONS Code`,
      area_name = Name,
      total = as.numeric(Total)
    ) %>%
    select(date, area_type, nhs_code, ons_code, area_name, total)

  return(df)
}

#' Summarise nims data by calculating monthly totals
#'
#' Convenience function to summarise nims data by month
#'
#' @param df_nims A tibble of nims data.
#'
#' @return A tibble giving the same data aggregated by month.
#' @export
#'
get_monthly_totals <- function(df_nims) {

  logger$info("Creating monthly totals")

  df <- df_nims %>%
    mutate(
      month = month(date),
      year = year(date)
    ) %>%
    group_by(year, month) %>%
    summarise(total = sum(total), .groups = "drop") %>%
    select(month, year, total)

  return(df)
}
