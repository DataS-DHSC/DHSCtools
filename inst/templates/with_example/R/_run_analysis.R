
# Load required packages --------------------------------------------------
library(tidyverse)


# Source other scripts ----------------------------------------------------
download <- new.env(parent = baseenv()); source("./R/download.R", local = download)
process <- new.env(parent = baseenv()); source("./R/process.R", local = process)
visualise <- new.env(parent = baseenv()); source("./R/visualise.R", local = visualise)


# Main function to run analysis -------------------------------------------

#' Run analysis
#'
#' Main function used to run analysis. This function is called from
#' the main.R script which should be used to run the project.
#'
#' @export
#'
run_analysis <- function() {
  # log action
  logger$info("Running analysis...")

  # read in configuration
  config_path <- file.path("input", "config.yml")
  config <- yaml::read_yaml(config_path)

  # get file date and time stamp
  config$date_stamp <- format(Sys.time(), "%Y%m%d-%H%M")

  # download summary file
  attendance_data <-
    download$download_nhsd_data(
      config$source_url,
      config$summary$regex,
      file.path(
        "input",
        sprintf("%s_%s", config$date_stamp, config$summary$filename)
      )
    ) %>%
    process$load_attendance_data()

  visualise$save_plot(
    visualise$plot_attendance_proportions(attendance_data),
    file.path(
      "output",
      sprintf("%s_%s", config$date_stamp, "attendance_plot.svg")
    )
  )

  # download summary file
  nims_data <-
    download$download_nhsd_data(
      config$source_url,
      config$nims$regex,
      file.path(
        "input",
        sprintf("%s_%s", config$date_stamp, config$nims$filename)
      )
    ) %>%
    process$load_nims_data()

  # pick region of interest and only select full years
  nims_monthly_eng <-
    process$get_monthly_totals(
      nims_data %>% filter(area_name == "England")
    ) %>%
    group_by(year) %>%
    filter(n() == 12) %>%
    ungroup()

  visualise$save_plot(
    visualise$plot_nims_monthly(nims_monthly_eng),
    file.path(
      "output",
      sprintf("%s_%s", config$date_stamp, "nims_monthly_eng.svg")
    )
  )

  # NIMS data uses old ICB codes so need to update to new 2023
  # codes
  nims_icb_data <- nims_data %>%
    filter(
      area_type == "ICB",
      date == as.Date("2023-01-01")
    ) %>%
    mutate(
      ons_code = case_when(
        ons_code == "E54000053" ~ "E54000064",
        ons_code == "E54000052" ~ "E54000063",
        TRUE ~ ons_code
      )
    ) %>%
    select(ons_code, total)


  visualise$save_plot(
    visualise$plot_nims_icb_map(nims_icb_data),
    file.path(
      "output",
      sprintf("%s_%s", config$date_stamp, "nims_icb_map_20230101.svg")
    )
  )

  # copy config to output
  fs::file_copy(
    file.path("input", "config.yml"),
    file.path(
      "output",
      sprintf("%s_config.yml", config$date_stamp)
    ),
    overwrite = TRUE
  )

  logger$info("Finished")
}

