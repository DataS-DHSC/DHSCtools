
# Load required packages --------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggrepel)
library(scales)
library(sf)
library(svglite)


# Function definitions ----------------------------------------------------

#' Display and save a plot
#'
#' @param plt Plot to be saved.
#' @param file_path Path to save plot to.
#'
#' @return Path plot saved to.
#' @export
#'
save_plot <- function(plt, file_path) {

  logger$info("Saving plot to %s", file_path)

  print(plt)

  ggsave(
    plt,
    dpi = 300, width = 12, height = 8, units = "in",
    filename = file_path
  )

  return(file_path)
}


#' Create attendance plot
#'
#' Create a line graph of proportion of attendances by type.
#'
#' @param attendance_data Attendance data to plot.
#'
#' @return Plot of data.
#' @export
#'
plot_attendance_proportions <- function(attendance_data) {

  logger$info("Plotting attendance data")

  df <- attendance_data %>%
    mutate(
      attended = attended / appointments,
      dna = dna / appointments,
      unknown = unk / appointments
    ) %>%
    select(date, attended, dna, unknown) %>%
    pivot_longer(
      c(attended, dna, unknown),
      names_to = "type",
      values_to = "proportion"
    )

  # want weekly separator lines
  v_ticks <- seq(min(df$date), max(df$date), by = "1 week")

  # function to display the weekday as a single character with
  # the day of the month at each week start
  format_dates <- function(x) {
    day_of_week <- strftime(x, format = "%a") %>%
      str_sub(1, 1)
    day_of_month <- strftime(x, format = "%d")
    week_start = wday(min(df$date))
    return(
      if_else(
        wday(x) == week_start,  # Conditions for pasting.
        true = paste(day_of_week, day_of_month, sep = "\n"),
        false = day_of_week)
    )
  }

  plt <- ggplot() +
    DHSCcolours::theme_dhsc() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    geom_line(
      data = df,
      aes(date, proportion, colour = type),
      linewidth = 1
    ) +
    DHSCcolours::scale_colour_dhsc_d() +
    scale_y_continuous(labels=scales::percent) +
    scale_x_date(
      name = "Date",
      labels = format_dates,
      date_breaks = "1 day"
    ) +
    geom_vline(
      xintercept = v_ticks,
      linetype="dotted",
      colour = DHSCcolours::dhsc_grey()
    ) +
    labs(
      title = "Proportion of GP attendances by type",
      subtitle = sprintf("England, %s", format(min(df$date), "%b %Y"))
    )

  return(plt)

}

#' Create NIMS monthly plot
#'
#' Create a line graph of number of monthly NIMS attendances by year.
#'
#' @param nims_monthly_data NIMS monthly data to plot.
#'
#' @return Plot of data.
#' @export
#'
plot_nims_monthly <- function(nims_monthly_data) {

  logger$info("Plotting monthly NIMS data")

  df <- nims_monthly_data %>%
    mutate(
      year = factor(year),
      month = factor(month.abb[month], levels = month.abb, ordered = TRUE)
    )

  plt <- ggplot() +
    DHSCcolours::theme_dhsc() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    ) +
    geom_line(
      data = df,
      aes(month, total, colour = year, group = year),
      linewidth = 1,
      show.legend = FALSE
    ) +
    scale_y_continuous(
      n.breaks = 4,
      labels = scales::label_number(
        accurancy = 0.1,
        scale = 1e-6,
        suffix = "m"
      ),
      limits = c(0, NA),
      expand = c(0, NA)
    ) +
    DHSCcolours::scale_colour_dhsc_d() +
    labs(
      title = "Number of NIMS GP attendances",
      subtitle = "England",
      x = "Month"
    )

  # add data labels at edge of graph
  plt <- plt +
    geom_text_repel(
      data = subset(df, month == "Dec"),
      aes(month, total, label = year),
      direction = "y",
      xlim = c(12, NA)
    )

  return(plt)
}


#' Create NIMS ICB map
#'
#' Create a choropleth of number of NIMS attendances.
#'
#' @param nims_icb_data NIMS ICB data to plot.
#'
#' @return Plot of data.
#' @export
#'
plot_nims_icb_map <- function(nims_icb_data) {
  # EXTRA SCRIPTS/FUNCTIONS NEEDED FOR FULL DHSC STYLE
  shape_icb <-
    read_sf(
      file.path(
        "input",
        "ICB_April_2023_BSC",
        "ICB_APR_2023_EN_BSC.shp"
      )
    )

  df <- shape_icb %>%
    left_join(
      nims_icb_data,
      by = c(ICB23CD = "ons_code")
    )

  # maps require a lot of manual adjust to get spacing looking good
  plt <- ggplot() +
    geom_sf(
      data = df,
      aes(fill = total),
      colour = "black",
      size = 0.1
    ) +
    scale_fill_gradient(
      low = DHSCcolours::dhsc_white(),
      high = DHSCcolours::dhsc_dark_green()
    ) +
    labs(
      title = str_wrap("NIMs attendances by ICB", width = 80),
      subtitle = str_wrap("1st Jan 2023", width = 80),
      fill = str_wrap("Number of attendances", width = 25),
      caption = str_wrap(
        "Source: NHSD - Appointments in General Practice, January 2023",
        width = 80
      )
    ) +
    theme_void(base_size = 18, base_family = "sans") +
    theme(
      legend.position = c(0.84, 0.93),
      plot.margin = margin(0, 10, 10, 10),
      plot.title = element_text(face = "bold"),
      plot.title.position = "plot"
    )

}
