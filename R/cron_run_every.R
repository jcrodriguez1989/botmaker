#' Get cron-style Running Schedule
#'
#' Visit "https://crontab.guru/" to learn what cron schedules could be built.
#'
#' @param minutes Run every given minutes.
#' @param hours Run every given hours.
#' @param days Run every given days.
#' @param months Run every given months.
#'
#' @export
#'
cron_run_every <- function(minutes = NULL, hours = NULL, days = NULL, months = NULL) {
  minutes <- ifelse(is.null(minutes), "*", paste0("*/", minutes))
  hours <- ifelse(is.null(hours), "*", paste0("*/", hours))
  days <- ifelse(is.null(days), "*", paste0("*/", days))
  months <- ifelse(is.null(months), "*", paste0("*/", months))
  paste(minutes, hours, days, months, "*")
}
