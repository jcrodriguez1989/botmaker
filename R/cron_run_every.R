#' Get cron-style Running Schedule
#'
#' Visit "https://crontab.guru/" to learn what cron schedules could be built.
#'
#' @param minutes Run every given minutes.
#' @param hours Run every given hours.
#' @param days Run every given days.
#' @param months Run every given months.
#'
#' @importFrom glue glue
#'
#' @export
#'
cron_run_every <- function(minutes = NULL, hours = NULL, days = NULL, months = NULL) {
  if (length(c(minutes, hours, days, months)) > 1) {
    stop("At most, one parameter can be provided.")
  }
  if (!is.null(months)) {
    glue("0 0 1 */{months} *")
  } else if (!is.null(days)) {
    glue("0 0 */{days} * *")
  } else if (!is.null(hours)) {
    glue("0 */{hours} * * *")
  } else if (!is.null(minutes)) {
    glue("*/{minutes} * * * *")
  } else {
    "* * * * *"
  }
}
