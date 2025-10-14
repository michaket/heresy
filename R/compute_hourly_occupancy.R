#' Compute hourly patient occupancy for psychiatry units
#'
#' @description
#' This function generates an hourly time sequence for the representative year
#' of the provided MB dataset and computes hourly patient occupancy for each
#' psychiatric unit.
#'
#' The function first checks whether the dataset primarily represents a single
#' year, based on the values in `admission_col`, while allowing for minor
#' deviations or outliers. If the 10% and 90% year quantiles and median are
#' identical, the function proceeds with that year; otherwise, it also proceeds
#' but issues a warning message.
#'
#' It then generates an hourly sequence for the identified year, performs a
#' cross join with the distinct unit codes, and calculates the number of
#' patients present in each unit at every hour of the year.
#'
#' @param data A data frame containing psychiatric admission and discharge data,
#' typically this would be the MB data.
#' @param unit_col The name of the column identifying units (default: `unit_code`).
#' @param admission_col The name of the admission date-time column (default: `admission`).
#' @param discharge_col The name of the discharge date-time column (default: `discharge`).
#' @param tz Time zone to use for generating hourly timestamps (default: `"UTC"`).
#'
#' @return
#' A tibble with columns:
#'
#' * `unit_code` (or the column provided in `unit_col`),
#' * `Hour` - hourly timestamps for the representative year,
#' * `n_patients` - number of patients in each unit at that hour.
#'
#' @examples
#' \dontrun{
#' occupancy_tbl <- compute_hourly_occupancy(dat$MB_filtered)
#' head(occupancy_tbl)
#' }
#'
#' @export
compute_hourly_occupancy <- function(
    data,
    unit_col = unit_code,
    admission_col = admission,
    discharge_col = discharge,
    tz = "UTC"
) {
  # --- Validation ------------------------------------------------------------
  if (missing(data)) {
    stop("`data` must be provided.")
  }

  # Capture column names
  unit_col_name <- rlang::as_name(rlang::enquo(unit_col))
  admission_col_name <- rlang::as_name(rlang::enquo(admission_col))
  discharge_col_name <- rlang::as_name(rlang::enquo(discharge_col))

  required_cols <- c(unit_col_name, admission_col_name, discharge_col_name)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing in `data`: ",
         paste(missing_cols, collapse = ", "))
  }

  # --- Step 1: Determine representative year ---------------------------------
  year_values <- lubridate::year(data[[admission_col_name]])

  year_median <- median(year_values, na.rm = TRUE)
  year_quants <- quantile(year_values, probs = c(0.1, 0.9), na.rm = TRUE)
  all_same <- length(unique(round(year_quants))) == 1

  if (all_same) {
    year_of_interest <- round(year_median)
    message("Everything is ok: the data represent year ", year_of_interest, ".")
  } else {
    year_of_interest <- round(year_median)
    message("Does the data really contain data from just one year?")
  }

  # --- Step 2: Generate hourly timeline --------------------------------------
  hours_tbl <- tibble::tibble(
    Hour = seq.POSIXt(
      from = as.POSIXct(paste0(year_of_interest, "-01-01 00:00"), tz = tz),
      to   = as.POSIXct(paste0(year_of_interest, "-12-31 23:00"), tz = tz),
      by   = "hour"
    )
  )

  # --- Step 3: Cross join units × hours --------------------------------------
  units_tbl <- data |>
    dplyr::select({{ unit_col }}) |>
    dplyr::distinct()

  expanded_tbl <- tidyr::cross_join(units_tbl, hours_tbl)

  # --- Step 4: Compute hourly patient occupancy ------------------------------
  result <- expanded_tbl |>
    dplyr::mutate(
      n_patients = purrr::map2_int(
        Hour, {{ unit_col }},
        ~ sum(
          data[[admission_col_name]] < .x &
            data[[discharge_col_name]] > .x &
            data[[unit_col_name]] == .y,
          na.rm = TRUE
        )
      )
    )

  return(result)
}
