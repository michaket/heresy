#' Filter ANQ Psychiatry datasets by unit identifiers
#'
#' @description
#' Filters one or more ANQ Psychiatry datasets (*MB*, *MP*, *PH*, *PB*, *FM*)
#' by specific unit identifiers.
#' This helper function is designed for standardized ANQ data structures amended
#' with unit info (not part of regular ANQ reporting).
#' The identifying column for a psychiatric unit may vary in name
#' (e.g., `unit_code`, `unit_id`, or `unitid`).
#'
#' @details
#' You can pass any combination of the datasets `MB`, `MP`, `PH`, `PB`, and `FM`.
#' Only non-`NULL` data frames will be processed.
#' Each data frame must contain a column that identifies the unit (specified by `unit_col`).
#'
#' The function supports two filtering modes:
#' * `keep = TRUE` (default): keep only rows where the unit column matches `unit_ids`.
#' * `keep = FALSE`: exclude rows where the unit column matches `unit_ids`.
#'
#' The returned list contains only the filtered data frames, with names suffixed
#' by the string provided via `suffix` (default: `"_filtered"`).
#'
#' @param MB,MP,PH,PB,FM Data frames containing ANQ Psychiatry data.
#'   Each corresponds to a specific dataset type (e.g., MB = main baseline, MP = main post, etc.).
#'   You may provide any subset of these arguments.
#' @param unit_ids A vector of unit identifiers to keep or remove.
#' @param unit_col A bare (unquoted) column name identifying the unit variable
#'   (e.g., `unit_code`, `unit_id`, or `unitid`). Defaults to `unit_code`.
#' @param keep Logical. If `TRUE`, keep only rows with matching unit identifiers;
#'   if `FALSE`, remove them. Defaults to `TRUE`.
#' @param suffix Character string appended to the names of the returned data frames.
#'   Defaults to `"_filtered"`.
#'
#' @return
#' A named list of filtered data frames, each corresponding to one of the datasets
#' provided as input. The list elements are named using the original dataset names
#' plus the provided `suffix`.
#'
#' @examples
#' \dontrun{
#' # Filter MB and MP for specific units
#' filtered <- filter_by_unit_id(
#'   MB = MB,
#'   MP = MP,
#'   unit_ids = c("A1", "B2"),
#'   unit_col = unit_code,
#'   keep = TRUE,
#'   suffix = "_subset"
#' )
#'
#' names(filtered)
#' #> [1] "MB_subset" "MP_subset"
#'
#' # Filter using a different unit column name
#' filtered <- filter_by_unit_id(
#'   PH = PH,
#'   PB = PB,
#'   unit_ids = c(101, 103),
#'   unit_col = unit_id
#' )
#' }
#'
#' @export
filter_by_unit_id <- function(
    MB = NULL,
    MP = NULL,
    PH = NULL,
    PB = NULL,
    FM = NULL,
    unit_ids,
    unit_col = unit_code,
    keep = TRUE,
    suffix = "_filtered"
) {
  # Validation
  if (!is.vector(unit_ids)) {
    stop("`unit_ids` must be a vector of valid unit identifiers.")
  }

  if (!is.logical(keep) || length(keep) != 1) {
    stop("`keep` must be a single logical value (TRUE or FALSE).")
  }

  if (!is.character(suffix) || length(suffix) != 1) {
    stop("`suffix` must be a single string.")
  }

  # Collect only the provided (non-null) data frames
  dfs <- list(MB = MB, MP = MP, PH = PH, PB = PB, FM = FM)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]

  if (length(dfs) == 0) {
    stop("At least one data frame (MB, MP, PH, PB, FM) must be provided.")
  }

  # Capture the column name
  unit_col_name <- rlang::as_name(rlang::enquo(unit_col))

  # Apply filtering
  filtered_dfs <- purrr::imap(dfs, \(df, name) {
    if (!unit_col_name %in% names(df)) {
      stop(paste0("Data frame `", name, "` does not contain a column named `", unit_col_name, "`."))
    }

    if (keep) {
      dplyr::filter(df, .data[[unit_col_name]] %in% unit_ids)
    } else {
      dplyr::filter(df, !.data[[unit_col_name]] %in% unit_ids)
    }
  })

  # Add configurable suffix to names
  names(filtered_dfs) <- paste0(names(filtered_dfs), suffix)

  return(filtered_dfs)
}
