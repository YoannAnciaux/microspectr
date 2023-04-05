#' Tidy excel raw data
#'
#' `import_excel` import raw data exported from *BMG MARS* into a tibble long
#' format with optionally signal names repaired.
#'
#' @param path Path to the xls/xlsx file.
#' @param name_repair A list of the following format `list('raw_name_signal1' =
#' 'new_name_signal1', 'raw_name_signal2' = 'new_name_signal2', ...)`. With the
#' 'raw_name_signal' corresponding to the signal name in *BMG MARS*. Default is
#' `NULL` and keep raw names from *BMG MARS*.
#' @details
#' In *BMG MARS* you must choose these formats for your excel export :
#' * hour format.
#' * 'wide' format, meaning that time must go horizontally not vertically.
#' * without 'metadata' before the table.
#' @returns A tibble with columns : Well, Content, (Group,) Signal, Time, Value.
#' Signal contains either raw names from *BMG MARS* or new names from `name_repair`.
#' @export
#' @examples
#' datasets <- microspectr_example("Ecoli_T7_24well_Lux_2Fluo_OD.xlsx")
#' import_excel(datasets)
#'
#' # With repaired names
#' signal_names <- list("No filter 1" = "LUX", "480-20 2" = "CFP", "600-40 3" = "SYTOX", "4" = "OD")
#' import_excel(datasets, name_repair = signal_names)
import_excel <- function(path,
                         name_repair = NULL) {
  data <- suppressMessages(read_excel(path))
  # Select column names which do not correspond to signal values
  discriminatory_variable <- select(data, !starts_with("Raw Data")) %>% colnames()
  # Extract column names of the signal associated with the time at which the signal has been measured
  tbl_name_time <- filter(data, grepl("Time", .data$Content)) %>%
    select(starts_with("Raw Data")) %>%
    pivot_longer(cols = everything(),
                 names_to = "Signal",
                 values_to = "Time")
  # Repair the colnames of the signals by either using the provided names or by keeping only what is in parenthesis
  if(is.list(name_repair)) {
    pattern <- paste0(".*(\\(", names(name_repair), "\\)).*") # pattern to recognize the names from name_repair between parentheses
    replacement <- unname(unlist(name_repair))
    tbl_name_time <- tbl_name_time %>%
      mutate(raw_Signal = .data$Signal,
             Signal = str_replace_all(.data$Signal, pattern = set_names(replacement, pattern)))
  } else {
    tbl_name_time <- tbl_name_time %>%
      mutate(raw_Signal = .data$Signal,
             Signal = gsub(".*\\((.*)\\).*", "\\1", .data$Signal)) # pattern to extract the names that are inside the parentheses
  }
  repaired_colnames <- tbl_name_time %>%
    unite(col = "Signal_Time", c("Signal","Time"))
  # repair the colnames of the tibble and reframe the tibble to be in long format with signal and time separated
  tidy_data <- data %>%
    rename_with(.fn = ~repaired_colnames$Signal_Time, .cols = repaired_colnames$raw_Signal) %>%
    filter(.data$Content != "Time [h]")  %>%
    pivot_longer(cols = !all_of(discriminatory_variable), names_to = "Signal_Time", values_to = "Value") %>%
    separate(col = c("Signal_Time"), into = c("Signal","Time"), sep = "_", convert = TRUE)

  return(tidy_data)
}

#' Get path to microspectr example
#'
#' microspectr comes bundled with some example files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' microspectr_example()
#' microspectr_example("Ecoli_T7_24well_Lux_2Fluo_OD.xlsx")
microspectr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "microspectr"))
  } else {
    system.file("extdata", path, package = "microspectr", mustWork = TRUE)
  }
}
