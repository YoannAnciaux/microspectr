#' Correct luminescence crosstalk in microplate
#'
#' `correct_crosstalk_lux` correct luminescence crosstalk/bleed-through between
#' wells based on a calibration for the given microplate model.
#'
#' @param data A data frame or a tibble with at least a column 'Time' and a column
#' 'Value'.
#' @param crosstalk_lux_ref A list with two elements `list(ct_matrix = 'matrix of
#' crosstalk', blank = 'value of the blank')`.
#' @param lux_name String with the name of the luminescence signal in the 'Signal'
#' column when providing a data with multiple signals. Default is NULL and will
#' consider that only the luminescent 'Value' are provided.
#' @details
#' * 'Value' must be raw and not blank corrected.
#' * The ct_matrix and the blank value must be obtained from a calibration for
#' the given microplate model.
#' * The number of 'Value' entry for each 'Time' entry must be equal to the number
#' of lines and wells of the ct_matrix.
#' @returns A tibble with at least a column 'Time' and a column 'Value' with crosstalk
#' corrected luminescence value (and Signal if lux_name is provided). Other columns
#' are returned unmodified.
#' @export
#' @examples
#' data <- Ecoli_T7_24well_Lux_2Fluo_OD
#' crosstalk_lux_ref <- ct_calibration_24wells_4titude_black_vision
#' correct_crosstalk_lux(data, crosstalk_lux_ref, "LUX")
correct_crosstalk_lux <- function(data,
                                  crosstalk_lux_ref,
                                  lux_name = NULL) {
lux_name = "LUX"
  blank     <- crosstalk_lux_ref$blank
  ct_matrix <- crosstalk_lux_ref$ct_matrix

  if (!is.null(lux_name)) {
    data_tmp <- data %>%
      filter(.data$Signal == lux_name) %>%
      select(.data$Time, .data$Value)
  } else {
    data_tmp <- data
  }

  data_corrected_lux <- data %>%
    filter(.data$Signal == lux_name) %>%
    group_by(.data$Time) %>%
    mutate(Value = as.double(solve(diag(dim(ct_matrix)[1]) + ct_matrix) %*% .data$Value-blank) + blank) %>%
    ungroup()


  if (!is.null(lux_name)) {
    data_corrected_lux <- data_corrected_lux %>%
      full_join(filter(data, .data$Signal != lux_name))
  }

  return(data_corrected_lux)
}
