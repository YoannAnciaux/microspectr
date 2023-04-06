#' Rescale the signal value by a reference
#'
#' `rescale_data` Divide each 'Signal' by a avlue at a given time of the ame Signal,
#' either from a shared reference = the controls or a reference specific to each well.
#'
#' @param data A data frame or a tibble with at least a column 'Time', 'Signal',
#' 'Content' and 'Value'.
#' @param value_name A string corresponding to the name of the column containing
#' the value to be rescaled. Default is the output of [correct_blank] 'Value_BC'
#' @param rescale_from_well A named vector c('name_Signal1' = TimeForReferenceSignal1,
#' 'name_Signal2' = TimeForReferenceSignal2, ...). TimeForReferenceSignalX is a time
#' existing in the column 'Time' at which the value of SignalX will be used as a
#' reference for rescaling. Default `NULL` leads to no rescaling by well.
#' @param rescale_from_control A named vector c('name_Signal1' = TimeForReferenceSignal1inControls,
#' 'name_Signal2' = TimeForReferenceSignal2inControls, ...). TimeForReferenceSignalinControlsX
#' is a time existing in the column 'Time' at which the mean value of SignalX in
#' the controls will be used as a reference for rescaling. Default `NULL` leads
#' to no rescaling by controls.
#' @param control_name A string corresponding to the name of the controls in 'Content'.
#' Ony used if `rescale_from_control` is not `NULL`. Default "Positive control P".
#' @param by_Group A boolean. If `TRUE` the rescaling from the control is done by
#' 'Group'. (Controls averaged by group)
#' @details
#' * If both `rescale_from_well` and `rescale_from_control` are `NULL` returns `NA`
#' in 'Value_Rescaled'.
#' * Signals mentioned neither in `rescale_from_well` nor `rescale_from_control`
#' are `NA` in 'Value_Rescaled'
#' @returns A tibble with with same column as in data plus a column Value_Rescaled
#' @export
#' @examples
#' data <- Ecoli_T7_24well_Lux_2Fluo_OD
#' rescale_from_well <- c("LUX" = 0, "OD" = 0, "SYTOX" = 5)
#' rescale_from_control <- c("CFP" = 10)
#' rescale_data(data, "Value", rescale_from_well, rescale_from_control,
#'              control_name = "Positive control P",  by_Group = TRUE)
rescale_data <- function(data,
                         value_name = "Value_BC",
                         rescale_from_well = NULL,
                         rescale_from_control = NULL,
                         control_name = "Positive control P",
                         by_Group = FALSE) {

  data <- data %>%
    mutate(Value_Rescaled = as.double(NA))

  if(!is.null(rescale_from_well)) {
    if(by_Group) data <- data %>% group_by(.data$Group)
    data <- data %>%
      group_by(.data$Signal, .data$Well) %>%
      mutate(Value_Rescaled = pmap_dbl(list(.data$Signal, !!sym(value_name), .data$Value_Rescaled), ~ifelse(..1 %in% names(rescale_from_well),
                                                                                                ..2 / (!!sym(value_name))[Time == rescale_from_well[..1]], ..3))) %>%
      ungroup()
  }

  if(!is.null(rescale_from_control)) {
    if(by_Group) data <- data %>% group_by(.data$Group)
    data <- data %>%
      group_by(.data$Signal) %>%
      mutate(Value_Rescaled = pmap_dbl(list(.data$Signal, !!sym(value_name), .data$Value_Rescaled), ~ifelse(..1 %in% names(rescale_from_control),
                                                                                                ..2 / mean((!!sym(value_name))[Time == rescale_from_control[..1] & Content == control_name]), ..3))) %>%
      ungroup()
  }

  return(data)
}
