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


#' Import, correct and rescale data from *BMG Clariostar*
#'
#' `tidy_data` Is a wrapper for all the functions in 'microspectr' package. It
#' creates a streamline workflow to process a raw excel export froæ *BMG MARS* to
#' data whcih can be directly fitted.
#'
#' @inheritParams import_excel
#' @inheritParams correct_crosstalk_lux
#' @inheritParams correct_blank
#' @inheritParams rescale_data
#' @details
#' Particularities of the default values :
#' * Default `crosstalk_lux_ref` is `NULL` which skips crosstalk correction.
#' * Default `blank_name`is `NULL` which skips blank correction.
#' * if both `rescale_from_well` and `rescale_from_control` are `NULL` it skips
#' the rescaling
#'
#' @returns A tibble with columns as in the initial excel plus 'Signal', 'Time',
#' 'Value', 'Blank', 'Value_BC', 'Value_Rescaled'.
#' @export
#' @examples
#' path <- microspectr_example("Ecoli_T7_24well_Lux_2Fluo_OD.xlsx")
#' name_repair <- list("No filter 1" = "LUX", "480-20 2" = "CFP", "600-40 3" = "SYTOX", "4" = "OD")
#' crosstalk_lux_ref <- ct_calibration_24wells_4titude_black_vision
#' rescale_from_well <- c("LUX" = 0, "OD" = 0, "SYTOX" = 5)
#' rescale_from_control <- c("CFP" = 10)
#' tidy_data(path = path, name_repair = name_repair,
#'           lux_name = "LUX", blank_name = "Blank B",
#'           rescale_from_well = rescale_from_well, rescale_from_control = rescale_from_control,
#'           control_name = "Positive control P", by_Group = TRUE)

tidy_data <- function(path,
                      name_repair = NULL,
                      crosstalk_lux_ref = NULL,
                      lux_name = NULL,
                      blank_name = NULL,
                      rescale_from_well = NULL,
                      rescale_from_control = NULL,
                      control_name = "Positive control P",
                      by_Group = F) {
  #### Import data ####
  print("Importing data ...")
  data <- import_excel(path = path, name_repair = name_repair)

  #### Crosstalk correction luminescence ####
  if(!is.null(crosstalk_lux_ref)) {
    print("Crosstalk correction ...")
    data <- correct_crosstalk_lux(data = data, crosstalk_lux_ref = crosstalk_lux_ref, lux_name = lux_name)
  }

  #### Blank correction and rescaling ####
  if(!is.null(blank_name)) {
    #### Blank correction  ####
    print("Blank correction ...")
    data <- correct_blank(data, blank_name = blank_name, by_Group = by_Group)

    #### Rescaling ####
    if((!is.null(rescale_from_well))|(!is.null(rescale_from_control))) {
      print("Rescaling ...")
      data <- rescale_data(data, value_name = "Value_BC",
                           rescale_from_well = rescale_from_well,
                           rescale_from_control = rescale_from_control,
                           control_name = control_name,
                           by_Group = by_Group)
    }
  }
  return(data)
}



