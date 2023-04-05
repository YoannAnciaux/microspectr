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

microspectr_example()
microspectr_example("Ecoli_T7_24well_Lux_2Fluo_OD.xlsx")
