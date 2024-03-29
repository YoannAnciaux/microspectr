% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_data.R
\name{correct_crosstalk_lux}
\alias{correct_crosstalk_lux}
\title{Correct luminescence crosstalk in microplate}
\usage{
correct_crosstalk_lux(data, crosstalk_lux_ref, lux_name = NULL)
}
\arguments{
\item{data}{A data frame or a tibble with at least a column 'Time' and a column
'Value'.}

\item{crosstalk_lux_ref}{A list with two elements \code{list(ct_matrix = 'matrix of crosstalk', blank = 'value of the blank')}.}

\item{lux_name}{String with the name of the luminescence signal in the 'Signal'
column when providing a data with multiple signals. Default is NULL and will
consider that only the luminescent 'Value' are provided.}
}
\value{
A tibble with at least a column 'Time' and a column 'Value' with crosstalk
corrected luminescence value (and Signal if lux_name is provided). Other columns
are returned unmodified.
}
\description{
\code{correct_crosstalk_lux} correct luminescence crosstalk/bleed-through between
wells based on a calibration for the given microplate model.
}
\details{
\itemize{
\item 'Value' must be raw and not blank corrected.
\item The ct_matrix and the blank value must be obtained from a calibration for
the given microplate model.
\item The number of 'Value' entry for each 'Time' entry must be equal to the number
of lines and wells of the ct_matrix.
}
}
\examples{
data <- Ecoli_T7_24well_Lux_2Fluo_OD
crosstalk_lux_ref <- ct_calibration_24wells_4titude_black_vision
correct_crosstalk_lux(data, crosstalk_lux_ref, "LUX")
}
