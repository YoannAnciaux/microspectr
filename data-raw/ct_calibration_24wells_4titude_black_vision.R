# TODO Explain where this data come from and put specifics call from elab
# DM500, 10Rilux2, gain 3000

dataset <- microspectr_example("24wells_4titude_black_vision.xlsx")
tidy_data_calib <- import_excel(dataset, list('No filter 1' = 'LUX', '2' = 'OD'))

# Add a column with the reference (the source of the crosstalk) for any well
# Find the coordinate in number format corresponding to the well name
# (A01 = (line = 1, column = 1))
tidy_data_calib_lux <- tidy_data_calib %>%
  filter(Content == "Positive control P", Signal == "LUX") %>%
  select(Time, Source = "Value") %>%
  full_join(filter(tidy_data_calib, Signal == "LUX"), .) %>%
  select(-c(Signal))  %>%
  separate(Well, c("Letters", "Numbers"), sep = 1) %>%
  mutate(Line = map_dbl(Letters, ~ which(.x == LETTERS)),
         Column = as.numeric(Numbers)) %>%
  unite(Well, c("Letters", "Numbers"), sep = "")

# register the coordinates of the source
source_well_calib <- tidy_data_calib_lux %>%
  filter(Content == "Positive control P", Time == 0)

# Compute the euclidian distance to the source well with the unit being the
# distance between two wells.
tidy_data_calib_lux <- tidy_data_calib_lux %>%
  mutate(Distance = map2_dbl(Line, Column, ~ sqrt((.x-source_well_calib$Line)^2+(.y-source_well_calib$Column)^2)))

# Fit the value of the blank over time by a linear regression
blank_calib <- tidy_data_calib_lux %>%
  filter(Distance > 3, Value != "NA") %$%
  lm(Value ~ Time) %>%
  predict(tibble(Time = unique(tidy_data_calib_lux$Time))) %>%
  tibble(Time = unique(tidy_data_calib_lux$Time), Blank = .)

# compute the ratio of
tidy_data_calib_lux <- tidy_data_calib_lux %>%
  full_join(., blank_calib) %>%
  mutate(Ratio = (Value-Blank) / (Source-Blank))

# Compute the distance matrix between all the wells
nline <- 4
ncolumn <- 6
dist_matrix <- as.matrix(dist(expand.grid(1:ncolumn,1:nline)))

# Transform data in loglog for linear fitting of the loglog relationship
data_temp <- filter(tidy_data_calib_lux, Content == "Blank B", Ratio > 0) %>%
  mutate(logRatio = log(Ratio),
         logDistance = log(Distance))

lin.mod <- lm(logRatio ~  logDistance, data = data_temp)
# fit a segmented linear relationship for the crosstalk clearly above the blank
# and the crosstalk just above the blank because of noise.
segmented.mod <- segmented(lin.mod, seg.Z = ~logDistance, psi=log(2))

a = exp(segmented.mod$coefficients[1])
b = segmented.mod$coefficients[2]

ct_matrix <- a *apply(dist_matrix, c(1,2), function(x) ifelse(x > 0, x^b, 0))

ct_calibration_24wells_4titude_black_vision <- list(ct_matrix = ct_matrix,
                                                    blank = mean(blank_calib$Blank))

use_data(ct_calibration_24wells_4titude_black_vision, overwrite = TRUE)
