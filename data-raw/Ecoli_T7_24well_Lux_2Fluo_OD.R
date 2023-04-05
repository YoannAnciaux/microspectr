# TODO Explain where this data come from and put specifics call from elab
# DM500, 10Cilux2, gain ????

dataset <- microspectr_example("Ecoli_T7_24well_Lux_2Fluo_OD.xlsx")
Ecoli_T7_24well_Lux_2Fluo_OD <- import_excel(dataset, list("No filter 1" = "LUX", "480-20 2" = "CFP", "600-40 3" = "SYTOX", "4" = "OD"))

usethis::use_data(Ecoli_T7_24well_Lux_2Fluo_OD, overwrite = TRUE)
