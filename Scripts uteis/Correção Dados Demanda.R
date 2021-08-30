library(openxlsx)

demandas <- openxlsx::read.xlsx("D:/Google Drive/UFSC/Monografia/ChegadaOrdens.xlsx", detectDates = TRUE)


length(unique(demandas$productionorder))
