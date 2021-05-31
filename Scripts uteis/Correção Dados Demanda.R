source("Scripts uteis/Functions.R")


demandas <- getDemanda("Dados/dados.xlsx")
listasTecnicas <- getLista("Dados/dados.xlsx")

#Pra ver

tudoJunto <- merge(x = demandas, y = listOfMaterial, by.x = "Material", by.y = "Material")

unique(demanda$`Texto breve material`)

data <- tibble(a = c(1,2,3),
               b = c(1,2,3))

data$c <- c(1,2,3)
data$d <- c(NA)