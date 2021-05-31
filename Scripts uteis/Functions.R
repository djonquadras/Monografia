library(readxl)
library(tibble)
library(lubridate)

getDemanda <- function(caminho, sheet = "Demanda", ...){
  demandas <- as.tibble(read_excel(caminho, sheet))
  names(demandas) <- c("Centro", "Ordem", "MRP", "Material", "Resumo",
                       "Demanda", "QEntregue", "Chegada", "Prazo",
                       "Deposito", "InicioReal", "FimReal")  

  demandas <- demandas[ , ! names(demandas) %in% c("Centro", "Ordem", "MRP", "QEntregue", "Deposito", "InicioReal", "FimReal")]
  demandas <- demandas[demandas$Demanda > 50,]
  demandas <- demandas[year(demandas$Chegada) == 2020 ,]
  demandas$Chegada <- demandas$Chegada - 2*(60*60*24)
  
  return(demandas)
}

getLista <- function(caminho, sheet = "RawMaterial", ...){
  listasTecnicas <- as.tibble(read_excel(caminho, sheet = "RawMaterial", 
                                         col_types = c("numeric", "text", "skip", 
                                                       "skip", "skip", "numeric", "skip", 
                                                       "skip", "numeric", "text", "numeric", 
                                                       "text")))
  names(listasTecnicas) <- c("Material", "Resumo", "Nivel", "Componente", "ResumoMP", "Quantidade", "Unidade")

  listasTecnicas$DemandaE230 <- colocaDemanda("E230", listasTecnicas)
  listasTecnicas$DemandaE170 <- colocaDemanda("E170", listasTecnicas)
  listasTecnicas$Demanda1006 <- colocaDemanda("1006",listasTecnicas)
  
  listOfMaterial <- tibble()
  
  for(i in unique(listasTecnicas$Material)){
    a <- tibble(listasTecnicas[listasTecnicas$Material == i,])
    
    q <- a$Quantidade[a$Nivel == 1]
    a$Quantidade[a$Nivel == 1] <- a$Quantidade[a$Nivel == 1]/q
    a$DemandaE230[a$Nivel == 1] <- round(max(a$DemandaE230)/q,2)
    a$DemandaE170[a$Nivel == 1] <- round(max(a$DemandaE170)/q,2)
    a$Demanda1006[a$Nivel == 1] <- round(max(a$Demanda1006)/q,2)
    
    listOfMaterial <- rbind(listOfMaterial, a[a$Nivel ==1,])
    
  }
  
  listOfMaterial <- listOfMaterial[ , ! names(listOfMaterial) %in% c("ResumoMP", "Nivel", "Componente")]
  listOfMaterial <- listOfMaterial[!(listOfMaterial$DemandaE230 + listOfMaterial$DemandaE170 + listOfMaterial$Demanda1006) == 0, ]
  listOfMaterial <- listOfMaterial[!is.na(listOfMaterial$DemandaE230), ]
  listOfMaterial <- listOfMaterial[!is.na(listOfMaterial$DemandaE170), ]
  listOfMaterial <- listOfMaterial[!is.na(listOfMaterial$Demanda1006), ]
  return(listOfMaterial)
}


colocaDemanda <- function(tipo, base){
  v <- vector()
  c <- grepl(tipo, base$ResumoMP)
  for (i in 1:length(c)) {
    if(c[i]){
      v[i] <- base$Quantidade[i]
    } else {
      v[i] <- 0
    }
  }
  return(v)
}
