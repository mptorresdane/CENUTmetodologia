
rm(list = ls())
pacman::p_load(naniar,tidyverse, data.table,openxlsx,janitor,DescTools)
library(mice)

######  3145 datos y 9 variables del cuestionario CBE
load("data/toydata.rdata")



####  bloques de flujos (NAs reales)


where <- is.na(datos)

where[datos$ciiu_economic_sector == "Sin CIIU", "single_tax_registry_flag"] <- FALSE

### matriz de prediccion #####

a_imputar  <- c( "total_income_2023", "total_costs_2023","single_tax_registry_flag")

resto <- setdiff(names(datos), a_imputar)  ### variables predictoras

pred <- make.predictorMatrix(datos)

pred[,] <- 0

##Filas = variables a imputar
#columnas = predictoras

### se pueden ajustar variables predictoras todas a la vez
 pred[a_imputar, resto] <- 1 

### o de a una/ por grupos de variables
pred["total_income_2023", resto] <- 1
pred["total_costs_2023",c("total_income_2023", resto)] <- 1
pred["single_tax_registry_flag", c("ciiu_economic_sector","economic_unit_owner_age_range")] <- 1

pred

### Metodo #########
meth <- make.method(datos)
meth[] <- ""    

meth["total_income_2023"] <- "pmm"
meth["total_costs_2023"] <- "pmm"
meth["single_tax_registry_flag"] <- "logreg"

imp <- mice(
  datos,
  method = meth, ##metodo
  predictorMatrix = pred,  # matrix de prediccion
  m = 5,
  maxit = 5,
  where = where,  ## NAS reales
  seed = 20262203, ## replicabilidad
  printFlag = TRUE
)

lista_imputada <- complete(imp, action = "all")

## desviacion

sd_obs <- lapply(a_imputar, function(v) {
  values <- sapply(lista_imputada, function(df) df[[v]])
  apply(values, 1, sd, na.rm = TRUE)
})

## desviacion estandar para cada imputacion
names(sd_obs) <- a_imputar

sd_obs$total_income_2023
sd_obs$total_costs_2023


### imputa Moda y mediana de 5 iteraciones en vacios

salida_final<- datos

for (v in a_imputar) {
  
  values <- sapply(lista_imputada, function(df) df[[v]])
  
  if (is.numeric(datos[[v]])) {
    combined <- apply(values, 1, median, na.rm = TRUE)  ## Mediana
  } else {
    combined <- apply(values, 1, Mode)  ## Moda
  }
  
  miss_pos <- is.na(datos[[v]])
  
  # Remplazar NA
  salida_final[[v]][miss_pos] <- combined[miss_pos]
  
  salida_final[[paste0(v, "_imputada")]] <- as.integer(miss_pos)
}

salida_final
