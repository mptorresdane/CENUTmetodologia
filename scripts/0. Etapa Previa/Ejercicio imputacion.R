rm(list = ls())
gc()
library(pacman)
pacman::p_load(arrow,data.table,dtplyr,dplyr,tidyr,janitor,mice,purrr,DescTools)

bloques<- readxl::read_xlsx("data/DicEspecificacionesMatriz (1).xlsx",sheet = 2) |> 
  filter(Publicación==3) |> 
  filter(`Variable Diccionario`!="economic_unit_owner_age_range")

## nombres columnas a imputar
cols<- bloques$`Variable Diccionario`
cols_flags<- paste0(cols,"_flag")

## muestra 10000 registros
datos<- fread("data/csv_10000.csv") |> 
select(-economic_unit_owner_age_range,-economic_unit_owner_age_range_flag) |> 
  mutate(across(any_of(cols[cols!="informant_age"]),~as.factor(.))) 


### solo datos, sin banderas
datos_noflag<- datos |> 
  select(-all_of(cols_flags))

### -4 (datos no validos)
datos_novalidos<- datos |> 
  select(all_of(cols_flags)) |> 
  mutate(across(everything(),~ifelse(. %in% c(-4),TRUE,FALSE)))

names(datos_novalidos)<- gsub("_flag$", "", names(datos_novalidos))

## volver los -4 = NA

datos_noflag[, (cols) := lapply(cols, function(col)
  replace(get(col), datos_novalidos[[col]], NA)
)]

#########################################################
############ matriz de verdaderos NA ####################
#########################################################

## banderas de columans  aimputar
flags_imp<- datos |> 
  select(all_of(cols_flags)) |> 
  mutate(across(everything(),~ifelse(. %in% c(-1,-2),TRUE,FALSE)))

### columnas que no se imputan (cuestionario, ids, auxiliares etc)
cols_noimp<- names(datos_noflag)[!names(datos_noflag) %in% cols]  

## quita el sufijo _flag a las banderas para matriz TRUE/FALSE
names(flags_imp)<- gsub("_flag$", "", names(flags_imp))

### columnas que no se imputan todo false
flags_noimp<- datos |> 
  select(all_of(cols_noimp)) |> 
  mutate(across(everything(),~FALSE))

### 
where<- bind_cols(flags_imp,flags_noimp) |> 
  select(all_of(names(datos_noflag))) |> 
  as.matrix()


#########################################################
############ matriz de predicción  ######################
#########################################################

##Filas = variables a imputar
#columnas = predictoras

pred <- make.predictorMatrix(datos_noflag)
pred[,] <- 0

# variables a usar como predictores
pred[cols,c("economic_unit_registered_with_any_authority_entity_flag",
            "unit_type",
            "state_code")] <- 1


#########################################################
############ método    #################################
#########################################################
meth <- make.method(datos_noflag)

meth["gender"] <- "pmm"
meth["highest_educational_level_achieved"] <- "pmm"
meth["economic_unit_record_in_chamber_of_commerce"] <- "sample"


#########################################################
##### imputacion por bloques de  sector economico  y tamaño empresa
#########################################################

(ver<- datos_noflag |> 
  count(economic_unit_total_worker_range,ciiu_economic_sector) |> 
   filter(n<10))

## unir categorias, para que no haya unidades solas por cruce
datos_noflag<- datos_noflag |> 
  mutate(tamano=ifelse(economic_unit_total_worker_range %in% 
                         c("","Pendiente","Sin clasificar"),"Sin clasificar",economic_unit_total_worker_range)) |> 
  mutate(sector=ifelse(ciiu_economic_sector %in% 
                         c("Otros_Sectores",
                           "Administración Pública y servicios publicos domiciliarios",
                           "Transporte"),"Otros_Sectores",ciiu_economic_sector))

(ver<- datos_noflag |> 
    count(tamano,sector))

sectores<- unique(datos_noflag$sector)
tamanos<- unique(datos_noflag$tamano)


listadelistas<- list()

### Loop para imputar cada interaccion sector*region por separado
for (i in 1:length(sectores)) {
  
  for (j in 1:length(tamanos)) {
    
  cat("empieza",sectores[i],":",tamanos[j])
  run=paste0(sectores[i],"_",tamanos[j])
  filas <- datos_noflag$sector == sectores[i] & 
    datos_noflag$tamano == tamanos[j]
  
  dfimputa<- datos_noflag[filas,] |> 
    select(-sector,-tamano)  ## deseleccionar union categorias
  
  where_recorta<- where[filas,]
  
  imp <- mice(
    dfimputa,
    method = meth, ##metodo
    predictorMatrix = pred,  # matrix de prediccion
    m = 5, ## data frames a crear (numero impar para evitar empates)
    maxit = 5, ## replicas x variable
    where = where_recorta,  ## NAS reales
    ## renovacion cam  comer despues de cam
    visitSequence = c("economic_unit_record_in_chamber_of_commerce",
                      "obtain_renew_business_registration_in_2023_flag",
                      setdiff(cols, c("economic_unit_record_in_chamber_of_commerce","obtain_renew_business_registration_in_2023_flag"))),  
    seed = 20260303, ## replicabilidad
    printFlag = TRUE
  )
  
  listadelistas[[run]] <- complete(imp, action = "all")
  
  
}}

length(sectores)*length(tamanos)
length(listadelistas)  ##  imputaciones separadas por sector y tamaño


##  pegue de cada una de las 25 imputaciones
## quedan 5 df de 10.000 filas
imputaciones_completas <- lapply(1:5, function(m){
  
  bind_rows(
    lapply(listadelistas, function(sectores){
      sectores[[m]]
    })
  )
  
})

### df final (5 df a 1)

final_df <- imputaciones_completas[[1]]
a<- names(final_df)
for(col in names(final_df)){
  
  vals <- lapply(imputaciones_completas, `[[`, col)
  mat  <- do.call(cbind, vals)
  
  final_df[[col]] <- if(is.numeric(final_df[[col]]))
    apply(mat, 1, median, na.rm = TRUE)  ## Mediana variables numericas     
  else
    apply(mat, 1, Mode) ## Moda otras  
}

## Nuevos NA debe ser igual a flag= -3 +-4
table(final_df$single_tax_registry_flag,useNA = "a")
table(datos$single_tax_registry_flag_flag,useNA = "a")

table(final_df$lending_entity,useNA = "a")
table(datos$lending_entity_flag,useNA = "a")


table(final_df$obtain_renew_business_registration_in_2023_flag,useNA = "a")
table(datos$obtain_renew_business_registration_in_2023_flag_flag,useNA = "a")


# resultados 

## lista deimputaciones
imputaciones_completas

# combinacion
final_df
