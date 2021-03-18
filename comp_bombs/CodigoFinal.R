#-------------------
# Autores: Juan Manuel del Valle, Marta Criado, Arnau Fabregat, Irene Berros
# Fecha: 2021_03_12
# Comentarios: Continua desde el último intento de la clase del día 6 de marzo
# se incluyen variables categóricas por frecuencia con data.table
#-------------------

#--- Cargo librerías
suppressPackageStartupMessages({
  library(dplyr)          # Manipulacion de datos 
  library(data.table)     # Leer y procesar ultra-rapido
  library(ggplot2)        # La librería grafica
  library(inspectdf)      # EDAs automaticos
  library(ranger)         # Fast randomForest
  library(forcats)        # Tratar variables categoricas
  library(tictoc)         # Calcular tiempos
  library(missRanger)     # Fast imputation of NAs --- Finalmente no se usa
  library(lubridate)      # Tratar formato fecha
})


#-- Leo ficheros
dattrainOr    <- fread(file = "./data/train.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "./data/train_labels.csv", data.table = FALSE )
dattestOr     <- fread(file = "./data/test.csv", data.table = FALSE  )

# Unimos los conjuntos train y test para tratarlos al mismo tiempo
alldata <- merge(dattrainOr, dattestOr, all = T, sort = F)

#--- Niveles de las categoricas.
datcat_df <- alldata %>% select(where(is.character))

numlev_df <- data.frame()
for (i in 1:ncol(datcat_df)) {
  col_tmp <- datcat_df[, i]
  num_lev <- length(unique(col_tmp))
  numlev_df[i, 1] <- names(datcat_df)[i]
  numlev_df[i, 2] <- num_lev
  print(numlev_df)
}
names(numlev_df) <- c('vars', 'levels')
numlev_df %>% arrange(levels)

#--- Me quedo con las castegorias que tienen valores de > 1 y < 2200
#--- Metemos "funder" para luego transformarlo en freqs --- cortamos en 2200 cat
vars_gd <- numlev_df %>%
  filter(levels < 2200, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

#---- Corregir estas deficiencias.
# Vamos a hacer un modelo con las variables numericas
# Vamos a la playa!
datnum_df <- alldata %>% select(where(is.numeric))

#--- Unifico el df de numericas y categoricas
datnumcat_df <- cbind(datnum_df, datcat_gd)

# Quitamos variables duplicadas payment_type y quantity_group --- MEJORA
datnumcat_df$payment_type <- NULL
datnumcat_df$quantity_group <- NULL



#---- FEATURE ENGINEERING -------

# se han probado las dos variables booleanas que contiene el dataset - NO MEJORA
# quitar variables por importancia o subir el corte en los niveles - NO MEJORA

datnumcat_df_imp <- datnumcat_df

# Añadimos la variable "date_recorded" en dos formatos --- MEJORA
datnumcat_df_imp$date_recorded <- alldata$date_recorded
datnumcat_df_imp$fe_date_recorded <- as.numeric(as.Date('2013-12-03') - as.Date(alldata$date_recorded))

# También añadimos el mes separado del resto de la fecha
datnumcat_df_imp$fe_month <- month(ymd(datnumcat_df_imp$date_recorded))


# -- CREAMOS NUEVAS VARIABLES

# se han probado tabmién las variables nuevas "década de creación de la bomba" y
# "cantidad de agua disponible por persona" (amount_tsh/population) -- NO MEJORA

#- Edad - Referencia año = 2014
datnumcat_df_imp$fe_age <- 2014 - datnumcat_df_imp$construction_year

#-- Distancia -30 (para quitar el offset)
datnumcat_df_imp$fe_dist <- sqrt(datnumcat_df_imp$longitude^2 + datnumcat_df_imp$latitude^2) - 30

#-- funder y quantity - Sustituimos sus categorías por la frecuencia

datnumcat_dt <- as.data.table(datnumcat_df_imp)

datnumcat_dt[ , fe_funder := .N , by = funder ]
datnumcat_dt[ ,  fe_quantity := .N , by = quantity ]

datnumcat_df_imp <- as.data.frame(datnumcat_dt)



# Separamos los conjuntos train y test
dattrainOr_imp <- datnumcat_df_imp[1:59400,]
dattestOr_imp <- datnumcat_df_imp[59401:74250,]

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab_imp <- merge(
  dattrainOr_imp, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

#-------------------- 
#-------Modelo

dattrainOrlab_imp$status_group <- as.factor(dattrainOrlab_imp$status_group)

# Buscamos los parámetros óptimos para ranger()

# #-- Grid Search
# my_ntree <- c(450,500,550,600)
# my_mtry  <- c(4,5)
# my_pars  <- expand.grid(my_ntree, my_mtry)
# names(my_pars) <- c('myntree', 'mymtry')
# my_pars$acierto <- 0
# 
# for (i in 1:nrow(my_pars)) {
#   tic()
#   my_model <- ranger(
#     status_group ~ . ,
#     importance = 'impurity',
#     num.trees = my_pars$myntree[i],
#     mtry = my_pars$mymtry[i],
#     data = dattrainOrlab_imp,
#     seed = 12345
#   )
# 
#   # **Estimacion** del error / acierto **esperado**
#   acierto <- 1 - my_model$prediction.error
#   acierto
#   my_pars$acierto[i] <- acierto
#   print(my_pars)
#   toc()
# }

# Corro modelo con el mejor grid

tic()
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  num.trees = 620,
  mtry = 5,
  data = dattrainOrlab_imp,
  seed = 67583
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model$prediction.error
acierto
toc()


#--- Pintar importancia de variables
impor_df <- as.data.frame(my_model$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
rownames(impor_df) <- NULL

ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
# ggsave('./charts/06_lga_nodup_fe01.png')

#------------ Prediccion
my_pred <- predict(my_model, dattestOr_imp)

#------ Submission
my_sub <- data.table(
  id = dattestOr_imp$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "./submissions/17_catfreqs_.csv" )
