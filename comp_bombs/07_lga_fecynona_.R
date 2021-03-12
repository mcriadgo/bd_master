#-------------------
# Autor: Carlos Ortega
# Fecha: 2021_03_05
# Inputs: Datos entrada bombas
# Salida: Modelo num + categor nodub hasta lga + grid search ranger.
# Comentarios: 
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
  library(missRanger)     # Fast imputatin of NAs.
})


#---- FEATURE ENGINEERING -------
# construction_year = Edad + Decadas
# long /  lat -> Distancia

#-- Exploramos construction_year
# as.data.frame(table(dattrainOrlab$construction_year))
#-- Imputamos 0s con missRanger (0 == NAs)
dattrainOrlab$fe_cyear <- dattrainOrlab$construction_year
dattrainOrlab$fe_cyear <- ifelse( 
                                  dattrainOrlab$fe_cyear == 0, 
                                  NA, 
                                  dattrainOrlab$fe_cyear
                                  )
# Train quito construction_year y status group 
dattrainOrlab$construction_year <- NULL
dattrainOrlab$status_group <- NULL  # se quita esta columna porque no hay referencia para hacer la imputacion al test
dattrainOrlab_imp <- missRanger(
                                dattrainOrlab,  
                                pmm.k = 3,
                                num.trees = 100
                               )
#-- Test
dattestOr$fe_cyear <- dattestOr$construction_year
dattestOr$fe_cyear <- ifelse( 
  dattestOr$fe_cyear == 0, 
  NA, 
  dattestOr$fe_cyear
)
dattrainOrlab$construction_year <- NULL
dattrainOrlab_imp <- missRanger(
  dattrainOrlab,  
  pmm.k = 3,
  num.trees = 100
)
# ERROR - ERROR--- MUY MAL CARLOS!!!!.MALA PRÁCTICA
# JUNTAR TRAIN Y TEST - E IMPUTAR DE UNA VEZ - QUEDANDO CON NUM/CAT <1000.


#- Edad - Referencia año = 2014
dattrainOrlab$fe_age <- 2014 - dattrainOrlab$fe_cyear
dattestOr$fe_age     <- 2014 - dattestOr$fe_cyear

#- Decadas.
# Dudas sobre años recientes - riesgo productivizacion.


#-- Distancia -----
dattrainOrlab$fe_dist <- sqrt( dattrainOrlab$longitude^2 + dattrainOrlab$latitude^2)
dattestOr$fe_dist     <- sqrt( dattestOr$longitude^2 + dattestOr$latitude^2)



#-------------------- 
#-------Modelo
# No hiperparamtrizo - La playa!. Cuidado con Optimizar demasiado pronto.
# Incluyo el parametro de importancia para saber importancia de variables
dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)

# Mejor grid search 
# num.trees = 700 - mtry = 6
# Corro modelo con ese mejor grid
tic()
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data       = dattrainOrlab
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
ggsave('./charts/06_lga_nodup_fe01.png')

#------------ Prediccion
my_pred <- predict(my_model, dattestOr)

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "./submissions/06_lga_nodup_fe01.csv" )


#------ Resultados
# 00 - 0.7127946 - solo num - 0.7145
# 01 - 0.8104714 - num + cat (>1 & < 100) - 0.8153 
# 02 - 0.8112458 - num + cat (>1 & < 100) no dup - 0.8154 
# 03 - 0.8168687 - num + cat (>1 & < 1000) no dup -  0.8128 
# 04 - 0.8122559 - num + cat (>1 & lga) no dup -  0.8156 
# 04 - 0.8138047 - num + cat (>1 & funder) no dup - 0.8156 
# 05 - 0.8132323 - num + cat (>1 & funder) no dup- grid 700-6 - 0.8161
# 06 - 0.8122559 - num + cat (>1 & <1000) no dup- fe cyear/dist - 0.8174 

#----- Trabajo exploratorio de analizar variables sospechosas iguales
numlev_df %>%
  #filter(levels < 100, levels > 1) %>% 
  filter(levels > 1) %>% 
  arrange(levels)  

# #--- quantity y quantity_group  - Iguales
# unique(datcat_df$quantity)
# unique(datcat_df$quantity_group)
# all.equal(datcat_df$quantity, datcat_df$quantity_group)
# #--- payment y payment_type  - Iguales
# unique(datcat_df$payment)
# unique(datcat_df$payment_type)
# all.equal(datcat_df$payment, datcat_df$payment_type)
# # Decision: siguiente modelo quito payment_type y quantity_group
# 