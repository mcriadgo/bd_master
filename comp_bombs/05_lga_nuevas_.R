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
})


#-- Leo ficheros
dattrainOr    <- fread(file = "./data/train.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "./data/train_labels.csv", data.table = FALSE )
dattestOr     <- fread(file = "./data/test.csv", data.table = FALSE  )


# # EDA--- (Exploratory Data Analysis) - hecho con inspectdf
# # Horizontal bar plot for categorical column composition
# x <- inspect_cat(dattrainOr) 
# show_plot(x)
# 
# # Correlation betwee numeric columns + confidence intervals
# x <- inspect_cor(dattrainOr)
# show_plot(x)
# 
# # Bar plot of most frequent category for each categorical column
# x <- inspect_imb(dattrainOr)
# show_plot(x)
# 
# # Bar plot showing memory usage for each column
# x <- inspect_mem(dattrainOr)
# show_plot(x)
# 
# # Occurence of NAs in each column ranked in descending order
# x <- inspect_na(dattrainOr)
# show_plot(x)
# 
# # Histograms for numeric columns
# x <- inspect_num(dattrainOr)
# show_plot(x)
# 
# # Barplot of column types
# x <- inspect_types(dattrainOr)
# show_plot(x)
# 
# # EDA - Results
# # Mirar 0s construction_year
# # gps_height 0 y negativos
# # longitude valores de 0 ???
# # population -> outliers? (300.000)
# # 27 categóricas - 10 numéricas
# # recorded_by es una sola categoria -> cte. (a quitar)

#--- Niveles de las categoricas.
datcat_df <- dattrainOr %>% select(where(is.character))

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

#--- Me quedo con las castegorias que tienen valores de > 1 y < 1000
vars_gd <- numlev_df %>%
  filter(levels < 1000, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

#---- Corregir estas deficiencias.
# Vamos a hacer un modelo con las variables numericas
# Vamos a la playa!
datnum_df <- dattrainOr %>% select(where(is.numeric))

#--- Unifico el df de numericas y categoricas
datnumcat_df <- cbind(datnum_df, datcat_gd)

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab <- merge(
  datnumcat_df, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

#... Efecto de quita payment_type y quantity_group
dattrainOrlab$payment_type <- NULL
dattrainOrlab$quantity_group <- NULL


#-------------------- 
#-------Modelo
# No hiperparamtrizo - La playa!. Cuidado con Optimizar demasiado pronto.
# Incluyo el parametro de importancia para saber importancia de varialbes
dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)

#-- Grid Search
my_ntree <- c(500, 700)
my_mtry  <- c(5, 6, 7)
my_pars  <- expand.grid(my_ntree, my_mtry)
names(my_pars) <- c('myntree', 'mymtry')
my_pars$acierto <- 0

for (i in 1:nrow(my_pars)) {
   tic() 
   my_model <- ranger( 
     status_group ~ . , 
     importance = 'impurity',
     num.trees = my_pars$myntree[i],
     mtry = my_pars$mymtry[i],
     data = dattrainOrlab
   )
   
   # **Estimacion** del error / acierto **esperado**
   acierto <- 1 - my_model$prediction.error
   acierto
   my_pars$acierto[i] <- acierto 
   print(my_pars)
   toc()

}

# Mejor grid search 
# num.trees = 700 - mtry = 6
# Corro modelo con ese mejor grid
tic()
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  num.trees = 700,
  mtry = 6,
  data = dattrainOrlab
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
ggsave('./charts/05_lga_nodup_grid.png')

#------------ Prediccion
my_pred <- predict(my_model, dattestOr)

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "./submissions/05_lga_nodup_grid.csv" )


#------ Resultados
# 00 - 0.7127946 - solo num - 0.7145
# 01 - 0.8104714 - num + cat (>1 & < 100) - 0.8153 
# 02 - 0.8112458 - num + cat (>1 & < 100) no dup - 0.8154 
# 03 - 0.8168687 - num + cat (>1 & < 1000) no dup -  0.8128 
# 04 - 0.8122559 - num + cat (>1 & lga) no dup -  0.8156 
# 04 - 0.8138047 - num + cat (>1 & funder) no dup - 0.8156 
# 05 - 0.8132323 - num + cat (>1 & funder) no dup- grid 700-6 - 0.8161

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