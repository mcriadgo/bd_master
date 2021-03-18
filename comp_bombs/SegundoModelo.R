-------------------
  # Autor: Marta Criado
  # Fecha: 2021_03_06
  # Inputs: Datos entrada bombas
  # Salida: Modelo inicial
  # Comentarios: 
  #-------------------

#--- Cargo librerÃ­as
suppressPackageStartupMessages({
  library(dplyr)          # Manipulacion de datos 
  library(data.table)     # Leer y procesar ultra-rapido
  library(ggplot2)        # La librerÃ­a grafica
  library(inspectdf)      # EDAs automaticos
  library(ranger)         # Fast randomForest
  library(forcats)        # Tratar variables categoricas
  library(mcen)
  library(missRanger)
  library(mice)
  library(Hmisc)          # para hacer el describe
  library(tidyverse)
  library(revgeo)         # para cambiar longitude, latitude por ciudad
  library(miceRanger)
  
})

dattrainOr    <- fread(file = "./data/training_set.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "./data/training_set_labels.csv", data.table = FALSE )
dattestOr     <- fread(file = "./data/test_set.csv", data.table = FALSE  )

datcat_df <- dattrainOr %>% select(where(is.character))
datnum_df <- dattrainOr %>% select(where(is.numeric))


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

#--- Unifico el df de numericas y categoricas
datnumcat_df <- cbind(datnum_df, datcat_gd)
datnumcat_df$payment_type <- NULL
datnumcat_df$quantity_group <- NULL

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab <- merge(
  datnumcat_df, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

dattrainOrlab$payment_type <- NULL
dattrainOrlab$quantity_group <- NULL

## No imputo nada 

my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = dattrainOrlab
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model$prediction.error
acierto


##################### SEGUNDO MODELO QUITANDO LAS COLUMNAS CON MAS DEL 50% DE NULOS% ###############
describe(dattrainOrlab)
# quito waterpoint_type_group
dattrainOrlab$waterpoint_type_group <- NULL
my_model2 <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = dattrainOrlab
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model2$prediction.error
acierto   #### 0.81200

#--- Pintar importancia de variables
impor_df <- as.data.frame(my_model2$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
rownames(impor_df) <- NULL

ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
ggsave('./charts/modelo_sinwaterpoint.png')


########################### Creo la variable de años de construccion con respecto al maximo ############
max_constYear <- max(dattrainOrlab$construction_year)
dattrainOrlab$fe_age <- ifelse( 
  dattrainOrlab$construction_year == 0, 
  NA, 
  max_constYear - dattrainOrlab$construction_year
)

dattestOr$fe_age <- ifelse( 
  dattestOr$construction_year == 0, 
  NA, 
  max_constYear - dattestOr$construction_year
)

# Convierte a categorica los años
dattrainOrlab$fe_age_cat <- ifelse( 
  dattrainOrlab$fe_age <= 17, 
  'Nuevo', 
  ifelse(
    (dattrainOrlab$fe_age > 17) & (dattrainOrlab$fe_age <= 35), 
    'Medio', 'Antiguo'
    )
)
dattrainOrlab$fe_age_cat <- replace_na(dattrainOrlab$fe_age_cat,'Otro')
dattrainOrlab$fe_age <- NULL

dattestOr$fe_age_cat <- ifelse( 
  dattestOr$fe_age <= 17, 
  'Nuevo', 
  ifelse(
    (dattestOr$fe_age > 17) & (dattestOr$fe_age <= 35), 
    'Medio', 'Antiguo'
  )
)
dattestOr$fe_age <- NULL

my_model3 <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = dattrainOrlab
)



# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model3$prediction.error
acierto   #### 0.811532

#--- Pintar importancia de variables
impor_df <- as.data.frame(my_model3$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
rownames(impor_df) <- NULL

ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
ggsave('./charts/modelo_agecat.png')

################# Las variables mas skewed transformadas a log ##############
dattrainOrlab$num_private <- NULL  # porque tiene muchos nulos
dattrainOrlab$population_log <- log(dattrainOrlab$population)
dattrainOrlab$amount_tsh_log <- log(dattrainOrlab$amount_tsh)
dattestOr$population_log <- log(dattestOr$population)
dattestOr$amount_tsh_log <- log(dattestOr$amount_tsh)

### Elimino nulos imputando con Random Forest
dattrainOrlab$status_group <- NULL
#Recode

dattrainOrlab$amount_tsh_log  <- replace_na(dattrainOrlab$amount_tsh_log  ,-Inf )
dattrainOrlab$population_log  <- replace_na(dattrainOrlab$population_log  ,-Inf )

dattestOr$amount_tsh_log  <- replace_na(dattestOr$amount_tsh_log  ,-Inf )
dattestOr$population_log  <- replace_na(dattestOr$population_log  ,-Inf )


# Imputo con mice
# poly <- c("fe_age_cat")
# poly2 <- c("quantity")
# pmm <- c("amount_tsh_log", "population_log")
# imp_mice <- mice(data = dattrainOrlab, 
#             seed = 500, 
#             m = 5,
#             print = FALSE)
# dattrain_imp <- complete(imp_mice,1)
# dattest_imp <-impute(imp_mice, dattestOr)

##### Imputar con miceRanger
# imp2 <- miceRanger(data = dattrainOrlab, 
#             m = 5, verbose=TRUE, returnModels = TRUE)
# 
# dattrain_imp <- completeData(imp2,3)$Dataset_3
# 
# cols_to_impute <- colnames(dattrain_imp)
# dattest_imp <- impute(data=dattestOr[,cols_to_impute],miceObj=imp2,datasets=1:imp2$callParams$m)
#%%%%%%%%

data_final <- subset(dattrainOrlab, select = -c(population, construction_year, amount_tsh))
data_finallab <- merge(
  dattrain_imp, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

data_finallab$status_group <- as.factor(data_finallab$status_group)

my_model_final1 <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = data_finallab
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model_final1$prediction.error
acierto   #### 

#--- Pintar importancia de variables
impor_df <- as.data.frame(my_model_final1$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
rownames(impor_df) <- NULL

ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
ggsave('./charts/modelo_PrimerIntentoFinal.png')



####### SUBMISSION #########
#------------ Prediccion
my_pred <- predict(my_model_final1, dattestOr)

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "PrimerIntento.csv" )


##
