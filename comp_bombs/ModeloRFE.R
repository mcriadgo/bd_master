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
  library(randomForest)
  
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
  filter(levels < 100, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

#--- Unifico el df de numericas y categoricas
datnumcat_df <- cbind(datnum_df, datcat_gd)
datnumcat_df$payment_type <- NULL
datnumcat_df$quantity_group <- NULL
datnumcat_df$waterpoint_type_group <- NULL

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab <- merge(
  datnumcat_df, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

########################### Creo la variable de años de construccion con respecto al maximo ############
max_constYear <- max(dattrainOrlab$construction_year)
dattrainOrlab$fe_age <- ifelse( 
  dattrainOrlab$construction_year == 0, 
  0, 
  max_constYear - dattrainOrlab$construction_year
)

dattestOr$fe_age <- ifelse( 
  dattestOr$construction_year == 0, 
  0, 
  max_constYear - dattestOr$construction_year
)

dattrainOrlab$num_private <- NULL  # porque tiene muchos nulos

dattrainOrlab$pop_cat <- ifelse( 
  dattrainOrlab$population <= 100, 
  'Poco_Poblado', 'Muy_Poblado')

dattestOr$pop_cat <- ifelse( 
  dattestOr$population <= 100, 
  'Poco_Poblado', 'Muy_Poblado')

dattrainOrlab$fe_dist <- sqrt( dattrainOrlab$longitude^2 + dattrainOrlab$latitude^2)
dattestOr$fe_dist     <- sqrt( dattestOr$longitude^2 + dattestOr$latitude^2)

dattrainOrlab$date_recorded <- as.Date(dattrainOrlab$date_recorded, "%Y-%m-%d")
dattestOr$date_recorded <- as.Date(dattestOr$date_recorded, "%Y-%m-%d")

dattrainOrlab$month_recorded <- month(dattrainOrlab$date_recorded)
dattestOr$month_recorded <- month(dattestOr$date_recorded)


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(500)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(status_group~., data=dattrainOrlab, method="rf", metric='accuracy', tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)
dattrainOrlab$date_recorded <- dattrainOr$date_recorded
my_model_final <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = dattrainOrlab
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model_final$prediction.error
acierto   #### 

#--- Pintar importancia de variables
impor_df <- as.data.frame(my_model_final$variable.importance)
names(impor_df)[1] <- c('Importance')
impor_df$vars <- rownames(impor_df)
rownames(impor_df) <- NULL

ggplot(impor_df, aes(fct_reorder(vars, Importance), Importance)) +
  geom_col(group = 1, fill = "darkred") +
  coord_flip() + 
  labs(x = 'Variables', y = 'Importancia', title = 'Importancia Variables') +
  theme_bw()
ggsave('./charts/modelo_SegundoIntentoFinal.png')


####### SUBMISSION #########
#------------ Prediccion
my_pred <- predict(my_model_final, dattestOr)

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "SegundoIntento.csv" )


