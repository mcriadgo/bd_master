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
  library(caret)
  library(tuneRanger)
  library(bnstruct)
  library(xgboost)
  
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
  filter(levels < 2200, levels > 1) %>% 
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

dattrainOrlab$fe_dist <- sqrt( dattrainOrlab$longitude^2 + dattrainOrlab$latitude^2)
dattestOr$fe_dist     <- sqrt( dattestOr$longitude^2 + dattestOr$latitude^2)


dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)
dattrainOrlab$date_recorded <- dattrainOr$date_recorded

#cambio a dicotomicas las variables gps_height y amount_tsh
dattrainOrlab$tsh0flag <- 0
dattrainOrlab$tsh0flag[dattrainOrlab$amount_tsh==0]<-1
table(dattrainOrlab$tsh0flag)

#same for gps_height

dattrainOrlab$gps0flag <- 0
dattrainOrlab$gps0flag[dattrainOrlab$gps_height==0]<-1
table(dattrainOrlab$gps0flag)

dattrainOrlab$year0flag<-0
dattrainOrlab$year0flag[dattrainOrlab$fe_age==max(dattrainOrlab$construction_year)]<-1

dattrainOrlab$pop_log <- log(dattrainOrlab$population +1 )
dattrainOrlab$amount_tsh <- log(dattrainOrlab$amount_tsh +1)
dattrainOrlab$amount_tsh[is.na(dattrainOrlab$amount_tsh)] <- 1

########## PARA TEST
dattestOr$tsh0flag <- 0
dattestOr$tsh0flag[dattestOr$amount_tsh==0]<-1

#same for gps_height

dattestOr$gps0flag <- 0
dattestOr$gps0flag[dattestOr$gps_height==0]<-1

dattestOr$year0flag<-0
dattestOr$year0flag[dattestOr$fe_age==max(dattestOr$construction_year)]<-1

dattestOr$pop_log <- log(dattestOr$population +1)
dattestOr$amount_tsh <- log(dattestOr$amount_tsh+1)
dattestOr$amount_tsh[is.na(dattestOr$amount_tsh)] <- 1


### Quito columnas
input_data <- subset(dattrainOrlab,,-c(extraction_type_group,gps_height, amount_tsh, construction_year, population, region_code))


######### TRAIN MODEL ##############
input_data$status_group <-as.factor(dattrainOrlab$status_group)
my_model_final <- ranger( 
  status_group ~., 
  importance = 'impurity',
  data = input_data,
  num.trees = 1500
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model_final$prediction.error
acierto   #### 

#tunning
train_features <- select(dattrainOrlab,-status_group)
tune <- tuneRF(train_features, dattrainOrlab$status_group, 
       ntreeTry=500, stepFactor=2, improve=0.05)
print(tune)
