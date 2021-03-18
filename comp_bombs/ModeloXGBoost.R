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
  filter(levels < 130, levels > 1) %>% 
  select(vars)
datcat_gd <- datcat_df[ , vars_gd$vars]

#--- Unifico el df de numericas y categoricas
datnumcat_df <- cbind(datnum_df, datcat_gd)
datnumcat_df$payment_type <- NULL
datnumcat_df$quantity_group <- NULL
datnumcat_df$waterpoint_type_group <- NULL
datnumcat_df$recorded_by <- NULL

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


input_data <- subset(dattrainOrlab,,-c(extraction_type_group,extraction_type_class, 
                                       management_group, quality_group, amount_tsh, construction_year))




###### XGBOOST################
input_data_cat <- input_data %>% select(where(is.character))
dummies <- dummyVars(~ . , data = input_data_cat)

df_all_ohe <- as.data.frame(predict(dummies, newdata = input_data))

dattrainOrlab_dt <- as.data.table(dattrainOrlab)
dattestOr_dt <- as.data.table(dattestOr)
dattrainOrlab_dtcat <- dattrainOrlab_dt %>% select(where(is.character))


for (col in names(dattrainOrlab_dtcat)){
  dattrainOrlab_dt[ , sprintf('fe_%s',col)  := .N , by = col ]
  dattestOr_dt[ , sprintf('fe_%s',col) := .N , by = col ]
}
for  (col in names(dattrainOrlab_dtcat)){
  #dattrainOrlab_dt[,col] <- NULL
  dattestOr_dt[,col] <- NULL
}   

trainlab_dt <- merge(
  dattrainOrlab_dt, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)

train_dt <- as.data.frame(dattrainOrlab_dt)
test_dt <-as.data.frame(dattestOr_dt)

df_all_combined <- cbind(input_data[,-c(which(colnames(input_data) %in% colnames(input_data_cat)))],df_all_ohe)
train_dt[,'status_group'] <- as.numeric(train_dt[,'status_group']) -1
train_feat <- select(train_dt,,-status_group)
xgb_train <- xgb.DMatrix(data = data.matrix(train_feat), label= data.matrix(df_all_combined$status_group))

xgb <- xgboost(data = data.matrix(train_feat), 
               label =  data.matrix(train_dt$status_group), 
               eta = 0.05,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 3,
               nthread = 3
)

test_dt$payment_type <- NULL
test_dt$quantity_group <- NULL
test_dt$waterpoint_type_group <- NULL
test_dt$recorded_by <- NULL
test_dt$funder <- NULL
test_dt$ward <- NULL
test_dt$wpt_name <- NULL
test_dt$public_meeting <- NULL
test_dt$permit <- NULL
test_dt$installer <- NULL
test_dt$num_private <- NULL
test_dt$scheme_name <- NULL
test_dt$subvillage <- NULL

test_dt$date_recorded <- dattestOr$date_recorded
train_dt$date_recorded <- dattrainOr$date_recorded



####### SUBMISSION #########
#------------ Prediccion
my_pred <- predict(xgb,data.matrix(test_dt), reshape = TRUE)
my_pred <- as.data.frame(my_pred)
colnames(my_pred) <- levels(dattrainOrlab_dt$status_group)

my_pred$PredictedClass <- apply(my_pred, 1, function(y) colnames(my_pred)[which.max(y)])

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$PredictedClass
)
# guardo submission
fwrite(my_sub, file = "XGBOOSTIntento.csv" )


#### XGBOOST CV
depth <- c(21, 19, 17, 15, 13, 11, 9, 7, 5, 3)
eta <- c(20:2) / 100
subsample <- c(5:10) / 10
colsample <- c(5:10) / 10

for (d in depth){
  for (e in eta){
    for (s in subsample){
      for (c in colsample){
        
        bst_model <- xgb.cv(data =  data.matrix(train_feat),
                            nfold = 5,
                            label =  data.matrix(train_dt$status_group),
                            num_class = 3,
                            max_depth = d,
                            eta = e,
                            nthread = 12,
                            subsample = s,
                            colsample_bytree = c,
                            min_child_weight = 1,
                            nrounds = 600, 
                            objective = "multi:softprob",
                            maximize = FALSE)
        
        write.csv(bst_model, file = paste0("./cross_validation/results_depth_", d, "_eta_", e, "_subsample_", s, "_colsample_", c, ".csv"), row.names = FALSE)
      }
    }
  }
}


