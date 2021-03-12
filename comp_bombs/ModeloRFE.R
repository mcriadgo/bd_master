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



dattrainOrlab.task = makeClassifTask(data = dattrainOrlab, target = "status_group")
estimateTimeTuneRanger(dattrainOrlab.task)
# Tuning
res = tuneRanger(iris.task, measure = list(multiclass.brier), num.trees = 1000, 
                 num.threads = 2, iters = 70, save.file.path = NULL)


dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)
dattrainOrlab$date_recorded <- dattrainOr$date_recorded


input_data <- subset(dattrainOrlab,,-c(extraction_type_group,extraction_type_class, 
                                       management_group, quality_group, amount_tsh, construction_year))


input_data$status_group <-as.factor(input_data$status_group)
my_model_final <- ranger( 
  status_group ~., 
  importance = 'impurity',
  data = input_data,
  num.trees = 1000,
  mtry=9
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model_final$prediction.error
acierto   #### 
input_data <- lapply(input_data, as.factor)

#hypertunning
grid <-  expand.grid(mtry = c(2,5,6,8), splitrule=c('gini'), min.node.size=1)

fitControl <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE)

fit = caret::train(
  status_group ~.,
  data=input_data,
  method = 'ranger',
  tuneGrid = grid,
  trControl = fitControl,
  verbose=TRUE,
  ntrees=1000
)
print(fit)


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


