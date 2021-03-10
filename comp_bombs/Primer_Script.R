#-------------------
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
})

dattrainOr    <- fread(file = "training_set.csv", data.table = FALSE )
dattrainLabOr <- fread(file = "training_set_labels.csv", data.table = FALSE )
dattestOr     <- fread(file = "test_set.csv", data.table = FALSE  )


# EDA--- (Exploratory Data Analysis) - hecho con inspectdf
# Horizontal bar plot for categorical column composition
x <- inspect_cat(dattrainOr) 
show_plot(x)

# Correlation betwee numeric columns + confidence intervals
x <- inspect_cor(dattrainOr)
show_plot(x)

# Bar plot of most frequent category for each categorical column
x <- inspect_imb(dattrainOr)
show_plot(x)

# Bar plot showing memory usage for each column
x <- inspect_mem(dattrainOr)
show_plot(x)

# Occurence of NAs in each column ranked in descending order
x <- inspect_na(dattrainOr)
show_plot(x)

# Histograms for numeric columns
x <- inspect_num(dattrainOr)
show_plot(x)

# Barplot of column types
x <- inspect_types(dattrainOr)
show_plot(x)

# EDA - Results
# Mirar 0s construction_year
# gps_height 0 y negativos
# longitude valores de 0 ???
# population -> outliers? (300.000)
# 27 categÃ³ricas - 10 numÃ©ricas
# recorded_by es una sola categoria -> cte. (a quitar)

#---- Corregir estas deficiencias.
# Vamos a hacer un modelo con las variables numericas
# Vamos a la playa!
datnum_df <- dattrainOr %>% select(is.numeric)

# Variable objetivo?
# Ante la duda del mismo orden de etiquetas - hago merge
dattrainOrlab <- merge(
  datnum_df, dattrainLabOr,
  by.x = c('id'), by.y = c('id'),
  sort = FALSE
)


#-------------------- 
#-------Modelo
# No hiperparamtrizo - La playa!. Cuidado con Optimizar demasiado pronto.
# Incluyo el parametro de importancia para saber importancia de varialbes
dattrainOrlab$status_group <- as.factor(dattrainOrlab$status_group)
my_model <- ranger( 
  status_group ~ . , 
  importance = 'impurity',
  data = dattrainOrlab
)
# **Estimacion** del error / acierto **esperado**
acierto <- 1 - my_model$prediction.error
acierto

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
ggsave('./charts/00_base_solo_num.png')

#------------ Prediccion
my_pred <- predict(my_model, dattestOr)

#------ Submission
my_sub <- data.table(
  id = dattestOr$id,
  status_group = my_pred$predictions
)
# guardo submission
fwrite(my_sub, file = "./submissions/00_solo_num.csv" )


#------ Resultados
# 00 - 0.7127946 - solo num - 0.7145

