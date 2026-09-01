

# Recuerda cambiar a tu directorio
setwd('PATH')

###########################################################################

# Función para instalar las librerías necesarias para realizar el proyecto.
# No se instalarán en el caso de que ya se encuentren instaladas.
verificar_paquetes <- function(paquetes) {
  paquetes_faltantes <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
  
  if (length(paquetes_faltantes) > 0) {
    mensaje <- paste("Los siguientes paquetes están ausentes y serán instalados:",
                     paste(paquetes_faltantes, collapse = ", "))
    print(mensaje)
    
    for (paquete in paquetes_faltantes) {
      install.packages(paquete, dependencies = TRUE)
    }
    
    print("Instalación de paquetes completada.")
  } else {
    print("Todos los paquetes requeridos están instalados.")
  }
}

paquetes <- c('ggplot2', "xgboost", "caret", "tidyverse", 
              'openxlsx', 'pROC', 'e1071',
              'nnet', 'randomForestSRC', 'gridExtra')

# Verificar e instalar los paquetes necesarios
verificar_paquetes(paquetes)

library(ggplot2)
library(gridExtra)
library(pROC)
library(xgboost)
library(caret)
library(tidyverse)
library(openxlsx)
library(e1071)
library(nnet)
library(randomForestSRC)
######################################################
# Leer los datos
df <- read.xlsx('GUIA-1.xlsx')
head(df)

######################################################
# Observar la estructura de los datos
str(df) 
names(df)

# Valores únicos
unique(df$AREA.DE.LA.PROFESIO)
unique(df$RESULTADO) # Cambiar valores a 0 y 1
unique(df$CATEGORIA.U)
unique(df$PROFESION)

# Contar Datos faltantes
colSums(is.na(df)) 

# Dimensión del dataframe
dim(df)

# Los datos no necesitan limpieza, dado que los tipos de datos
# son consistentes, no hay valores perdidos ni repetidos y no
# se observan valores mal ingresados. 

# Ahora bien, hay que cambiar los valores de la variable
# 'RESULTADO' a 0 y 1 para ingresar el algoritmo correctamente:

df$RESULTADO <- ifelse(df$RESULTADO == "BUENO", 1, 0)
######################################################
# REALIZAR EDA

# Gráficos de torta
proporciones <- df %>%
  group_by(RESULTADO) %>%
  summarise(n = n()) %>%
  mutate(proporcion = n / sum(n))

piePlot1 <- ggplot(proporciones, aes(x = "", y = proporcion, fill = factor(RESULTADO))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Valores", x = NULL, y = NULL, title = "Proporción de variable\nrespuesta") +
  geom_text(aes(label = paste0(round(proporcion, 4) * 100, "%")), 
          position = position_stack(vjust = 0.5), 
          size = 10) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(1, "cm"))

# Genero
proporciones <- df %>%
  group_by(GENERO) %>%
  summarise(n = n()) %>%
  mutate(proporcion = n / sum(n))

piePlot2 <- ggplot(proporciones, aes(x = "", y = proporcion, fill = factor(GENERO))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Valores", x = NULL, y = NULL, title = "Proporción de variable\ngénero") +
  geom_text(aes(label = paste0(round(proporcion, 4) * 100, "%")), 
          position = position_stack(vjust = 0.5), 
          size = 10) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.spacing.x = unit(1, "cm"))

piePlots <- grid.arrange(piePlot1, piePlot2, widths = c(0.5, 0.5), ncol = 2)
ggsave("pieChart.jpg", width = 10, height = 6, units = "in", plot = piePlots, dpi = 300)


# Gráfico pairs
pairs(select(df, c(-1, -3, -10, -4, -9, 
                    -15, -22, -11, -12, -17, 
                    -18, -20, -21, -23, -24, 
                    -25, -13, -14, -16, -19)), 
      main = "Dispersión de datos")
dev.copy(png, "pairsPlot.jpg")
dev.off()


# Bar plots
barPlot1 <- ggplot(df, aes(x = EDAD, fill = factor(RESULTADO))) +
  geom_bar() +
    labs(title = 'Cantidad de personas por\nedad según resultado', 
      x = 'Edad', 
      y = 'Cantidad',
      fill = 'Resultado') +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = 'bold'),
      axis.title = element_text(size = 15),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 18))


barPlot2 <- ggplot(df, aes(x = NACIONALIDAD, fill = factor(RESULTADO))) +
  geom_bar(position = "dodge") +
    labs(title = 'Cantidad de personas por\nnacionalidad según resultado', 
      x = 'Nacionalidad', 
      y = 'Cantidad',
      fill = 'Resultado') +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = 'bold'),
      axis.title = element_text(size = 15),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 18))

barPlot3 <- ggplot(df, aes(x = ETNIA, fill = factor(RESULTADO))) +
  geom_bar(position = "dodge") +
    labs(title = 'Cantidad de personas por etnia\nsegún resultado', 
      x = 'Etnia', 
      y = 'Cantidad',
      fill = 'Resultado') +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12, face = 'bold'),
      axis.title = element_text(size = 15),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 18))


barPlots <- grid.arrange(barPlot1, barPlot2, barPlot3, widths = c(0.33, 0.33, 0.34), ncol = 3)
ggsave("barpPlot.jpg", width = 10, height = 6, units = "in", plot = barPlots, dpi = 300)
######################################################

# Función para estandarizar los datos
estandarizar_datos <- function(datos, tipo_estandarizacion = "standardize") {
  if (tipo_estandarizacion == "standardize") {
    datos_estandarizados <- preProcess(datos, method = c("center", "scale"))
  } else if (tipo_estandarizacion == "normalize") {
    datos_estandarizados <- preProcess(datos, method = "range")
  } else {
    stop("Tipo de estandarización no válido. Debe ser 'standardize' o 'normalize'.")
  }
  
  datos_estandarizados <- predict(datos_estandarizados, datos)
  
  return(datos_estandarizados)
}

######################################################
# Función para dividir los datos en entrenamiento y prueba
dividir_datos <- function(datos, etiquetas, proporcion_entrenamiento = 0.7) {
  indices_entrenamiento <- sample(1:nrow(datos), floor(proporcion_entrenamiento * nrow(datos)))
  
  datos_entrenamiento <- datos[indices_entrenamiento, ]
  etiquetas_entrenamiento <- etiquetas[indices_entrenamiento]
  
  datos_prueba <- datos[-indices_entrenamiento, ]
  etiquetas_prueba <- etiquetas[-indices_entrenamiento]
  
  lista_resultados <- list(X_train = datos_entrenamiento,
                           y_train = etiquetas_entrenamiento,
                           X_test = datos_prueba,
                           y_test = etiquetas_prueba)
  
  return(lista_resultados)
}

######################################################
# Función para aplicar el modelo logistic
logistic <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba) {
  set.seed(15)
  # Crear un modelo de regresión logística
  modelo_logistico <- glm(etiquetas_entrenamiento ~ ., data = datos_entrenamiento, family = binomial())

  # Realizar predicciones en los datos de prueba
  predicciones <- predict(modelo_logistico, newdata = datos_prueba, type = "response")
  predicciones <- ifelse(predicciones >= 0.31, 1, 0)
  
  return(predicciones)
}

######################################################
# Función para aplicar el modelo XGBoost
xgBoost <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba) {
  set.seed(15)
  datos_entrenamiento <- as.matrix(datos_entrenamiento)
  datos_prueba <- as.matrix(datos_prueba)
  
  dtrain <- xgb.DMatrix(data = datos_entrenamiento, label = etiquetas_entrenamiento)
  dtest <- xgb.DMatrix(data = datos_prueba)
  
  # Definir los parámetros del modelo
  params <- list(
    objective = "binary:logistic",
    eval_metric = "error"
  )
  
  # Entrenar el modelo Gradient Boosting
  modelo <- xgb.train(params, dtrain, nrounds = 100)
  
  predicciones <- predict(modelo, dtest)
  predicciones <- ifelse(predicciones >= 0.15, 1, 0)
  
  return(predicciones)
}

######################################################
# Función para aplicar el modelo SVC
SVC <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)
   
  # Entrenar el modelo SVC
  modelo <- svm(etiquetas_entrenamiento ~ ., data = entrenamiento)

  # Realizar predicciones en los datos de prueba
  predicciones <- predict(modelo, newdata = datos_prueba)
  predicciones <- ifelse(predicciones >= 0.1, 1, 0)
  
  return(predicciones)
}

######################################################
# Función para aplicar el modelo de Redes Neuronales
redNeuronal <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)
   
  # Entrenar el modelo de Redes Neuronales
  modelo_red <- nnet(etiquetas_entrenamiento ~ ., data = entrenamiento, size = 17)
  
  # Realizar predicciones en los datos de prueba
  predicciones <- predict(modelo_red, newdata = datos_prueba)
  predicciones <- ifelse(predicciones >= 0.005, 1, 0)
  # Devolver las predicciones
  return(predicciones)
}

######################################################
# Función para aplicar el modelo randomForest
randomForestSRC <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)
  
  # Entrena el modelo Random Forest
  modelo_rf <- rfsrc(etiquetas_entrenamiento ~ ., data = entrenamiento)
    
  # Realiza predicciones en los datos de prueba
  predicciones <- predict(modelo_rf, newdata = datos_prueba)$predicted
  predicciones <- ifelse(predicciones >= 0.3, 1, 0)
  return(predicciones)
}

######################################################
# Función para medir el rendimiento del modelo
medir_rendimiento <- function(predicciones, etiquetas_reales, clasificador) {
  predicciones <- factor(predicciones, levels = c(0, 1))
  etiquetas_reales <- factor(etiquetas_reales, levels = c(0, 1))
  
  resultados <- confusionMatrix(predicciones, etiquetas_reales)
  accuracy <- resultados$overall["Accuracy"]
  precision <- resultados$byClass["Precision"]
  recall <- resultados$byClass["Recall"]
  specificity <- resultados$byClass["Specificity"]
  f1_score <- resultados$byClass["F1"]
  
  # Curva ROC
  roc_obj <- roc(as.numeric(etiquetas_reales) - 1, as.numeric(predicciones) - 1)
  datos_roc <- coords(roc_obj, "all")

  # Curva de aprendizaje
  training_sizes <- seq(0.1, 1, by = 0.1) * length(etiquetas_reales)
  accuracy_values <- numeric(length(training_sizes))
  
  for (i in seq_along(training_sizes)) {
    subset_indices <- sample(length(etiquetas_reales), training_sizes[i])
    subset_predicciones <- predicciones[subset_indices]
    subset_etiquetas_reales <- etiquetas_reales[subset_indices]
    
    accuracy_values[i] <- sum(subset_predicciones == subset_etiquetas_reales) / length(subset_etiquetas_reales)
  }
  
  df_learning <- data.frame(Training_Size = training_sizes, Accuracy = accuracy_values)
  
  df_resultados <- data.frame(clasificador, accuracy, precision, recall, specificity, f1_score)
  
  return(list(df_resultados, datos_roc, df_learning, resultados))
}

######################################################
# Función para aplicar PCA
performPCA <- function(data, n_components = ncol(data)) {
  # Realizar el PCA
  pca <- prcomp(data, center = TRUE, scale. = TRUE, retx = TRUE, rank. = n_components)
  
  # Obtener los componentes principales
  components <- pca$x
  
  # Obtener la varianza explicada
  variance <- pca$sdev^2
  
   # Obtener la proporción de varianza explicada
  explained_variance_ratio <- cumsum(variance / sum(variance))
  
  # Crear un data frame con los resultados
  result <- data.frame(component = seq(1, length(explained_variance_ratio)), 
              varianceExplaines = explained_variance_ratio)

  return(components)
}

################################################
# Función para aplicar modelo
modelPipeline <- function(df, model, varNum, varCat){
  y <- df[, ncol(df)]
  X <- cbind(varNumericas_scaled, df[varCategoricas])

  # Usar las primeras 15 componentes del PCA
  result <- performPCA(X)
  X <- data.frame(result[, 1:15])

  # Dividir datos
  splitData <- dividir_datos(X, y, proporcion_entrenamiento = 0.7)
  
  # Aplicar modelo
  if (model == 'logistic'){
    predicciones <- logistic(splitData$X_train, splitData$y_train, splitData$X_test)
  } 

  if (model == 'xgBoost'){
    predicciones <- xgBoost(splitData$X_train, splitData$y_train, splitData$X_test)
  } 
  
  if (model == 'SVC'){
    predicciones <- SVC(splitData$X_train, splitData$y_train, splitData$X_test)
  } 

  if (model == 'redNeuronal'){
    predicciones <- redNeuronal(splitData$X_train, splitData$y_train, splitData$X_test)
  } 

  if (model == 'AdaBoost'){
    predicciones <- adaBoost(splitData$X_train, splitData$y_train, splitData$X_test)
  } 

  if (model == 'RandomForest'){
    predicciones <- randomForestSRC(splitData$X_train, splitData$y_train, splitData$X_test)
  } 

  # Crear una lista con las predicciones y los valores de splitData
  resultado <- list(Predicciones = predicciones, SplitData = splitData)

  return(resultado)
}


################################################
# Seleccionar todas las variables numéricas y categóricas sin el NOMBRE
# ni RESULTADO para escalar las columnas numéricas
varCategoricas <- c("NACIONALIDAD", 
                        "AREA.DEL.TITULO.2",
                        'GENERO',
                        'CATEGORIA.U',
                        'PhD',
                        'IDIOMA.AVANZADO',
                        'CATEGORIA.DEL.TITULO.(.TERCER.NIVEL./MASTER/PHD)', 
                        "CONTINENTE.DE.LA.UNIVERSIDAD.DEL.TITULO.DE.MAYOR.NIVEL", 
                        "ESTADO.CIVIL", 
                        "ETNIA",
                        'IDIOMA.PRINCIPIANTE',
                        'IDIOMA.INTERMEDIO',
                        'PROFESION',
                        'AREA.DE.LA.PROFESION')

varNumericas <- df[, !(names(df) %in% varCategoricas)]


varNumericas_toScale = subset(varNumericas, select = -c(1, ncol(varNumericas)))
varNumericas_scaled <- estandarizar_datos(varNumericas_toScale)


################################################
# Apply model Logistic
model <- 'logistic'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)
prediccionesLog <- resultados[1]
splitLog <- resultados[2]

# Obtener métricas del modelo
clasificador <- 'Regresión Logística'
resultados <- medir_rendimiento(prediccionesLog$Predicciones, splitLog$SplitData$y_test, clasificador)
dfResults_log <- resultados[[1]]
dfROC_log <- resultados[[2]]
dfLearning_log <- resultados[[3]]
cm_log <- resultados[[4]]

################################################
# Apply model xgBoost
model <- 'xgBoost'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)
prediccionesXgBoost <- resultados[1]
splitXgBoost <- resultados[2]

# Obtener métricas del modelo
clasificador <- 'xgBoost'
resultados <- medir_rendimiento(prediccionesXgBoost$Predicciones, splitXgBoost$SplitData$y_test, clasificador)
dfResults_xgBoost <- resultados[[1]]
dfROC_xgBoost <- resultados[[2]]
dfLearning_xgBoost <- resultados[[3]]
cm_xgBoost <- resultados[[4]]

################################################
# Apply model SVC
model <- 'SVC'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)
prediccionesSVC <- resultados[1]
splitSVC <- resultados[2]

# Obtener métricas del modelo
clasificador <- 'SVC'
prediccion <- (prediccionesSVC)
splitdata <- (splitSVC)

resultados <- medir_rendimiento(prediccion$Predicciones, splitdata$SplitData$y_test, clasificador)
dfResults_SVC <- resultados[[1]]
dfROC_SVC <- resultados[[2]]
dfLearning_SVC <- resultados[[3]]
cm_SVC <- resultados[[4]]

################################################
# Apply model redNeuronal
model <- 'redNeuronal'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)
prediccionesNN <- resultados[1]
splitNN <- resultados[2]

# Obtener métricas del modelo
clasificador <- 'Red Neuronal'
resultados <- medir_rendimiento(prediccionesNN$Predicciones, splitNN$SplitData$y_test, clasificador)
dfResults_NN <- resultados[[1]]
dfROC_NN <- resultados[[2]]
dfLearning_NN <- resultados[[3]]
cm_NN <- resultados[[4]]


################################################
# Apply model RandomForest
model <- 'RandomForest'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)
prediccionesRF <- resultados[1]
splitRF <- resultados[2]

# Obtener métricas del modelo
clasificador <- 'Random Forest'
resultados <- medir_rendimiento(prediccionesRF$Predicciones, splitRF$SplitData$y_test, clasificador)
dfResults_rf <- resultados[[1]]
dfROC_rf <- resultados[[2]]
dfLearning_rf <- resultados[[3]]
cm_rf <- resultados[[4]]


################################################
# Tabla con las métricas de desempeño
metrics <- rbind(dfResults_log, dfResults_xgBoost, 
      dfResults_SVC, dfResults_NN, 
      dfResults_rf)
metrics
################################################
# Graficar la curva de aprendizaje
learnRate <- ggplot() +
    geom_point(data = dfLearning_log, aes(x = Training_Size, y = Accuracy, color = "Logistic"), size = 5, alpha = 1.5) +
    geom_line(data = dfLearning_log, aes(x = Training_Size, y = Accuracy, color = "Logistic"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfLearning_xgBoost, aes(x = Training_Size, y = Accuracy, color = "xgBoost"), size = 5, alpha = 1.5) +
    geom_line(data = dfLearning_xgBoost, aes(x = Training_Size, y = Accuracy, color = "xgBoost"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfLearning_SVC, aes(x = Training_Size, y = Accuracy, color = "SVC"), size = 5, alpha = 1.5) +
    geom_line(data = dfLearning_SVC, aes(x = Training_Size, y = Accuracy, color = "SVC"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfLearning_NN, aes(x = Training_Size, y = Accuracy, color = "Red Neuronal"), size = 5, alpha = 1.5) +
    geom_line(data = dfLearning_NN, aes(x = Training_Size, y = Accuracy, color = "Red Neuronal"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfLearning_rf, aes(x = Training_Size, y = Accuracy, color = "Random Forest"), size = 5, alpha = 1.5) +
    geom_line(data = dfLearning_rf, aes(x = Training_Size, y = Accuracy, color = "Random Forest"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    labs(title = 'Nivel de exactitud según el tamaño de\nentrenamiento por cada modelo', 
      x = 'Tamaño de entrenamiento', 
      y = 'Accuracy', color = "Modelo") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 16, face = 'bold'),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.key.size = unit(2, "lines"),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 25))

ggsave("learningRate.jpg", width = 10, height = 8, units = "in", plot = learnRate, dpi = 300)
################################################
# Graficar la curva ROC
rocPlot <- ggplot() +
    geom_point(data = dfROC_log, aes(x = 1 - specificity, y = sensitivity, color = "Logistic"), size = 5, alpha = 1.5) +
    geom_line(data = dfROC_log, aes(x = 1 - specificity, y = sensitivity, color = "Logistic"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfROC_xgBoost, aes(x = 1 - specificity, y = sensitivity, color = "xgBoost"), size = 5, alpha = 1.5) +
    geom_line(data = dfROC_xgBoost, aes(x = 1 - specificity, y = sensitivity, color = "xgBoost"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfROC_SVC, aes(x = 1 - specificity, y = sensitivity, color = "SVC"), size = 5, alpha = 1.5) +
    geom_line(data = dfROC_SVC, aes(x = 1 - specificity, y = sensitivity, color = "SVC"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfROC_NN, aes(x = 1 - specificity, y = sensitivity, color = "Red Neuronal"), size = 5, alpha = 1.5) +
    geom_line(data = dfROC_NN, aes(x = 1 - specificity, y = sensitivity, color = "Red Neuronal"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_point(data = dfROC_rf, aes(x = 1 - specificity, y = sensitivity, color = "Random Forest"), size = 5, alpha = 1.5) +
    geom_line(data = dfROC_rf, aes(x = 1 - specificity, y = sensitivity, color = "Random Forest"), linetype = 'solid', linewidth = 1, alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = 'Curva ROC', 
      x = 'Tasa de Falsos Positivos', 
      y = 'Tasa de Verdaderos Positivos', color = "Modelo") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 16, face = 'bold'),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.key.size = unit(2, "lines"),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 25))


ggsave("rocCurve.jpg", width = 10, height = 8, units = "in", plot = rocPlot, dpi = 300)

#
