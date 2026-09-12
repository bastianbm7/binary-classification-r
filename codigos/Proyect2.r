

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

# Fix: verificar_paquetes() nunca fijaba un mirror de CRAN, así que si de
# verdad faltaba un paquete, install.packages() fallaba con "trying to use
# CRAN without setting a mirror" en vez de instalarlo -- necesario para que
# esto funcione en una máquina nueva sin configuración previa de R.
options(repos = c(CRAN = "https://cloud.r-project.org"))

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
df <- read.xlsx('../datos/bases/GUIA-1.xlsx')
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
# Variables categóricas codificadas como números en el Excel original
# (moved here from further down so the pairs plot below can use the
# "numéricas reales" complement instead of excluir columnas por número).
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
varNumericas_toScale <- subset(varNumericas, select = -c(1, ncol(varNumericas)))  # sin NOMBRE ni RESULTADO
# (el escalado real, que necesita la función definida más abajo, se calcula
# donde ya estaba -- ver "varNumericas_scaled <- estandarizar_datos(...)")

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
ggsave("../datos/resultados/pieChart.jpg", width = 10, height = 6, units = "in", plot = piePlots, dpi = 300)


# Gráfico pairs
# Antes: excluía columnas por número de posición (frágil e ilegible -- las
# etiquetas de los ejes quedaban truncadas y no quedaba claro qué variables
# se estaban graficando). Ahora: usa varNumericas_toScale, que ya es
# exactamente el complemento de varCategoricas sin NOMBRE ni RESULTADO --
# es decir, las variables genuinamente numéricas del dataset -- y colorea
# por RESULTADO para que el gráfico también sirva de EDA de la respuesta.
# Nota: dev.copy(png, ...) (el enfoque original) copia lo que haya en el
# dispositivo gráfico "activo" -- que en modo no interactivo (Rscript, como
# se corre aquí) normalmente no existe, así que el jpg quedaba en blanco.
# Abrir el dispositivo PNG explícitamente antes de graficar es lo que
# realmente funciona corriendo el script de punta a punta sin RStudio.
png("../datos/resultados/pairsPlot.jpg", width = 1400, height = 1400, res = 150)
pairs(varNumericas_toScale,
      main = "Dispersión de variables numéricas (color = Resultado)",
      col = ifelse(df$RESULTADO == 1, rgb(0.2, 0.4, 0.8, 0.5), rgb(0.8, 0.3, 0.2, 0.5)),
      pch = 19, cex = 0.6,
      labels = c("Edad", "Años\nclase", "Exp. acad.\nanterior", "Años\nacademia",
                 "Años\nprofesional", "Decil\nScimago", "Decil\nQS", "N°\ntítulos", "N°\nidiomas"))
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
ggsave("../datos/resultados/barpPlot.jpg", width = 10, height = 6, units = "in", plot = barPlots, dpi = 300)
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
logistic <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba, umbral = 0.31) {
  set.seed(15)
  # Crear un modelo de regresión logística
  modelo_logistico <- glm(etiquetas_entrenamiento ~ ., data = datos_entrenamiento, family = binomial())

  # Realizar predicciones en los datos de prueba
  raw <- predict(modelo_logistico, newdata = datos_prueba, type = "response")
  predicciones <- ifelse(raw >= umbral, 1, 0)

  # Se devuelve tanto el score continuo (raw, para la curva ROC real con
  # barrido de umbrales) como la predicción ya umbralizada (para accuracy/CM).
  return(list(raw = raw, pred = predicciones))
}

######################################################
# Función para aplicar el modelo XGBoost
xgBoost <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba, umbral = 0.15) {
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

  raw <- predict(modelo, dtest)
  predicciones <- ifelse(raw >= umbral, 1, 0)

  return(list(raw = raw, pred = predicciones))
}

######################################################
# Función para aplicar el modelo SVC
SVC <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba, umbral = 0.1) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)

  # Entrenar el modelo SVC
  modelo <- svm(etiquetas_entrenamiento ~ ., data = entrenamiento)

  # Realizar predicciones en los datos de prueba
  raw <- predict(modelo, newdata = datos_prueba)
  predicciones <- ifelse(raw >= umbral, 1, 0)

  return(list(raw = raw, pred = predicciones))
}

######################################################
# Función para aplicar el modelo de Redes Neuronales
redNeuronal <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba, umbral = 0.005) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)

  # Entrenar el modelo de Redes Neuronales
  modelo_red <- nnet(etiquetas_entrenamiento ~ ., data = entrenamiento, size = 17)

  # Realizar predicciones en los datos de prueba
  raw <- predict(modelo_red, newdata = datos_prueba)
  predicciones <- ifelse(raw >= umbral, 1, 0)
  return(list(raw = as.numeric(raw), pred = predicciones))
}

######################################################
# Función para aplicar el modelo randomForest
randomForestSRC <- function(datos_entrenamiento, etiquetas_entrenamiento, datos_prueba, umbral = 0.3) {
  set.seed(15)
  # Combina los datos de entrenamiento y etiquetas en un único dataframe
  entrenamiento <- cbind(datos_entrenamiento, etiquetas_entrenamiento)

  # Entrena el modelo Random Forest
  modelo_rf <- rfsrc(etiquetas_entrenamiento ~ ., data = entrenamiento)

  # Realiza predicciones en los datos de prueba
  raw <- predict(modelo_rf, newdata = datos_prueba)$predicted
  predicciones <- ifelse(raw >= umbral, 1, 0)
  return(list(raw = raw, pred = predicciones))
}

######################################################
# Función para medir el rendimiento del modelo
# Antes: la "curva ROC" se calculaba con roc(etiquetas, PREDICCIONES_YA_UMBRALIZADAS
# 0/1) -- eso solo puede dar 3 puntos posibles (FPR/TPR de la única predicción
# binaria, más los extremos 0,0 y 1,1), no una curva real. Ahora recibe también
# el score continuo (raw) y calcula la curva de verdad, con barrido completo de
# umbrales vía pROC -- y de paso reporta el umbral óptimo (Youden) al lado del
# umbral que efectivamente usa cada clasificador, para poder justificarlo o no.
medir_rendimiento <- function(predicciones, raw_scores, etiquetas_reales, clasificador, umbral_usado) {
  predicciones <- factor(predicciones, levels = c(0, 1))
  etiquetas_reales_f <- factor(etiquetas_reales, levels = c(0, 1))

  resultados <- confusionMatrix(predicciones, etiquetas_reales_f)
  accuracy <- resultados$overall["Accuracy"]
  precision <- resultados$byClass["Precision"]
  recall <- resultados$byClass["Recall"]
  specificity <- resultados$byClass["Specificity"]
  f1_score <- resultados$byClass["F1"]

  # Curva ROC real (barrido de umbrales sobre el score continuo)
  roc_obj <- roc(etiquetas_reales, raw_scores, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  optimo <- coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"),
                    best.method = "youden", transpose = FALSE)
  # coords() puede devolver más de una fila si hay empate en el estadístico de Youden
  umbral_optimo <- optimo$threshold[1]

  # Curva de aprendizaje
  training_sizes <- seq(0.1, 1, by = 0.1) * length(etiquetas_reales)
  accuracy_values <- numeric(length(training_sizes))

  for (i in seq_along(training_sizes)) {
    subset_indices <- sample(length(etiquetas_reales), training_sizes[i])
    subset_predicciones <- predicciones[subset_indices]
    subset_etiquetas_reales <- etiquetas_reales_f[subset_indices]

    accuracy_values[i] <- sum(subset_predicciones == subset_etiquetas_reales) / length(subset_etiquetas_reales)
  }

  df_learning <- data.frame(Training_Size = training_sizes, Accuracy = accuracy_values)

  df_resultados <- data.frame(clasificador, accuracy, precision, recall, specificity, f1_score,
                               auc = round(auc_val, 3),
                               umbral_usado = umbral_usado,
                               umbral_optimo_youden = round(umbral_optimo, 4))

  return(list(df_resultados, roc_obj, df_learning, resultados))
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

  # Aplicar modelo -- cada función ahora devuelve list(raw=, pred=)
  if (model == 'logistic'){
    salida <- logistic(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  if (model == 'xgBoost'){
    salida <- xgBoost(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  if (model == 'SVC'){
    salida <- SVC(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  if (model == 'redNeuronal'){
    salida <- redNeuronal(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  if (model == 'AdaBoost'){
    salida <- adaBoost(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  if (model == 'RandomForest'){
    salida <- randomForestSRC(splitData$X_train, splitData$y_train, splitData$X_test)
  }

  # Crear una lista con las predicciones (raw + umbralizadas) y los valores de splitData
  resultado <- list(Predicciones = salida$pred, Raw = salida$raw, SplitData = splitData)

  return(resultado)
}


################################################
# varCategoricas / varNumericas / varNumericas_toScale ya se definieron antes
# del EDA (para el pairs plot); acá solo falta escalarlas.
varNumericas_scaled <- estandarizar_datos(varNumericas_toScale)


################################################
# Apply model Logistic
model <- 'logistic'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)

# Obtener métricas del modelo
clasificador <- 'Regresión Logística'
resultados <- medir_rendimiento(resultados$Predicciones, resultados$Raw, resultados$SplitData$y_test,
                                 clasificador, umbral_usado = 0.31)
dfResults_log <- resultados[[1]]
dfROC_log <- resultados[[2]]
dfLearning_log <- resultados[[3]]
cm_log <- resultados[[4]]

################################################
# Apply model xgBoost
model <- 'xgBoost'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)

# Obtener métricas del modelo
clasificador <- 'xgBoost'
resultados <- medir_rendimiento(resultados$Predicciones, resultados$Raw, resultados$SplitData$y_test,
                                 clasificador, umbral_usado = 0.15)
dfResults_xgBoost <- resultados[[1]]
dfROC_xgBoost <- resultados[[2]]
dfLearning_xgBoost <- resultados[[3]]
cm_xgBoost <- resultados[[4]]

################################################
# Apply model SVC
model <- 'SVC'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)

# Obtener métricas del modelo
clasificador <- 'SVC'
resultados <- medir_rendimiento(resultados$Predicciones, resultados$Raw, resultados$SplitData$y_test,
                                 clasificador, umbral_usado = 0.1)
dfResults_SVC <- resultados[[1]]
dfROC_SVC <- resultados[[2]]
dfLearning_SVC <- resultados[[3]]
cm_SVC <- resultados[[4]]

################################################
# Apply model redNeuronal
model <- 'redNeuronal'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)

# Obtener métricas del modelo
clasificador <- 'Red Neuronal'
resultados <- medir_rendimiento(resultados$Predicciones, resultados$Raw, resultados$SplitData$y_test,
                                 clasificador, umbral_usado = 0.005)
dfResults_NN <- resultados[[1]]
dfROC_NN <- resultados[[2]]
dfLearning_NN <- resultados[[3]]
cm_NN <- resultados[[4]]


################################################
# Apply model RandomForest
model <- 'RandomForest'
resultados <- modelPipeline(df, model, varCategoricas, varNumericas)

# Obtener métricas del modelo
clasificador <- 'Random Forest'
resultados <- medir_rendimiento(resultados$Predicciones, resultados$Raw, resultados$SplitData$y_test,
                                 clasificador, umbral_usado = 0.3)
dfResults_rf <- resultados[[1]]
dfROC_rf <- resultados[[2]]
dfLearning_rf <- resultados[[3]]
cm_rf <- resultados[[4]]

################################################
# Tabla de umbrales: el usado por cada clasificador vs. el óptimo real
# (estadístico de Youden sobre la curva ROC) -- responde a por qué cada
# modelo usa un umbral tan distinto de los demás en vez de el 0.5 típico.
tabla_umbrales <- rbind(dfResults_log, dfResults_xgBoost, dfResults_SVC,
                         dfResults_NN, dfResults_rf)[, c("clasificador", "auc", "umbral_usado", "umbral_optimo_youden")]
print(tabla_umbrales)
write.csv(tabla_umbrales, "../datos/resultados/tabla_umbrales.csv", row.names = FALSE)


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

ggsave("../datos/resultados/learningRate.jpg", width = 10, height = 8, units = "in", plot = learnRate, dpi = 300)
################################################
# Graficar la curva ROC
# Antes: dfROC_* eran tablas de 2-3 puntos (coords() sobre predicciones ya
# umbralizadas 0/1 -- ver medir_rendimiento), así que esto dibujaba un
# triángulo, no una curva. Ahora dfROC_* son objetos `roc` reales (barrido
# completo de umbrales sobre el score continuo), así que se puede usar
# ggroc() directamente, con el AUC real de cada modelo en la leyenda.
roc_list <- list(
  Logistic = dfROC_log,
  xgBoost = dfROC_xgBoost,
  SVC = dfROC_SVC,
  `Red Neuronal` = dfROC_NN,
  `Random Forest` = dfROC_rf
)
auc_labels <- sapply(names(roc_list), function(n) sprintf("%s (AUC=%.3f)", n, auc(roc_list[[n]])))
names(roc_list) <- auc_labels

rocPlot <- ggroc(roc_list, linewidth = 1, alpha = 0.85) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey40") +
    labs(title = 'Curva ROC (barrido completo de umbrales, no 3 puntos)',
      x = 'Especificidad',
      y = 'Sensibilidad', color = "Modelo") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 16, face = 'bold'),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 15),
      legend.key.size = unit(2, "lines"),
      legend.spacing.y = unit(0.8, "lines"),
      plot.title = element_text(size = 20))


ggsave("../datos/resultados/rocCurve.jpg", width = 10, height = 8, units = "in", plot = rocPlot, dpi = 300)

################################################
# Conclusión: techo de accuracy y por qué es ruidoso
# El 70%/30% MALO/BUENO de la variable respuesta implica que un modelo trivial
# que siempre prediga "MALO" ya acierta ~70% -- cualquier accuracy cercano a
# ese número no es evidencia de que el modelo esté aprendiendo algo real.
baseline_mayoria <- max(table(df$RESULTADO)) / nrow(df)
cat(sprintf("\nBaseline (predecir siempre la clase mayoritaria): %.1f%%\n", baseline_mayoria * 100))
cat("Accuracy por modelo vs. ese baseline:\n")
print(tabla_umbrales)
cat(sprintf(
  "\nCon solo %d observaciones de test (30%% de %d filas), cada observación mal\n",
  round(nrow(df) * 0.3), nrow(df)))
cat("clasificada mueve el accuracy más de un punto porcentual -- de ahí el ruido.\n")
cat("Además, modelPipeline() llama a dividir_datos() una vez POR MODELO: los 5\n")
cat("clasificadores de la tabla de arriba NO se evalúan sobre el mismo split de\n")
cat("test (dividir_datos() no fija su propia semilla, solo cada modelo fija la\n")
cat("suya para su propio entrenamiento) -- comparar sus accuracy es comparar\n")
cat("contra 5 muestras de test distintas, otra fuente real de ruido entre modelos.\n")

#
