library("dplyr")
library("ggplot2")
library("VIM")

# Introducción

# 1. Carga del archivo

data <- read.csv("train3.csv", header=TRUE, sep=";", dec=".")

summary(data[c("ClaimNumber","DateTimeOfAccident","Gender","Age")])


# 2. Duplicación de códigos

# valores duplicados
data$ClaimNumber[duplicated(data$ClaimNumber) == TRUE] 

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

data$ClaimNumberId <- substrRight(data$ClaimNumber, 7)
data$ClaimNumberId <- as.integer(data$ClaimNumberId)

maxClaimNumberId <- summary(data$ClaimNumberId)[["Max."]]
nduplicated <- length(data$ClaimNumber[duplicated(data$ClaimNumber) == TRUE])
for (i in 1:nduplicated) {
  data$ClaimNumberId[duplicated(data$ClaimNumberId) == TRUE][1] <- maxClaimNumberId + i
}

# pegar 'WC' de vuelta al número como paso previo a incorporarlo a la variable original
data$ClaimNumber[duplicated(data$ClaimNumber) == TRUE] <- paste(
  "WC",                                                   
  data$ClaimNumberId[duplicated(data$ClaimNumber) == TRUE], 
  sep=""
)
# borramos la variable auxiliar
data <- subset(data, select = -c(ClaimNumberId))

# 3. Nombres de las variables

names(data)[names(data) == "InitialIncurredCalimsCost"] <- "IniCost" #typo in colname
names(data)[names(data) == "UltimateIncurredClaimCost"] <- "UltCost"
names(data)[names(data) == "HoursWorkedPerWeek"] <- "HoursWeek"
names(data)[names(data) == "DaysWorkedPerWeek"] <- "DaysWeek"

# 4. Normalización de los datos cualitativos

## 4.1. Marital Status

unique(data$MaritalStatus)

# unificamos el formato de los valores, capitalizando el texto y sustituyendo MARRIED por M
data$MaritalStatus <- toupper(data$MaritalStatus)
data["MaritalStatus"][data["MaritalStatus"] == "MARRIED"] <- "M"
# los valores vacíos los informamos con la U, dando a entender que son desconocidos
data["MaritalStatus"][data["MaritalStatus"] == ""] <- "U"
# convertimos la variable a tipo factor, indicando que es una variable categórica
data$MaritalStatus <- factor(data$MaritalStatus)
levels(data$MaritalStatus)

## 4.2. Gender

unique(data$Gender)

# sustituir por F todo valor que comience por una F, sea mayúscula o minúscula
data$Gender[grepl("[F|f].*", data$Gender)] <- "F"
# convertimos la variable a tipo factor, indicando que es una variable categórica
data$Gender <- factor(data$Gender)
levels(data$Gender)

# 5. Normalización de los datos cuantitativos

## 5.1. IniCost y UltCost

# el resumen nos adelanta que la variable IniCost es entera (sin decimales)
summary(data$IniCost)
class(data$IniCost)

# observamos que algunos valores están expresados en miles
head(data$UltCost[grepl(".*[a-zA-Z]", data$UltCost)])
# unificamos el formato expresándolos en unidades
data$UltCost[grepl(".*[a-zA-Z]", data$UltCost)] <- as.numeric(
  sub("K", "", data$UltCost[grepl(".*[a-zA-Z]", data$UltCost)], fixed = TRUE)
)*1000
# transformamos la variable en entera (sin decimales)
data$UltCost <- as.integer(data$UltCost)
# el resumen nos sugiere que la transformación se ha realizado correctamente
summary(data$UltCost)

## 5.2. Age

data$Age <- as.integer(data$Age)
class(data$Age)

## 5.3. WeeklyWages, HoursWeek, DaysWeek

# sustituir la coma por el punto donde corresponda
data$WeeklyWages <- as.numeric(sub(",", ".", data$WeeklyWages, fixed = TRUE))
summary(data$WeeklyWages)

# la línea vertical indica la media de la variable
ggplot(data, aes(x = WeeklyWages)) + 
  geom_density() + 
  geom_vline(aes(xintercept = mean(WeeklyWages)), linetype = "dashed", size = 0.6) +
  xlab("weekly wages")

data$HoursWeek <- as.numeric(sub(",", ".", data$HoursWeek, fixed = TRUE))
summary(data$HoursWeek)

# necesitamos tener la variable ordenada
HoursWeek <- sort(data$HoursWeek)
# especificamos los cortes que queremos realizar
breaks <- c(0,10,20,30,40,60,80,max(HoursWeek))
# especificamos las etiquetas de los intervalos 
tags <- c("0-10","10-20", "20-30", "30-40", "40-50", "60-80","80<")
# agrupamos los valores bajo sus correspondientes intervalos
group_tags <- cut(HoursWeek, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# transformamos la variable en una variable categórica ordinal
group_tags <- factor(group_tags, levels=tags, ordered=TRUE)

ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(
    geom="text", 
    aes(label=sprintf("%i (%.2f)", ..count.., ..count../length(group_tags))), 
    vjust=-0.5
  ) +
  labs(x='hours week') +
  theme_minimal()

data$DaysWeek <- factor(data$DaysWeek)
levels(data$DaysWeek)

ggplot(data, mapping = aes(x=DaysWeek)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) +
  stat_count(geom="text", aes(label=sprintf("%i (%.2f)", ..count.., ..count../length(data$DaysWeek))), vjust=-0.5) +
  labs(x='days week') +
  theme_minimal()

# 6. Valores atípicos

out_tukey <- function(x){
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  
  iqr <- q3-q1 # rango = IQR(x)
  
  res <- x<(q1-1.5*iqr) | x>(q3+1.5*iqr)
  res
}

# listar valores atípicos
data$Age[out_tukey(data$Age)]
# sustituimos 999 por NA
data$Age[data$Age == 999] <- NA

# proporción de valores atípicos
length(data$WeeklyWages[out_tukey((data$WeeklyWages))])/length(data$WeeklyWages)

# sustituir
data$WeeklyWages[out_tukey((data$WeeklyWages))] <- NA

data$HoursWeek[out_tukey(data$HoursWeek)] <- NA
boxplot(data$HoursWeek, staplewex = 1)
text(
  y=boxplot.stats(data$HoursWeek)$stats, 
  labels = boxplot.stats(data$HoursWeek)$stats, 
  x = 1.25
)

# 7. Imputación de valores

# columnas con NAs
colnames(data)[apply(data, 2, anyNA)]

mean_missing <- mean(data$Age, na.rm = TRUE)
# para Age, aplicamos la imputación por la media aritmética
data <- data %>%
  mutate(Age_imp  = ifelse(is.na(Age), mean_missing, Age))

head(data[is.na(data$Age),c("Age", "Age_imp")])

# agrupamos por género
by_gender <- data %>% group_by(Gender)
# imputamos por género teniendo en cuenta las variables cuantitativas mencionadas
by_gender_imp <- kNN(
  by_gender, 
  c("WeeklyWages", "HoursWeek"), 
  dist_var = c("Age", "IniCost", "UltCost")
)
# desagrupamos el resultado
data_imp <- by_gender_imp %>% ungroup()
# eliminamos las columnas que indican la imputación con TRUE o FALSE
data_imp <- subset(data_imp, select = -c(WeeklyWages_imp, HoursWeek_imp)) 
# renombramos las columnas para indicar que han sido imputadas
names(data_imp)[names(data_imp) == "WeeklyWages"] <- "WeeklyWages_imp"
names(data_imp)[names(data_imp) == "HoursWeek"] <- "HoursWeek_imp"
data_imp <- subset(data_imp, select = 
                     c("ClaimNumber", "WeeklyWages_imp", "HoursWeek_imp"))
# añadimos las columnas imputadas al set de datos original
data <- merge(data, data_imp, by="ClaimNumber")
# verificamos que la imputación se ha realizado correctamente
sample_n(data[
  is.na(data$Age) | is.na(data$WeeklyWages) | is.na(data$HoursWeek),
  c(
    "ClaimNumber", "Age", "WeeklyWages", "HoursWeek", 
    "Age_imp", "WeeklyWages_imp", "HoursWeek_imp"
  )
], 5)

# eliminamos las columnas con valores perdidos
data <- subset(data, select = -c(Age, WeeklyWages, HoursWeek)) 
# sustituimos por las columnas imputadas
names(data)[names(data) == "Age_imp"] <- "Age"
names(data)[names(data) == "WeeklyWages_imp"] <- "WeeklyWages"
names(data)[names(data) == "HoursWeek_imp"] <- "HoursWeek"
# limpiamos las variables de entorno salvando únicamente el set de datos
envars <- ls()
rm(list = envars[envars != "data"])

# 8. Preparación de los datos

## 8.1. Tiempo de abertura del expediente

# convertimos las variables en formato Date
data$DateReported <- as.POSIXct(
  data$DateReported, 
  format="%Y-%m-%dT%H:%M:%SZ", 
  tz=Sys.timezone()
)
data$DateTimeOfAccident <- as.POSIXct(
  data$DateTimeOfAccident, 
  format="%Y-%m-%dT%H:%M:%SZ", 
  tz=Sys.timezone()
)
# realizamos el cálculo y almacenamos el resultado en una nueva variable del conjunto de datos (Time)
data$Time <- as.numeric(difftime(data$DateReported, data$DateTimeOfAccident), units = "days")

## 8.2. Diferencia entre IniCost y UltCost

data$DifCost <- data$UltCost - data$IniCost
summary(data$DifCost)

ggplot(data, aes(x = DifCost)) + 
  geom_density() + 
  geom_vline(aes(xintercept = mean(DifCost), linetype = "media"), 
             size = 0.3, show.legend = TRUE) +
  scale_linetype_manual(name = "MTC", values = c("media" = "solid")) +
  scale_x_continuous(limits = c(-8000, 8000)) +
  xlab("difcost")

# 9. Estudio descriptivo

## 9.1. Funciones de media robustas

media <- function(x) {
  mean(x)
}

media.recortada <- function(x, perc=0.05) {
  mean(x, trim=perc)
}

media.winsor <- function (x, perc=0.05) {
  lim <- quantile(x, probs=c(perc, 1-perc))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  mean(x)
}

mediana <- function(x) {
  median(x)
}

desviacion.tipica <- function(x) {
  sd(x)
}

## 9.2. Estudio descriptivo de las variables cuantitativas

resumen.tcd <- function(df, cols, fns) {
  tmp.df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  x <- cols
  colnames(tmp.df) <- x
  
  for (fun in fns) {
    tmp.row <- df %>% 
      select(all_of(cols)) %>%
      summarise(across(.fns=get(fun)))
    row.names(tmp.row) <- fun
    tmp.df <- rbind(tmp.df, tmp.row)
  }
  
  tmp.df
}

# aplicamos el formato numérico para incluirla en el análisis de manera temporal 
data$DaysWeek <- as.integer(data$DaysWeek)
resumen.cols <- c("Age", "WeeklyWages", "DaysWeek", "HoursWeek", "IniCost", "UltCost")
resumen.fns <- c("media", "mediana", "media.recortada", "media.winsor", 
                 "desviacion.tipica", "IQR", "mad")
# muestra tabla resumen con medidas de tendencia central y dispersión, robustas y no robustas
resumen.tcd(data, resumen.cols, resumen.fns)

# resumen gráfico de las medidas de tendencia central (MTC)
resumen.grf <- function(df, cols) {
  for (col in cols) {
    print(
      ggplot(df, aes(x = get(col))) + 
        geom_density() + 
        geom_vline(aes(xintercept = media(get(col)), linetype="media"), 
                   size = 0.3) +
        geom_vline(aes(xintercept = media.recortada(get(col)), linetype="media.recortada"), 
                   size = 0.3) +
        geom_vline(aes(xintercept = mediana(get(col)), linetype="mediana"), 
                   size = 0.3) +
        scale_linetype_manual(
          name = "MTC", 
          values = c("media" = "solid", "media.recortada" = "dashed", "mediana" = "dotted")
        ) +
        labs(x=tolower(col))
    )
  }
}

# resumen gráfico de las MTC por variable
resumen.grf(data, resumen.cols)
# revertimos el formato a variable categórica una vez terminado el análisis
data$DaysWeek <- factor(data$DaysWeek)
levels(data$DaysWeek)

write.csv(data, file = 'train_clean.csv', row.names = FALSE)

# Conclusión