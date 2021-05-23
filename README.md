# Datos-Hidrologicos
```{r}
# Importacion de los datos

inp <- read.csv("FDC.csv",na.strings = "")

#Primeras filas del archivo
head(inp)







```

```{r}
#Longitud de las serie, filas y columnas 
dim(inp)


#N/A en el archivo (casos completos)

inp[!complete.cases(inp),]

#Se quitan los casos faltantes 
# newinp <- na.omit(inp)

```
### Graficos del crecimiento de los caudales en los rios, como respuesta a las lluvias 

```{r}
#Datos  del rio estrella  
plot(inp[,2], type = "l",
     col= "red",
     xlab = "Fecha",
     ylab = "Crecimiento de los caudales ")


#Datos del río banano
lines(inp[,3], col="black")



#Resumen de los datos para las columnas de 2 a 3
summary(inp[,2:3])


#Histograma del río Estrella
hist(inp[, 2], 
     col = "yellow", 
     xlab = "Rango absoluto",
     ylab = " Caudal ml por día",
     main = "Clases de agua por día, contra el agua del caudal en el río Estrella")


#Histograma del río Banano
hist(inp[,3], 
     col = "Brown", 
     xlab = "Rango absoluto",
     ylab = "Caudal ml por día",
     main = "Clases de agua por día, contra el agua del caudal en el río Banano")

#Se le colocan los noombres 
names(inp)<- c("fecha", "Estrella", "Banano")

#Para desarrollar el nombre
attach(inp)


#Se toma el nombre y se asocia con la columna numero 2
plot(Estrella, col= "pink")



#Formato del tiempo 
Tempdate <- strptime(inp[,1],format = "%d/%m/%Y")


# Volumen de los rios
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum) 
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum) 


#Se exportan los volumenes del caudal de los respectivos ríos
write.csv(rbind(MAQ_Estrella, MAQ_Banano), file="MAQ.csv")


#Valores anuales de los caduales 
plot(MAQ_Banano, ylim = c(100,3000), col= "red", main= "Valores anules de los caudales")
lines(MAQ_Estrella, col=2)


# Se observa el  volumen del caudal acumulado mensualmente 
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum) 
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum) 

```

## Analisis de correlación

```{r}

#Se correlaciona el río Banano y el  río Estrella, para observar si ya mencionadas cuencas tienen una relación climaticamente cuantificable 


corinp <-cor(inp[,2:3], method= "spearman")

#Correlación entre rios
plot(Estrella, Banano, col= "gray", main= "Correlación entre Río Estrella y Río Banano")

```


### Modelo de regresión lineal

```{r}

#Se generara un modelo de regresión lineal 
inp.lm <- lm(inp[,2]~ inp[,3], data = inp) 
summary(inp.lm)

#Se observa el modelo de regresión lineal 
plot(inp.lm, col= "blue", main= "Modelo de regresión lineal")



```
