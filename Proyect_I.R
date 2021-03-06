## Librerias
library(ggplot2)
library(dplyr)
## Carga de datos
activity<-read.csv("activity.csv")
Sys.setlocale("LC_TIME","English")

### Informacion de variables
str(activity)

### 1 ### Numero de pasos por dia

##crear e imprimir el n�mero de pasos por d�a
StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay)<-c("Date","Steps")
StepsPerDay

### 2 ### Histograma del n�mero total de pasos dados cada d�a

png(filename = "plot1.png",width=480,height=480,units="px",bg="white")
# Histograma
g<-ggplot(StepsPerDay,aes(Steps))
g+geom_histogram(boundary=0,binwidth = 2500,col="darkgreen",fill="lightgreen")+ggtitle("Histogram of steps perday")+
  xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face = "bold",size = 12))+scale_x_continuous(breaks = seq(0,25000,250,0))+
  scale_y_continuous(breaks = seq(0,18,2))
dev.off()

### 3 ### Media y mediana del numero total de pasos por dia 

## mean
mean(StepsPerDay$Steps, na.rm = TRUE)

## median
median(StepsPerDay$Steps,na.rm = TRUE)

### Patr�n de actividad diaria promedio

## 1 ##Gr�fico de serie de tiempo del intervalo de 5 minutos (x) y 
## el n�mero promedio de pasos tomados promediados en todos los d�as (y)

# crear tabla con pasos por tiempo
StepsPerTime<-aggregate(steps~interval,data = activity, FUN=mean,na.action = na.omit)

# tiempo variable (m�s comprensible para el eje del gr�fico)
StepsPerTime$time<-StepsPerTime$interval/100

png(filename = "plot2.png",width=480,height=480,units="px",bg="white")
# dibuja la gr�fica lineal
h<-ggplot(StepsPerTime, aes(time,steps))
h+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+
  theme(plot.title = element_text(face = "bold",size = 12))

dev.off()

### 2 ### Intervalo de 5 minutos (en promedio en todos los d�as) con el n�mero m�ximo de pasos

# tabla para dplyr
ST<- tbl_df(StepsPerTime)

ST%>%select(time,steps)%>%filter(steps==max(ST$steps))

### Imputacion de Valores perdidos

### 1 ### Numero total de valores faltantes en el conjunto de datos

## tabla
ACT<-tbl_df(activity)

ACT%>%filter(is.na(steps))%>%summarize(missing_values=n())

### 2 ### Remplazar los valores perdidos

# valores sin NA se imputan en una nueva columna
activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)

### 3 ### Nuevo conjunto de datos que es igual al conjunto de datos original pero con los datos faltantes completados

# nueva actividad del conjunto de datos
activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)

# ver los primeros 10 valores del nuevo conjunto de datos

head(activityFull,n=10)

### 4A ### Histograma del numero total de pasos cada dia con datos faltantes completados

## preparar data 
StepsPerDayFull<-aggregate(activityFull$steps,list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull)<-c("Date","Steps")

### Histograma
png(filename = "plot3.png",width=480,height=480,units="px",bg="white")

g<-ggplot(StepsPerDayFull,aes(Steps))
g+geom_histogram(boundary=0, binwidth = 2500, col="blue", fill="lightblue")+ggtitle("Histograma de pasos por d�a")+
  xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face = "bold",size = 12))+
  scale_x_continuous(breaks = seq(0,25000,2500))+scale_y_continuous(breaks = seq(0,26,2))

dev.off()

### 4B ###Calcule e informe la media y la mediana del n�mero total de pasos dados por d�a. �Estos valores difieren de las estimaciones de la primera parte de la tarea? 
### �Cu�l es el impacto de imputar los datos faltantes a las estimaciones del n�mero total diario de pasos?

# Mean
mean(StepsPerDayFull$Steps)

# median
median(StepsPerDayFull$Steps)

##�Existen diferencias en los patrones de actividad entre los d�as de semana y los fines de 
##semana?

### 1 ### Cree una nueva variable de factor en el conjunto de datos con dos niveles: 
### "d�a de la semana" y "fin de semana", que indican si una fecha determinada es un d�a de la 
### semana o un fin de semana.

# Crear variable con fecha en formato correcto
activityFull$RealDate<-as.Date(activityFull$date,format="%Y-%m-%d")

# crea una variable con el nombre de los d�as de la semana
activityFull$weekday<-weekdays(activityFull$RealDate)

# crea una nueva variable que indique el d�a de la semana o el fin de semana
activityFull$DayType<-ifelse(activityFull$weekday=='Saturday'|activityFull$weekday=='Sunday','weekend','weekday')

# vemos los 10 primeros registros
head(activityFull,n=10)

### 2 ###Gr�fico de dos series de tiempo del intervalo de 5 minutos (x) y el n�mero promedio de 
### pasos tomados promediados entre los d�as de la semana o los d�as de fin de semana (y).

# crear una tabla con pasos por hora durante los d�as de la semana o los fines de semana
StepsPerTimeDT<-aggregate(steps~interval+DayType,data = activityFull, FUN=mean, na.action = na.omit)

# tiempo variable (m�s comprensible para el eje del gr�fico)
StepsPerTimeDT$time<-StepsPerTime$interval/100

# dibuja la gr�fica lineal
png(filename = "plot4.png",width=480,height=480,units="px",bg="white")
j<-ggplot(StepsPerTimeDT,aes(time,steps))
j+geom_line(col="red")+ggtitle("Pasos promedio por intervalo de tiempo: d�as laborables frente a fines de semana")+
  xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face = "bold",size = 12))+
  facet_grid(DayType~.)

dev.off()
