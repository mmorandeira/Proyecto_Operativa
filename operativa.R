library(pastecs)
library(tibble)
library(ggplot2)

archivos<-c("./Data/data-2009.csv","./Data/data-2012.csv","./Data/data-2013.csv","./Data/data-2014.csv","./Data/data-2015.csv","./Data/data-2016.csv","./Data/data-2017.csv","./Data/data-2018.csv","./Data/data-2019.csv","./Data/data-2020.csv")

###Porcentaje de abandono por año
porcentaje_abandono<-c()
n_inscriptos_año<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  abandono <- dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL")) 
  n_abandono <- dplyr::count(abandono)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- n_abandono/n_inscriptos
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje*100)
  n_inscriptos_año<-c(n_inscriptos_año,n_inscriptos)
  print(n_inscriptos)

 }

traspuesta<-t(porcentaje_abandono)
labels<-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
bp<-barplot(traspuesta,names=labels,ylim=c(0,100),main="Porcentaje de abandonos por año",col=(brewer.pal(3, "Pastel2") ))


###Porcentaje de abandono por año en parcial
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  ausente_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(ausente_parcial$Legajo, ausente_recu$Legajo)  
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- n_abandono/n_inscriptos
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,0.6),main="Porcentaje de abandonos por año en parcial")
x2<-porcentaje_abandono



###Porcentaje de abandono por año en parcial del total de abandonos
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  ausente_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(ausente_parcial$Legajo, ausente_recu$Legajo)  
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_ausentes_prefi <-dplyr::count(ausente_prefi)
  porcentaje<- n_abandono/n_ausentes_prefi
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
 

}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,1),main="Porcentaje de abandonos por año en parcial del total de abandonos")

x<-porcentaje_abandono

###Porcentaje de abandono por año en recuperatorio
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  desaprobado_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Desaprobado")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(desaprobado_parcial$Legajo, ausente_recu$Legajo)
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- n_abandono/n_inscriptos
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,0.1),main="Porcentaje de abandonos por año en recuperatorio")
y2<-porcentaje_abandono

###Porcentaje de abandono por año en recuperatorio del total de abandonos
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  desaprobado_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Desaprobado")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(desaprobado_parcial$Legajo, ausente_recu$Legajo)
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_ausentes_prefi <-dplyr::count(ausente_prefi)
  porcentaje<- n_abandono/n_ausentes_prefi
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
  
  
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,1),main="Porcentaje de abandonos por año en recuperatorio del total de abandonos")
y<-porcentaje_abandono


###Porcentaje de abandono por año en prefinal
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  desaprobado_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & (Resultado=="Desaprobado" | Resultado=="Ausente"))
  desaprobado_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Desaprobado")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(desaprobado_parcial$Legajo, desaprobado_recu$Legajo)
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- n_abandono/n_inscriptos
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
  
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,0.15),main="Porcentaje de abandonos por año en prefinal")
z2<-porcentaje_abandono
print(porcentaje_abandono)
###Porcentaje de abandono por año en prefinal del total de abandonos
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  desaprobado_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & (Resultado=="Desaprobado" | Resultado=="Ausente"))
  desaprobado_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & (Resultado=="Desaprobado" | Resultado=="NULL"))
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common <- intersect(desaprobado_parcial$Legajo, desaprobado_recu$Legajo)
  common <- intersect(ausente_prefi$Legajo,common)
  n_abandono <- length(common)
  n_ausentes_prefi <-dplyr::count(ausente_prefi)
  porcentaje<- n_abandono/n_ausentes_prefi
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
  
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,1),main="Porcentaje de abandonos por año en prefinal del total de abandonos")
z<-porcentaje_abandono

matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
colnames(matriz) <- c("Abandonos por año en parcial del total de abandonos","Abandonos por año en recuperatorio del total de abandonos","Abandonos por año en prefinal del total de abandonos")

# create color palette:
library(RColorBrewer)
coul <- brewer.pal(3, "Pastel2") 

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100/sum(as.numeric(x),na.rm=T)})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul , border="white", main="Abandono por instancia del total de abandonos")


##par(mar = c(0, 0, 0, 0))
##plot.new()
##legend("top", rownames(matriz),fill=coul, cex=0.8,inset=.02)


matriz<-matrix(c(x2,y2,z2),ncol=3)
rownames(matriz) <-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
colnames(matriz) <- c("Abandonos por año en parcial del total de inscriptos","Abandonos por año en recuperatorio del total de inscriptos","Abandonos por año en prefinal del total de inscriptos")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de abandono por instancia")


##par(mar = c(0, 0, 0, 0))
##plot.new()
##legend("top", rownames(matriz),fill=coul, cex=0.8,inset=.02)


###Porcentaje de recursantes de grado 1 
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  print(i)


  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
    common <- setdiff(common,inscriptos_2$Legajo)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    common <- setdiff(common,inscriptos_3$Legajo)

  }  

  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
}
traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,50),main="Porcentaje de recursantes de grado 1",col=brewer.pal(3, "Pastel2"))

print(traspuesta)



###Porcentaje de recursantes de grado 2
porcentaje_recursantes<-c()

for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial") 
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial") 
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes_1$Legajo)
  common <- intersect(common, inscriptos_recursantes_2$Legajo)
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    common <- setdiff(common,inscriptos_3$Legajo)
  }
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  print(recursantes)
}
traspuesta<-t(porcentaje_recursantes)
labels_2_anios_atras<-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,50),main="Porcentaje de recursantes de grado 2",col=brewer.pal(3, "Pastel2"))

print(traspuesta)

###Porcentaje de recursantes de grado 3
porcentaje_recursantes<-c()

for(i in 1:7)
{
  archivo_recursantes_3 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_2 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  inscriptos_recursantes_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes_3$Legajo)
  common <- intersect(common,inscriptos_recursantes_2$Legajo)
  common <- intersect(common,inscriptos_recursantes_1$Legajo)
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)

}
traspuesta<-t(porcentaje_recursantes)

labels_3_anios_atras<-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,20),main="Porcentaje de recursantes de grado 3",col=brewer.pal(3, "Pastel2"))

print(traspuesta)

###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_2$Legajo)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

}

traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.7),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes



###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_2$Legajo)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

}

traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes


###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_2$Legajo)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

}

traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
colnames(matriz) <- c("Recursantes de grado 1 aprobados en parcial","Recursantes de grado 1 aprobados en recuperatorio","Recursantes de grado 1 aprobados en prefinal")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 1 aprobados por instancia")

print(matriz)
#par(mar = c(0, 0, 0, 0))
#plot.new()
#legend("top", colnames(matriz),fill=coul, cex=0.8,inset=.02)

###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }

  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes

###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")  
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes

###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
colnames(matriz) <- c("Recursantes de grado 2 aprobados en parcial","Recursantes de grado 2 aprobados en recuperatorio","Recursantes de grado 2 aprobados en prefinal")
matriz

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 2 aprobados por instancia")

print(matriz)

#par(mar = c(0, 0, 0, 0))
#plot.new()
#legend("top", colnames(matriz),fill=coul, cex=0.8,inset=.02)

###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes_3 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_2 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  inscriptos_recursantes_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(recursantes,inscriptos_recursantes_3$Legajo)
  
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes


###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes_3 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_2 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  inscriptos_recursantes_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(recursantes,inscriptos_recursantes_3$Legajo)
  
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes

###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes_3 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_2 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  inscriptos_recursantes_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial")
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial")
  
  recursantes <- intersect(inscriptos_recursantes_1$Legajo,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(recursantes,inscriptos_recursantes_3$Legajo)
  
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
 
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
colnames(matriz) <- c("Recursantes de grado 3 aprobados en parcial","Recursantes de grado 3 aprobados en recuperatorio","Recursantes de grado 3 aprobados en prefinal")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 3 aprobados por instancia")


print(matriz)


###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en alguna instancia
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,1),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en alguna instancia")


###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en alguna instancia
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
 
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.7),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en alguna instancia")


###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en alguna instancia
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.7),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en alguna instancia")


###Porcentaje de recursantes por abandono con respecto al ultimo año del total de inscriptos
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  abandono <- dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & Resultado=="Ausente")
  abandono_inscriptos<- intersect(recursantes, abandono$Legajo)
  n_inscriptos<-nrow(inscriptos)
  n_recursantes <-length(recursantes)
  n_abandono_inscriptos <- length(abandono_inscriptos)
  porcentaje<- n_abandono_inscriptos/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,100),main="Porcentaje de recursantes por abandono con respecto al ultimo año del total de inscriptos",col=(brewer.pal(3, "Pastel2")))

###Porcentaje de recursantes por abandono con respecto al ultimo año del total de recursantes
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  abandono <- dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & Resultado=="Ausente")
  abandono_inscriptos<- intersect(recursantes, abandono$Legajo)
  n_recursantes <-length(recursantes)
  n_abandono_inscriptos <- length(abandono_inscriptos)
  porcentaje<- n_abandono_inscriptos/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)

  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,100),main="Porcentaje de recursantes por abandono con respecto al ultimo año del total de recursantes",col=(brewer.pal(3, "Pastel2") ))



###Porcentaje de recursantes por abandono con respecto al penultimo año del total de recursantes
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  abandono <- dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & Resultado=="Ausente")
  abandono_inscriptos<- intersect(recursantes, abandono$Legajo)
  n_recursantes <-length(recursantes)
  n_abandono_inscriptos <- length(abandono_inscriptos)
  porcentaje<- n_abandono_inscriptos/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,100),main="Porcentaje de recursantes por abandono con respecto al penultimo año del total de recursantes",col=(brewer.pal(3, "Pastel2") ))

print(porcentaje_recursantes)
mean(porcentaje_recursantes)

###Porcentaje de recursantes por abandono con respecto al antepenultimo año del total de recursantes
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  abandono <- dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & Resultado=="Ausente")
  abandono_inscriptos<- intersect(recursantes, abandono$Legajo)
  n_recursantes <-length(recursantes)
  n_abandono_inscriptos <- length(abandono_inscriptos)
  porcentaje<- n_abandono_inscriptos/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,100),main="Porcentaje de recursantes por abandono con respecto al antepenultimo año del total de recursantes",col=(brewer.pal(3, "Pastel2") ))


###Porcentaje de aprobados por año
porcentaje_aprobados<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial")
  aprobados <-dplyr::filter(archivo,Resultado=="Aprobado")
  n_inscriptos<-dplyr::count(inscriptos)
  n_aprobados <-dplyr::count(aprobados)
  porcentaje<- n_aprobados/n_inscriptos
  porcentaje_aprobados<-c(porcentaje_aprobados,porcentaje*100)
}

print(porcentaje_aprobados)
x<-porcentaje_aprobados

###Porcentaje de desaprobados por año
porcentaje_desaprobados<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial")
  desaprobados <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Desprobado" | Resultado=="Ausente" | Resultado=="NULL"))
  n_inscriptos<-dplyr::count(inscriptos)
  n_desaprobados <-dplyr::count(desaprobados)
  porcentaje<- n_desaprobados/n_inscriptos
  porcentaje_desaprobados<-c(porcentaje_desaprobados,porcentaje*100)
}

y<-porcentaje_desaprobados


matriz<-matrix(c(x,y),ncol=2)
rownames(matriz) <-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
colnames(matriz) <- c("Aprobados por año","Desaprobados por año")

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100/sum(as.numeric(x),na.rm=T)})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de aprobados y desaprobados por año")


##par(mar = c(0, 0, 0, 0))
##plot.new()
##legend("top", colnames(matriz),fill=coul, cex=0.8,inset=.02)




###Porcentaje de recursantes por ausente puro de grado 2
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_2_anios <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_1_anio <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_2_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_1_anio,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  
  common <- intersect(inscriptos$Legajo, common_2_anios)
  common <- intersect(common_1_anio, common)
  
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    common <- setdiff(common,common_3_anio)
  }
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
}


traspuesta<-t(porcentaje_recursantes)

labels_2_anios_atras<-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,20),main="Porcentaje de recursantes por ausente puro de grado 2",col=(brewer.pal(3, "Pastel2") ))

print(traspuesta)

###Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registros
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_3_anios <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_2_anios <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_1_anio <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  
  ausente_parcial_3_anios <- dplyr::filter(archivo_3_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_3_anios <- intersect(ausente_parcial_3_anios$Legajo, ausente_recu_3_anios$Legajo)  
  common_3_anios <- intersect(ausente_prefi_3_anios$Legajo,common_3_anios)
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_2_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_1_anio,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  
  common <- intersect(inscriptos$Legajo, common_3_anios)
  common <- intersect(common_2_anios, common)
  common <- intersect(common_1_anio, common)
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
  
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,10),main="Porcentaje de recursantes por ausente puro de grado 3",col=(brewer.pal(3, "Pastel2") ))

print(traspuesta)

###Porcentaje de recursantes por ausente puro con respecto al ultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  recursantes <- intersect(inscriptos$Legajo, common_1_anio)
  
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_2_anio <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_2_anio <- intersect(ausente_parcial_2_anio$Legajo, ausente_recu_2_anio$Legajo)  
    common_2_anio <- intersect(ausente_prefi_2_anio$Legajo,common_2_anio)
    recursantes <- setdiff(recursantes,common_2_anio)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  
  
  
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.7),main="Porcentaje de recursantes por ausente puro con respecto al ultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro con respecto al ultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")

  
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
   
  recursantes <- intersect(inscriptos$Legajo, common_1_anio )

  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_2_anio <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_2_anio <- intersect(ausente_parcial_2_anio$Legajo, ausente_recu_2_anio$Legajo)  
    common_2_anio <- intersect(ausente_prefi_2_anio$Legajo,common_2_anio)
    recursantes <- setdiff(recursantes,common_2_anio)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}
traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.7),main="Porcentaje de recursantes por ausente puro con respecto al ultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes


###Porcentaje de recursantes por ausente puro con respecto al ultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  recursantes <- intersect(inscriptos$Legajo,common_1_anio)
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_2_anio <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_2_anio <- intersect(ausente_parcial_2_anio$Legajo, ausente_recu_2_anio$Legajo)  
    common_2_anio <- intersect(ausente_prefi_2_anio$Legajo,common_2_anio)
    recursantes <- setdiff(recursantes,common_2_anio)
  }
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al ultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes

matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 1 aprobados en parcial","Recursantes por ausente puro de grado 1 aprobados en recuperatorio","Recursantes por ausente puro de grado 1 aprobados en prefinal")

print(matriz)
# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 1 aprobados por instancia")

grado_1<-(x+y+z)
grado_1
##par(mar = c(0, 0, 0, 0))
##plot.new()
##legend("top", colnames(matriz),fill=coul, cex=0.8,inset=.02)


###Porcentaje de recursantes por ausente puro con respecto al penultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
 
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes_1,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  recursantes <- intersect(common_1_anio,common_2_anios)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al penultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro con respecto al penultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes_1,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  recursantes <- intersect(common_1_anio,common_2_anios)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al penultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro con respecto al penultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes_1,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes_1,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  recursantes <- intersect(common_1_anio,common_2_anios)
  recursantes <- intersect(inscriptos$Legajo, recursantes)
  
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    recursantes <- setdiff(recursantes,common_3_anio)
  }
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al penultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 2 aprobados en parcial","Recursantes por ausente puro de grado 2 aprobados en recuperatorio","Recursantes por ausente puro de grado 2 aprobados en prefinal")

grado_2<-c(x+y+z)

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 2 aprobados por instancia")

print(matriz)
###Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:7)
{
  
  archivo_3_anios <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_2_anios <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_1_anio <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  
  ausente_parcial_3_anios <- dplyr::filter(archivo_3_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_3_anios <- intersect(ausente_parcial_3_anios$Legajo, ausente_recu_3_anios$Legajo)  
  common_3_anios <- intersect(ausente_prefi_3_anios$Legajo,common_3_anios)
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_2_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_1_anio,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  common <- intersect(inscriptos$Legajo, common_3_anios)
  common <- intersect(common_2_anios, common)
  common <- intersect(common_1_anio, common)

  
  recursantes <- intersect(inscriptos$Legajo, common)
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registro aprobados en parcial")

x<-porcentaje_recursantes


###Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:7)
{
  
  archivo_3_anios <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_2_anios <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_1_anio <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  
  ausente_parcial_3_anios <- dplyr::filter(archivo_3_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_3_anios <- intersect(ausente_parcial_3_anios$Legajo, ausente_recu_3_anios$Legajo)  
  common_3_anios <- intersect(ausente_prefi_3_anios$Legajo,common_3_anios)
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_2_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_1_anio,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  common <- intersect(inscriptos$Legajo, common_3_anios)
  common <- intersect(common_2_anios, common)
  common <- intersect(common_1_anio, common)
  
  
  recursantes <- intersect(inscriptos$Legajo, common)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registro aprobados en recuperatorio")

y<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:7)
{
  
  archivo_3_anios <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_2_anios <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_1_anio <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  
  
  ausente_parcial_3_anios <- dplyr::filter(archivo_3_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_3_anios <-dplyr::filter(archivo_3_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_3_anios <- intersect(ausente_parcial_3_anios$Legajo, ausente_recu_3_anios$Legajo)  
  common_3_anios <- intersect(ausente_prefi_3_anios$Legajo,common_3_anios)
  
  ausente_parcial_2_anios <- dplyr::filter(archivo_2_anios,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_2_anios <-dplyr::filter(archivo_2_anios,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_2_anios <- intersect(ausente_parcial_2_anios$Legajo, ausente_recu_2_anios$Legajo)  
  common_2_anios <- intersect(ausente_prefi_2_anios$Legajo,common_2_anios)
  
  ausente_parcial_1_anio <- dplyr::filter(archivo_1_anio,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_1_anio,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  
  common <- intersect(inscriptos$Legajo, common_3_anios)
  common <- intersect(common_2_anios, common)
  common <- intersect(common_1_anio, common)
  
  
  recursantes <- intersect(inscriptos$Legajo, common)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes por ausente puro con respecto al antepenultimo año con registro aprobados en prefinal")

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 3 aprobados en parcial","Recursantes por ausente puro de grado 3 aprobados en recuperatorio","Recursantes por ausente puro de grado 3 aprobados en prefinal")

grado_3<-c(x+y+z)

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 3 aprobados por instancia")

print(matriz)

###Porcentaje de recursantes por ausente puro de grado 1 
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 

  ausente_parcial_1_anio <- dplyr::filter(archivo_recursantes,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi_1_anio <-dplyr::filter(archivo_recursantes,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  common_1_anio <- intersect(ausente_parcial_1_anio$Legajo, ausente_recu_1_anio$Legajo)  
  common_1_anio <- intersect(ausente_prefi_1_anio$Legajo,common_1_anio)
  common <- intersect(inscriptos$Legajo,common_1_anio)
  
  if(i>1){
    archivo_recursantes_2 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    ausente_parcial_2_anio <- dplyr::filter(archivo_recursantes_2,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_2_anio <-dplyr::filter(archivo_recursantes_2,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_2_anio <- intersect(ausente_parcial_2_anio$Legajo, ausente_recu_2_anio$Legajo)  
    common_2_anio <- intersect(ausente_prefi_2_anio$Legajo,common_2_anio)
    common <- setdiff(common,common_2_anio)
  }
  
  if(i>2){
    archivo_recursantes_3 <- read.table(file = archivos[i-2],header = TRUE, sep=",")
    ausente_parcial_3_anio <- dplyr::filter(archivo_recursantes_3,Instancia=="Parcial" & Resultado=="Ausente")
    ausente_recu_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Recuperatorio" & Resultado=="Ausente")
    ausente_prefi_3_anio <-dplyr::filter(archivo_recursantes_3,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
    common_3_anio <- intersect(ausente_parcial_3_anio$Legajo, ausente_recu_3_anio$Legajo)  
    common_3_anio <- intersect(ausente_prefi_3_anio$Legajo,common_3_anio)
    common <- setdiff(common,common_3_anio)
  }
   

  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje*100)
  
}
traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,50),main="Porcentaje de recursantes por ausente puro de grado 1",col=brewer.pal(3, "Pastel2"))

print(traspuesta)


Tipo_de_recursante<- c("Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1",
         "Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2",
         "Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3")

año<- c(2012,2013,2014,2015,2016,2017,2018,2019,2020)



Porcentaje_de_aprobacion <- c(grado_1,0,grado_2,0,0,grado_3)
datos<- data.frame(Tipo_de_recursante,año,Porcentaje_de_aprobacion)
library(ggplot2)

ggplot(datos,aes(x=año,y=Porcentaje_de_aprobacion,group=Tipo_de_recursante,colour=Tipo_de_recursante))+geom_line()+geom_point(size=2,shape=21,fill="white")+theme_minimal()+ ggtitle("Tendencia de aprobación recursantes por ausente puro")+labs(y="Porcentaje de aprobación",x="Año",colour="Tipo de recursante")



