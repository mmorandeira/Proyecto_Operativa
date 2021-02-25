library(pastecs)
library(tibble)
library(ggplot2)

archivos<-c("./Data/data-2009.csv","./Data/data-2012.csv","./Data/data-2013.csv","./Data/data-2014.csv","./Data/data-2015.csv","./Data/data-2016.csv","./Data/data-2017.csv","./Data/data-2018.csv","./Data/data-2019.csv","./Data/data-2020.csv")

###Porcentaje de abandono por año
porcentaje_abandono<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  abandono <- dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL")) 
  n_abandono <- dplyr::count(abandono)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- n_abandono/n_inscriptos
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)

 }

traspuesta<-t(porcentaje_abandono)
labels<-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
barplot(traspuesta,names=labels,ylim=c(0,1),main="Porcentaje de abandonos por año")



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

###Porcentaje de abandono por año en prefinal del total de abandonos
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
  n_ausentes_prefi <-dplyr::count(ausente_prefi)
  porcentaje<- n_abandono/n_ausentes_prefi
  porcentaje_abandono<-c(porcentaje_abandono,porcentaje)
  print(porcentaje)
  
}
traspuesta<-t(porcentaje_abandono)
barplot(traspuesta,names=labels,ylim=c(0,1),main="Porcentaje de abandonos por año en prefinal del total de abandonos")



###Porcentaje de recursantes con respecto al ultimo año con registros
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}
traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al ultimo año con registro")



###Porcentaje de recursantes con respecto al penultimo año con registros
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}
traspuesta<-t(porcentaje_recursantes)
labels_2_anios_atras<-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.4),main="Porcentaje de recursantes con respecto al penúltimo año con registro")



###Porcentaje de recursantes con respecto al antepenultimo año con registros
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  common <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  recursantes<-length(common)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje<- recursantes/n_inscriptos
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}
traspuesta<-t(porcentaje_recursantes)
labels_3_anios_atras<-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.2),main="Porcentaje de recursantes con respecto al antepenúltimo año con registro")



###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

}

traspuesta<-t(porcentaje_recursantes)
labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en parcial")



###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
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



###Porcentaje de recursantes con respecto al ultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:9)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
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



###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en parcial")



###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en recuperatorio")

###Porcentaje de recursantes con respecto al penultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:8)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al penultimo año con registro aprobados en prefinal")



###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en parcial
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(inscriptos,Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en parcial")




###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en recuperatorio
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en recuperatorio")


###Porcentaje de recursantes con respecto al antepenultimo año con registros aprobados en prefinal
porcentaje_recursantes<-c()
for(i in 1:7)
{
  archivo_recursantes <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+3],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial")
  inscriptos_recursantes <-dplyr::filter(archivo_recursantes,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes$Legajo)
  aprobados_parcial <-dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  aprobados_parcial_recursantes<- intersect(aprobados_parcial$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_parcial <-length(aprobados_parcial_recursantes)
  porcentaje<- n_recursantes_parcial/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
 
}

traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,0.6),main="Porcentaje de recursantes con respecto al antepenultimo año con registro aprobados en prefinal")


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
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.7),main="Porcentaje de recursantes con respecto al ultimo año con registro aprobados en alguna instancia")

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
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)
  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,0.5),main="Porcentaje de recursantes por abandono con respecto al ultimo año del total de inscriptos")

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
  n_recursantes <-length(recursantes)
  n_abandono_inscriptos <- length(abandono_inscriptos)
  porcentaje<- n_abandono_inscriptos/n_recursantes
  porcentaje_recursantes<-c(porcentaje_recursantes,porcentaje)

  
}
traspuesta<-t(porcentaje_recursantes)
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,1),main="Porcentaje de recursantes por abandono con respecto al ultimo año del total de recursantes")

