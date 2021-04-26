library(pastecs)
library(tibble)
library(ggplot2)
library(RColorBrewer)

# create color palette:
coul <- brewer.pal(3, "Pastel2") 

archivos<-c("./Data/data-2009.csv","./Data/data-2012.csv","./Data/data-2013.csv","./Data/data-2014.csv","./Data/data-2015.csv","./Data/data-2016.csv","./Data/data-2017.csv","./Data/data-2018.csv","./Data/data-2019.csv","./Data/data-2020.csv")


labels_año_anterior<-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
labels_2_anios_atras<-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
labels_3_anios_atras<-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")


porcentaje_abandono<-c()
porcentaje_abandono_en_parcial<-c()
porcentaje_abandono_en_recuperatorio<-c()
porcentaje_abandono_en_prefinal<-c()

for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial") 
  abandono <- dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))  
  ausente_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & (Resultado=="Ausente" | Resultado=="NULL"))
  desaprobado_parcial <- dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Desaprobado")
  desaprobado_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Desaprobado")
  ausente_3_inst <- intersect(ausente_parcial$Legajo, ausente_recu$Legajo)  
  ausente_3_inst <- intersect(ausente_prefi$Legajo,ausente_3_inst)
  desp_parcial_ausente_2_inst <- intersect(desaprobado_parcial$Legajo, ausente_recu$Legajo)
  desp_parcial_ausente_2_inst <- intersect(ausente_prefi$Legajo,desp_parcial_ausente_2_inst)
  desp_aus_parcial <- union(desaprobado_parcial$Legajo,ausente_parcial$Legajo)
  desp_parcial_recu_ausente_1_inst <- intersect(desp_aus_parcial, desaprobado_recu$Legajo)
  desp_parcial_recu_ausente_1_inst <- intersect(ausente_prefi$Legajo,desp_parcial_recu_ausente_1_inst)
  n_ausente_3_inst <- length(ausente_3_inst)
  n_desp_parcial_ausente_2_inst <- length(desp_parcial_ausente_2_inst)
  n_desp_parcial_recu_ausente_1_inst <- length(desp_parcial_recu_ausente_1_inst)  
  n_abandono <- dplyr::count(abandono)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje_abandono_en_parcial<-c(porcentaje_abandono_en_parcial,n_ausente_3_inst/n_inscriptos)
  porcentaje_abandono_en_recuperatorio<-c(porcentaje_abandono_en_recuperatorio,n_desp_parcial_ausente_2_inst/n_inscriptos)
  porcentaje_abandono_en_prefinal<-c(porcentaje_abandono_en_prefinal,n_desp_parcial_recu_ausente_1_inst/n_inscriptos)
}


matriz<-matrix(c(porcentaje_abandono_en_parcial,porcentaje_abandono_en_recuperatorio,porcentaje_abandono_en_prefinal),ncol=3)
rownames(matriz) <-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
colnames(matriz) <- c("Abandonos por año en parcial del total de inscriptos","Abandonos por año en recuperatorio del total de inscriptos","Abandonos por año en prefinal del total de inscriptos")

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de abandono por instancia")


porcentaje_recursantes_g_1<-c()
porcentaje_recursantes_g_1_parcial<-c()
porcentaje_recursantes_g_1_recuperatorio<-c()
porcentaje_recursantes_g_1_prefinal<-c()
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
  aprobados_parcial <- dplyr::filter(archivo_actual,Instancia=="Parcial" & Resultado=="Aprobado")
  aprobados_recuperatorio <- dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_prefinal <- dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  recursantes_aprobados_parcial <- intersect(aprobados_parcial$Legajo,recursantes)
  recursantes_aprobados_recuperatorio <- intersect(aprobados_recuperatorio$Legajo,recursantes)
  recursantes_aprobados_prefinal <- intersect(aprobados_prefinal$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_aprobados_parcial <- length(recursantes_aprobados_parcial)
  n_recursantes_aprobados_recuperatorio <- length(recursantes_aprobados_recuperatorio)
  n_recursantes_aprobados_prefinal <- length(recursantes_aprobados_prefinal)
  n_recursantes <- length(recursantes)
  n_inscriptos <- dplyr::count(inscriptos) 
  porcentaje_recursantes_g_1 <- c(porcentaje_recursantes_g_1, n_recursantes/n_inscriptos*100)
  porcentaje_recursantes_g_1_parcial <- c(porcentaje_recursantes_g_1_parcial,n_recursantes_aprobados_parcial/n_recursantes)
  porcentaje_recursantes_g_1_recuperatorio <- c(porcentaje_recursantes_g_1_recuperatorio,n_recursantes_aprobados_recuperatorio/n_recursantes)
  porcentaje_recursantes_g_1_prefinal <- c(porcentaje_recursantes_g_1_prefinal,n_recursantes_aprobados_prefinal/n_recursantes)
  
}
porcentaje_recursantes_g_1<-t(porcentaje_recursantes_g_1)
barplot(porcentaje_recursantes_g_1,names=labels_año_anterior,ylim=c(0,50),main="Porcentaje de recursantes de grado 1",col=brewer.pal(3, "Pastel2"))


matriz<-matrix(c(porcentaje_recursantes_g_1_parcial,porcentaje_recursantes_g_1_recuperatorio,porcentaje_recursantes_g_1_prefinal),ncol=3)
rownames(matriz) <-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
colnames(matriz) <- c("Recursantes de grado 1 aprobados en parcial","Recursantes de grado 1 aprobados en recuperatorio","Recursantes de grado 1 aprobados en prefinal")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 1 aprobados por instancia")


###Porcentaje de recursantes de grado 2
porcentaje_recursantes_g_2<-c()
porcentaje_recursantes_g_2_parcial<-c()
porcentaje_recursantes_g_2_recuperatorio<-c()
porcentaje_recursantes_g_2_prefinal<-c()

for(i in 1:8)
{
  archivo_recursantes_2 <- read.table(file = archivos[i],header = TRUE, sep=",")
  archivo_recursantes_1 <- read.table(file = archivos[i+1],header = TRUE, sep=",")
  archivo_actual <- read.table(file = archivos[i+2],header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo_actual,Instancia=="Parcial") 
  inscriptos_recursantes_1 <-dplyr::filter(archivo_recursantes_1,Instancia=="Parcial") 
  inscriptos_recursantes_2 <-dplyr::filter(archivo_recursantes_2,Instancia=="Parcial") 
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes_1$Legajo)
  recursantes <- intersect(recursantes, inscriptos_recursantes_2$Legajo)
  if(i>1){
    archivo_recursantes_3 <- read.table(file = archivos[i-1],header = TRUE, sep=",")
    inscriptos_3 <-dplyr::filter(archivo_recursantes_3,Instancia=="Parcial")
    recursantes <- setdiff(recursantes,inscriptos_3$Legajo)
  }
  aprobados_parcial <- dplyr::filter(archivo_actual,Instancia=="Parcial" & Resultado=="Aprobado")
  aprobados_recuperatorio <- dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_prefinal <- dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  recursantes_aprobados_parcial <- intersect(aprobados_parcial$Legajo,recursantes)
  recursantes_aprobados_recuperatorio <- intersect(aprobados_recuperatorio$Legajo,recursantes)
  recursantes_aprobados_prefinal <- intersect(aprobados_prefinal$Legajo,recursantes)
  n_recursantes<-length(recursantes)
  n_recursantes_aprobados_parcial <- length(recursantes_aprobados_parcial)
  n_recursantes_aprobados_recuperatorio <- length(recursantes_aprobados_recuperatorio)
  n_recursantes_aprobados_prefinal <- length(recursantes_aprobados_prefinal)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje_recursantes_g_2<-c(porcentaje_recursantes_g_2,n_recursantes/n_inscriptos*100)
  porcentaje_recursantes_g_2_parcial<-c(porcentaje_recursantes_g_2_parcial,n_recursantes_aprobados_parcial/n_recursantes)
  porcentaje_recursantes_g_2_recuperatorio<-c(porcentaje_recursantes_g_2_recuperatorio,n_recursantes_aprobados_recuperatorio/n_recursantes)
  porcentaje_recursantes_g_2_prefinal<-c(porcentaje_recursantes_g_2_prefinal,n_recursantes_aprobados_prefinal/n_recursantes)
}

porcentaje_recursantes_g_2<-t(porcentaje_recursantes_g_2)
barplot(porcentaje_recursantes_g_2,names=labels_2_anios_atras,ylim=c(0,50),main="Porcentaje de recursantes de grado 2",col=brewer.pal(3, "Pastel2"))

matriz<-matrix(c(porcentaje_recursantes_g_2_parcial,porcentaje_recursantes_g_2_recuperatorio,porcentaje_recursantes_g_2_prefinal),ncol=3)
rownames(matriz) <-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
colnames(matriz) <- c("Recursantes de grado 2 aprobados en parcial","Recursantes de grado 2 aprobados en recuperatorio","Recursantes de grado 2 aprobados en prefinal")

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 2 aprobados por instancia")


###Porcentaje de recursantes de grado 3
porcentaje_recursantes_g_3 <- c()
porcentaje_recursantes_g_3_parcial <- c()
porcentaje_recursantes_g_3_recuperatorio <- c()
porcentaje_recursantes_g_3_prefinal <- c()
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
  recursantes <- intersect(inscriptos$Legajo, inscriptos_recursantes_3$Legajo)
  recursantes <- intersect(recursantes,inscriptos_recursantes_2$Legajo)
  recursantes <- intersect(recursantes,inscriptos_recursantes_1$Legajo)
  
  aprobados_parcial <- dplyr::filter(archivo_actual,Instancia=="Parcial" & Resultado=="Aprobado")
  aprobados_recuperatorio <- dplyr::filter(archivo_actual,Instancia=="Recuperatorio" & Resultado=="Aprobado")
  aprobados_prefinal <- dplyr::filter(archivo_actual,Instancia=="Prefinal" & Resultado=="Aprobado")
  recursantes_aprobados_parcial <- intersect(aprobados_parcial$Legajo,recursantes)
  recursantes_aprobados_recuperatorio <- intersect(aprobados_recuperatorio$Legajo,recursantes)
  recursantes_aprobados_prefinal <- intersect(aprobados_prefinal$Legajo,recursantes)
  n_recursantes_aprobados_parcial <- length(recursantes_aprobados_parcial)
  n_recursantes_aprobados_recuperatorio <- length(recursantes_aprobados_recuperatorio)
  n_recursantes_aprobados_prefinal <- length(recursantes_aprobados_prefinal)
  
  
  n_recursantes<-length(recursantes)
  n_inscriptos <-dplyr::count(inscriptos)
  porcentaje_recursantes_g_3<-c(porcentaje_recursantes_g_3,n_recursantes/n_inscriptos*100)
  porcentaje_recursantes_g_3_parcial <- c(porcentaje_recursantes_g_3_parcial,n_recursantes_aprobados_parcial/n_recursantes)
  porcentaje_recursantes_g_3_recuperatorio <- c(porcentaje_recursantes_g_3_recuperatorio,n_recursantes_aprobados_recuperatorio/n_recursantes)
  porcentaje_recursantes_g_3_prefinal <- c(porcentaje_recursantes_g_3_prefinal,n_recursantes_aprobados_prefinal/n_recursantes)
  
}
traspuesta<-t(porcentaje_recursantes_g_3)

barplot(traspuesta,names=labels_3_anios_atras,ylim=c(0,20),main="Porcentaje de recursantes de grado 3",col=brewer.pal(3, "Pastel2"))

matriz<-matrix(c(porcentaje_recursantes_g_3_parcial,porcentaje_recursantes_g_3_recuperatorio,porcentaje_recursantes_g_3_prefinal),ncol=3)
rownames(matriz) <-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
colnames(matriz) <- c("Recursantes de grado 3 aprobados en parcial","Recursantes de grado 3 aprobados en recuperatorio","Recursantes de grado 3 aprobados en prefinal")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes de grado 3 aprobados por instancia")


###Porcentaje de aprobados, desaprobados y abadonos por ausente puro por año
porcentaje_aprobados<-c()
porcentaje_desaprobados<-c()
porcentaje_ausentes<-c()
for (val in archivos)
{
  archivo <- read.table(file = val,header = TRUE, sep=",")
  inscriptos <-dplyr::filter(archivo,Instancia=="Parcial")
  aprobados <-dplyr::filter(archivo,Resultado=="Aprobado")
  ausente_parcial <-dplyr::filter(archivo,Instancia=="Parcial" & Resultado=="Ausente")
  ausente_recu <-dplyr::filter(archivo,Instancia=="Recuperatorio" & Resultado=="Ausente")
  ausente_prefi <-dplyr::filter(archivo,Instancia=="Prefinal" & Resultado=="Ausente")
  ausentes<-intersect(ausente_parcial$Legajo,ausente_recu$Legajo)
  ausentes<-intersect(ausente_prefi$Legajo,ausentes)
  desaprobados<-setdiff(inscriptos$Legajo,aprobados$Legajo)
  desaprobados<-setdiff(desaprobados,ausentes)
  n_inscriptos<-dplyr::count(inscriptos)
  n_aprobados <-dplyr::count(aprobados)
  n_ausentes <-length(ausentes)
  n_desaprobados<-length(desaprobados)
  porcentaje_apr<- n_aprobados/n_inscriptos
  porcentaje_au<- n_ausentes/n_inscriptos
  porcentaje_des<-n_desaprobados/n_inscriptos
  porcentaje_aprobados<-c(porcentaje_aprobados,porcentaje_apr*100)
  porcentaje_desaprobados<-c(porcentaje_desaprobados,porcentaje_des*100)
  porcentaje_ausentes<-c(porcentaje_ausentes,porcentaje_au*100)
}

length(porcentaje_aprobados)
length(porcentaje_desaprobados)
length(porcentaje_ausentes)


matriz<-matrix(c(porcentaje_aprobados,porcentaje_desaprobados,porcentaje_ausentes),ncol=3)
rownames(matriz) <-c("2009","2012","2013","2014","2015","2016","2017","2018","2019","2020")
colnames(matriz) <- c("Aprobados por año","Desaprobados por año","Abandonos por ausente puro por año")


# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100/sum(as.numeric(x),na.rm=T)})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de aprobados, desaprobados y abandonos por ausente puro por año")


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

barplot(traspuesta,names=labels_2_anios_atras,ylim=c(0,20),main="Porcentaje de recursantes por ausente puro de grado 2",col=(brewer.pal(3, "Pastel2") ))



###Porcentaje de recursantes por ausente puro de grado 3
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



###Porcentaje de recursantes de grado 1 aprobados en parcial
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

x<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro de grado 1 aprobados en recuperatorio
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

y<-porcentaje_recursantes


###Porcentaje de recursantes por ausente puro de grado 1 aprobados en prefinal
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


z<-porcentaje_recursantes

matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 1 aprobados en parcial","Recursantes por ausente puro de grado 1 aprobados en recuperatorio","Recursantes por ausente puro de grado 1 aprobados en prefinal")

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 1 aprobados por instancia")

grado_1<-(x+y+z)

##par(mar = c(0, 0, 0, 0))
##plot.new()
##legend("top", colnames(matriz),fill=coul, cex=0.8,inset=.02)


###Porcentaje de recursantes por ausente puro de grado 2 aprobados en parcial
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

x<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro de grado 2 aprobados en recuperatorio
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

y<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro de grado 2 aprobados en prefinal
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

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2013","2012-2014","2013-2015","2014-2016","2015-2017","2016-2018","2017-2019","2018-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 2 aprobados en parcial","Recursantes por ausente puro de grado 2 aprobados en recuperatorio","Recursantes por ausente puro de grado 2 aprobados en prefinal")

grado_2<-c(x+y+z)

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 2 aprobados por instancia")


###Porcentaje de recursantes por ausente puro de grado 3 aprobados en parcial
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

x<-porcentaje_recursantes


###Porcentaje de recursantes por ausente puro de grado 3 aprobados en recuperatorio
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

y<-porcentaje_recursantes

###Porcentaje de recursantes por ausente puro de grado 3  aprobados en prefinal
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

z<-porcentaje_recursantes


matriz<-matrix(c(x,y,z),ncol=3)
rownames(matriz) <-c("2009-2014","2012-2015","2013-2016","2014-2017","2015-2018","2016-2019","2017-2020")
colnames(matriz) <- c("Recursantes por ausente puro de grado 3 aprobados en parcial","Recursantes por ausente puro de grado 3 aprobados en recuperatorio","Recursantes por ausente puro de grado 3 aprobados en prefinal")

grado_3<-c(x+y+z)

# Transform this data in %
data_percentage <- apply(t(matriz), 2, function(x){as.numeric(x)*100})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul ,ylim=c(0,100), border="white", main="Porcentaje de recursantes por ausente puro de grado 3 aprobados por instancia")


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
barplot(traspuesta,names=labels_año_anterior,ylim=c(0,50),main="Porcentaje de recursantes por ausente puro de grado 1",col=brewer.pal(3, "Pastel2"))


Tipo_de_recursante<- c("Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1","Ausente puro grado 1",
                       "Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2","Ausente puro grado 2",
                       "Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3","Ausente puro grado 3")

año<- c(2012,2013,2014,2015,2016,2017,2018,2019,2020)



Porcentaje_de_aprobacion <- c(grado_1,0,grado_2,0,0,grado_3)
datos<- data.frame(Tipo_de_recursante,año,Porcentaje_de_aprobacion)

ggplot(datos,aes(x=año,y=Porcentaje_de_aprobacion,group=Tipo_de_recursante,colour=Tipo_de_recursante))+geom_line()+geom_point(size=2,shape=21,fill="white")+theme_minimal()+ ggtitle("Tendencia de aprobación recursantes por ausente puro")+labs(y="Porcentaje de aprobación",x="Año",colour="Tipo de recursante")

