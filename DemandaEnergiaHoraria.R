#Cargar las librerías requeridas
#***************************************
library(fda)

#***************************************
#Leer y graficar los datos discretizados
#***************************************
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Funcionales")
library(readxl)

DemandaRealDiariaTolima2023a2024 <- read_excel("DemandaRealDiariaTolima2023a2024.xlsx", 
                                               sheet = "DatosHorDemanda", col_types = c("date", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric", 
                                                                                        "numeric", "numeric", "numeric"))



sub<-DemandaRealDiariaTolima2023a2024[1:nrow(DemandaRealDiariaTolima2023a2024),2:ncol(DemandaRealDiariaTolima2023a2024)]
demanda <- t(as.matrix(sub))

#***************************************
#Graficar los datos discretizados
#***************************************
plot(1:nrow(demanda),demanda[,1], col=1, main="Datos discretizados de la Demanda de Energía Tolima", ylim = c(min(demanda)-1,max(demanda)+1), ylab="Demanda")
for (j in 1:ncol(demanda))
  points(1:nrow(demanda), demanda[,j], col=j)

#********************************************************************************
#Crea las bases de Funciones usando Series de Fourier y B-splines
#********************************************************************************
Bfourier <- create.fourier.basis(rangeval=c(1,24), nbasis=10)
plot(Bfourier)

BSpl     <- create.bspline.basis(norder=3, breaks=seq(1,24,length=4))
plot(BSpl)



#********************************************
#Suaviza los datos usando la función Data2fd
#***********************************************************************************************
#This function tends to be used in rather simple applications where there is no need to control"# the roughness of the resulting curve with any great finesse. The roughness is essentially
# controlled by how many basis functions are used. 
#***********************************************************************************************

Fdemanda   <- Data2fd(demanda, basisobj=Bfourier)
plot(Fdemanda, main="Demanda de Energía Horaria Tolima 2023 a 2024 suavizados con una base de Fourier")

x11()
Fdemanda    <- Data2fd(demanda, basisobj=BSpl)
plot(Fdemanda, main="Datos Suavizados usando la función Data2fd con una base de BSpl") 


#****************************************************************************************************************
#Suaviza los datos usando la función smooth.basis:
#Discrete observations on one or more curves and for one more more variables are fit with a set of smooth curves, 
#each defined by an expansion in terms of user-selected basis functions. 
#The fitting criterion is weighted least squares, and smoothness can be defined in terms of a roughness penalty
# Smoothing without covariates is often called nonparametric regression, and with covariates is termed semiparametric regression.
#****************************************************************************************************************
x11()
Fdemanda1 <-  smooth.basis(1:nrow(demanda), demanda, Bfourier)
plot(Fdemanda1, main="Datos Suavizados usando la función smooth.basis con una base de Fourier")

x11()
Fdemanda1 <-  smooth.basis(1:nrow(demanda), demanda, BSpl)
plot(Fdemanda1, main="Datos Suavizados usando la funci?n smooth.basis con una base de B-Splines")


#********************************************************************************
#Suaviza los datos usando la función smooth.spline
#Fits a CUBIC smoothing spline
#Involucra Penalización 
#********************************************************************************
x11()
SmoothDeman <- matrix(0, nrow=dim(demanda)[1], ncol=dim(demanda)[2])
for (j in 1:ncol(demanda))
  SmoothDeman[,j] <- smooth.spline(1:nrow(demanda), demanda[,j], nknots=7)$y
plot(1:nrow(demanda), SmoothDeman[,1], col=1, main="Datos Suavizados usando la funci?n smooth.spline", ylim=c(min(SmoothDeman),max(SmoothDeman)))
for (j in 1:ncol(SmoothTemp))
  lines(1:nrow(demanda), SmoothTemp[,j], col=j)

#******************************
#realiza el functional boxplot
#******************************
fbplot(SmoothTemp, ylim=c(-50,30))

library(fda)
Bfourier1 <-create.fourier.basis(rangeval=c(0,3), nbasis=10)
plot(Bfourier1)
