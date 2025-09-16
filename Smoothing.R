#***************************************
#Cargar las librerías requeridas
#***************************************
library(fda)

#***************************************
#Leer y graficar los datos discretizados
#***************************************
setwd("C:/Data_Sets/CanadianWeather")
temp <- matrix(scan("dailtemp.txt",0, dec="."), nrow=365, 35)

#***************************************
#Graficar los datos discretizados
#***************************************
plot(1:365,temp[,1], col=1, main="Datos discretizados de la Temperatura", ylim = c(-40,25))
for (j in 1:ncol(temp))
	points(1:365, temp[,j], col=j)

#********************************************************************************
#Crea las bases de Funciones usando Series de Fourier y B-splines
#********************************************************************************
Bfourier <- create.fourier.basis(rangeval=c(0,365), nbasis=10)
plot(Bfourier)

BSpl     <- create.bspline.basis(norder=1, breaks=seq(0,365,length=4))
plot(BSpl)


#********************************************
#Suaviza los datos usando la función Data2fd
#***********************************************************************************************
#This function tends to be used in rather simple applications where there is no need to control"# the roughness of the resulting curve with any great finesse. The roughness is essentially
# controlled by how many basis functions are used. 
#***********************************************************************************************
x11()
Ftemp    <- Data2fd(temp, basisobj=Bfourier)
plot(Ftemp, main="Datos Suavizados usando la función Data2fd con una base de Fourier")

x11()
Ftemp    <- Data2fd(temp, basisobj=BSpl)
plot(Ftemp, main="Datos Suavizados usando la función Data2fd con una base de BSpl") 


#****************************************************************************************************************
#Suaviza los datos usando la función smooth.basis:
#Discrete observations on one or more curves and for one more more variables are fit with a set of smooth curves, 
#each defined by an expansion in terms of user-selected basis functions. 
#The fitting criterion is weighted least squares, and smoothness can be defined in terms of a roughness penalty
# Smoothing without covariates is often called nonparametric regression, and with covariates is termed semiparametric regression.
#****************************************************************************************************************
x11()
Ftemp1 <-  smooth.basis(1:365, temp, Bfourier)
plot(Ftemp1, main="Datos Suavizados usando la función smooth.basis con una base de Fourier")

x11()
Ftemp1 <-  smooth.basis(1:365, temp, BSpl)
plot(Ftemp1, main="Datos Suavizados usando la funci?n smooth.basis con una base de B-Splines")


#********************************************************************************
#Suaviza los datos usando la función smooth.spline
#Fits a CUBIC smoothing spline
#Involucra Penalización 
#********************************************************************************
x11()
SmoothTemp <- matrix(0, nrow=dim(temp)[1], ncol=dim(temp)[2])
for (j in 1:ncol(temp))
	SmoothTemp[,j] <- smooth.spline(1:365, temp[,j], nknots=7)$y
plot(1:365, SmoothTemp[,1], col=1, main="Datos Suavizados usando la funci?n smooth.spline", ylim=c(min(SmoothTemp),max(SmoothTemp)))
for (j in 1:ncol(SmoothTemp))
	lines(1:365, SmoothTemp[,j], col=j)

#******************************
#realiza el functional boxplot
#******************************
fbplot(SmoothTemp, ylim=c(-50,30))

