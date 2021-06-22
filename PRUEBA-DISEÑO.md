ANÁLISIS FACTORIAL
================
Maryury Johana Garcia Gelves 1950186

# **EXAMEN FINAL DE DISEÑO DE EXPERIMENTOS**

La siguiente base de datos fue una encuesta realizada a **30 jovenes de
la Universidad Francisco de Paula Santarder** en la cual se les pregunta
la edad y a que se dedican durante las 24 horas del día.

\#**Importar base de datos en formato Excel**

``` r
library(readxl)
datos<- read_excel("C:/Users/USUARIO/Documents/B.D.xlsx")
```

\#**Tipificación o estandarización de variables**

La tipificación permite que todas las variables métricas gocen de una
misma unidad de medida estadistica.

``` r
datosm<- datos # nueva base de datos o data frame
datosm<-scale(datosm,center = T, scale = T)
datosm<- as.data.frame(datosm)
```

\#**Normalidad Multivariante**

H0: Normalida Multivariante

H1: No Normalidad Multivariante

confianza= 95%

Alfa= 5% = 0,05%

P value &gt; alfa: no se rechaza la H0 (Normalidad) P value &lt; alfa:
se rechaza la H0 (No normalidad)
