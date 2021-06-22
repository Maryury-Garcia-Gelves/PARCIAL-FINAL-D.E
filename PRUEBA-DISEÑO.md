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

P value &gt; alfa: no se rechaza la H0 (Normalidad)

P value &lt; alfa: se rechaza la H0 (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datosm[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic             p value Result
    ## 1 Mardia Skewness  88.3227734012552 0.00379931165630201     NO
    ## 2 Mardia Kurtosis 0.717064623019103   0.473334223170721    YES
    ## 3             MVN              <NA>                <NA>     NO
    ## 
    ## $univariateNormality
    ##           Test        Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk      edad          0.9208  0.0281      NO    
    ## 2 Shapiro-Wilk hrs en  familia    0.8113   1e-04      NO    
    ## 3 Shapiro-Wilk   hrs-deporte      0.7666  <0.001      NO    
    ## 4 Shapiro-Wilk    hrs_redes       0.8725  0.0019      NO    
    ## 5 Shapiro-Wilk   hrs_trabajo      0.6318  <0.001      NO    
    ## 6 Shapiro-Wilk hrs_de estudio     0.8869  0.0041      NO    
    ## 
    ## $Descriptives
    ##                  n          Mean Std.Dev      Median        Min      Max
    ## edad            30 -5.777750e-16       1 -0.46157777 -1.3008101 2.475735
    ## hrs en  familia 30  1.815473e-16       1  0.04357922 -1.2637975 1.350956
    ## hrs-deporte     30 -3.329946e-17       1 -0.25720529 -0.8083595 2.498566
    ## hrs_redes       30 -2.035752e-16       1 -0.41666490 -1.3781993 1.506404
    ## hrs_trabajo     30  4.440169e-17       1 -0.85977653 -0.8597765 1.124323
    ## hrs_de estudio  30  4.163336e-17       1 -0.02101433 -1.9123038 1.239845
    ##                       25th      75th        Skew   Kurtosis
    ## edad            -0.8811939 0.6923667  0.63925349 -0.6031394
    ## hrs en  familia -1.2637975 1.0241117  0.05197543 -1.3464531
    ## hrs-deporte     -0.8083595 0.2939489  1.05643704  0.1942479
    ## hrs_redes       -0.4166649 0.5448695  0.17134363 -1.2169119
    ## hrs_trabajo     -0.8597765 1.1243232  0.25572840 -1.9979035
    ## hrs_de estudio  -0.6514441 1.2398453 -0.05259886 -1.3500914

En el test Mardia Skewness P value &lt; alfa,se rechaza la H0,es decir
no existe normalidad multivariante, en cambio en el test Mardia Kurtosis
P value &gt; alfa, no se rechaza H0, es decir existe normalidad
multivariable. por tanto, MVN nos da un resultado de NO.
