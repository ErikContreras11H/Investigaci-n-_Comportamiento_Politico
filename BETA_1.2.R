######################################################
#### Erik Contreras Hernandez
#### *Script*
###           Modelos econometricos de las Hipotises (H1,H2,H3,H4,H5)
#####
#####Covid y comportamiento lectoral, medidas de contencion y sus factores que las dtermina
###
######                           FLACSO- MEXICO
########
#######  Mexico 14 de junio de 2023
######
###### Director: Rodrigo Zalazar
#################
#########Paqueteria requierida para H1
install.packages("read")
install.packages("effects")
library(effects)
install.packages("glmulti")
install.packages("effects")
library(glmulti)
library(effects)
library(readr)
install.packages("ggplot2")
library(ggplot2)
library(car)
install.packages("dplyr")
library(dplyr)
library(factoextra)
install.packages("factoextra")
library(cluster)
library(simstudy)
library(data.table)
install.packages("tidyverse")
library(tidyverse)
library(PerformanceAnalytics)
library(corrr)
library(sandwich)
library(lmtest)
library(margins)
#######################Descragar la base de datos
Inves <- read_csv("Bases_datos_doctorado_investigacion/Inves_cov.csv")
covid <- as.data.frame(Inves)
summary(covid)
View(covid)

fixed <- lm(Diferencia_voto ~ StringencyIndex * Crecimiento+ 
               factor(Tipo) + factor(wave) + Exces_mor + bed_1000 + 
               Inflacion_2020 +EconomicSupportIndex, data = covid)
summary(fixed)

###### Garifico de intervalos de confianza
interplot(m = fixed, var1 ="StringencyIndex", var2 = "Crecimiento", hist = F) +
  geom_hline(yintercept = 0, linetype = "dashed", color= "black")+ 
  xlab ("Desempeño económico")+ ylab("EM Politicas de Contencción")+
  theme_classic()

library(interplot)

##Modelo para presentar la primera exuacion de interaccion
fixed <- lm(Diferencia_voto ~ StringencyIndex * Crecimiento, data = covid)
summary(fixed)

###### Ajustar por errores robusto (varianza)

robust_se <- vcovHC(fixed, type = "HC")

# Realiza pruebas de hipótesis sobre los coeficientes usando los errores estándar robustos
coeftest(fixed, robust_se)


#########Procesar la grafica para ese modelo

# Calcular los efectos marginales con errores estándar robustos
efectos_marginales <- margins(modelo, vcov = robust_se)

# Graficar los efectos marginales con errores estándar robustos
plot(efectos_marginales)
# Calcular los efectos marginales con errores estándar robustos para la interacción
efectos_marginales_interaccion <- margins(modelo, 
                                          variables = c("StringencyIndex", "Crecimiento"), 
vcov = robust_se)

View(efectos_marginales_interaccion)

plot(efectos_marginales_interaccion)

###### De esta forma obtienes los valores estimados

# Convirtiendo las variables a factores antes de utilizarlas en el modelo
covid$Tipo <- as.factor(covid$Tipo)
covid$wave <- as.factor(covid$wave)

# Luego puedes ajustar tu modelo
fixed <- lm(Diferencia_voto ~ StringencyIndex * Crecimiento + Tipo + wave + 
              Exces_mor + bed_1000 + Inflacion_2020 + EconomicSupportIndex, 
            data = covid)

# Y finalmente, calcular el efecto marginal
efecto_marginal <- effect("StringencyIndex:Crecimiento", fixed)


# Calcular el efecto marginal de la interacción entre StringencyIndex y crecimiento_pib_prtge2020
efecto_marginal <- effect("StringencyIndex:Crecimiento", fixed)


efecto_marginal

head(efecto_marginal)

# Graficar el efecto marginal
plot(efecto_marginal, main="Efectos Marginales (EM)", lwd = 1.2,##para robuztecer la linea
     xlab = "Politicas de contención", ylab= "Diferencia de voto")


?plot
#### Datos estadisticos sobre la base del conjunto de obserbaciones 

### El crecimiento economico tiene una media de:

promedio de -4.972 crecimiento negativo

### Mientras que la media de medidas de contencion:
promedio de 45.55 medidas de contencion

#StringencyIndex*Crecimiento effect *Efectos marginales*
#  
###                                    Crecimiento
#StringencyIndex       -20         -5         10         30         40
#6                 1.155023   1.845303   2.535584   3.455957   3.916144
#30               -1.249238  -4.474557  -7.699876 -12.000301 -14.150514
#50               -3.252788  -9.741107 -16.229425 -24.880517 -29.206062
#70               -5.256339 -15.007657 -24.758975 -37.760732 -44.261611
#90               -7.259890 -20.274207 -33.288524 -50.640947 -59.317159


#######################################################
###############   Segundo proceso para las hipotesis H2 H3 H4 H5
##############   Base de datados panel con efectos fijos pais y año
#############3

#Paqueteria necesaia para los modelos econometricos

install.packages("plm")
library(readr)
library(plm)## efectos fijos
install.packages("AER")
library(AER)### variable instrumental
library(dplyr)
library(lubridate)

#### Decargar la base de datos panes (2019- 2022) Otra base

library(read)
Investigacion_2 <- read_csv("Bases_datos_doctorado_investigacion/Investigacion_2.csv")

####        Base de datos panel 2019 - 2022 
 library(readr)
 Investigacion_2 <- read_csv("Bases_datos_doctorado_investigacion/Investigacion_2.csv")
 
 
 medidas <- subset(Investigacion_2, year !=2019)#omitir el año 2019 nohay covid-19

 View(medidas)
 
 names(medidas)
 
 summary(medidas)
 # Muestra las primeras filas de los datos para verificar que se ven correctos
 head(medidas)
 
 # Verifica si hay valores NA en tus datos
 any(is.na(medidas))
 
 # Si hay valores NA, puedes eliminarlos con la siguiente línea de código
 medidas <- na.omit(medidas)
 
 pdata <- pdata.frame(medidas, index = c("CountryName","year"))
 
 medidas$densidad_pob <- log(medidas$densidad_pob + 1)
 medidas$infections <- log(medidas$infections + 1)
 medidas$Decesos <- log(medidas$Decesos + 1)
 
 
 
###################  Modelo que incorpora efectos fijos por pais y año
 Hipotesis
   H2
 
   medidas <- medidas %>%
     mutate(Robustez_I=Robustez_ins * 100)
 
 fixed1 <- plm(StringencyIndex ~ Robustez_I + densidad_pob
              +Pib_per_prctg+ log(infections) +pob_65mayor+
                 log(Decesos)###jala mejor con muertes
              , data = medidas,
  
              
                        index = c("CountryName", "year"), model = "within")
 
 summary(fixed)
 
 plot_model(fixed)
 
 # Calculamos los efectos marginales
 marg_eff <- margins(fixed1)
 
 plot(marg_eff, type= "h", main="Modelo 2 Fixed",
      xlab = 'Variables', ylab = "Politicas de contención")+ abline(h = 0, col = "blue")
 
 
 #poner nombre en los ejes
 axis(1, at = 1:length(variable_names), labels = variable_names, las = 1)
 
 
 # Agregar las etiquetas de las variables en el eje x
 
 # Crear un vector con los nombres deseados para las variables
 # Asegúrate de que el orden de los nombres corresponda con el orden de las variables en df_marg_eff
 variable_names <- c("Robustez Institucional", "Desidad poblacional", "ingreso", "Contagios", "Adultos mayores", "Decesos")
 
 axis(1, at = 1:length(variable_names), labels = variable_names, las = 2)
 
 
 ?plot
 
 
 ####plot model para EM
 # Convertir marg_eff a un data.frame
 
 
 
 # Cargar el paquete ggplot2
 library(ggplot2)
 
 
 
 
 fijos33 <- lmer( StringencyIndex~ Robustez_I+ log(infections)+ 
                   log(densidad_pob)+ log(Decesos)+Pib_per_prctg+ pob_65mayor+
                   
                  Camas_hosp+(1 | year)  , data =  medidas)
 summary(fijos33)
 summary(fixed)
 
 plot_model(fijos33)
 

 names(medidas)
 head(medidas)
 summary(medidas)
 
 fixed1 <- lm(StringencyIndex ~ Robustez_I + densidad_pob
              +Pib_per_prctg+ log(infections) +pob_65mayor+
                Camas_hosp+ log(Decesos)###jala mejor con muertes
              , data = medidas)
 
 
plot_model(fixed11)

plot_model ( fixed11, tipo  =  "eff" )

 ### Cambiar la variable Robustez_ins a Robustez_I para ver el efecto del incremento en .01 unidades en robuztes

  medidas <- medidas %>%
   mutate(Robustez_I=Robustez_ins * 100)

 library(dplyr)
 
 head(medidas)
 
 ##Por cada 1 unida porcentual de incremento en la robuztes institucional
 #Variable discreta de 0 a 1, incrementara la politica de contencio
 ## de contencion en 221.71 Variable
 ######Modelo de H1 robustez institucional
 Hipotesis
 H3
 
 names(medidas)
 
 fixed2 <- plm(StringencyIndex ~ Pob_extrem + Pib_per_prctg+
                 log(infections)+ log(Decesos)+ Camas_hosp+ 
                 pob_65mayor  + log(densidad_pob), data = medidas,
               index = c("CountryName", "year"), model = "within")
 
 summary(fixed2)
 
 
 
 marg_eff <- margins(fixed2)
 
 plot(marg_eff, type= "h", main="Modelo 3 Fixed",
      xlab = 'Variables', ylab = "Politicas de contención")+ abline(h = 0, col = "blue")
 
 
 pib percapita, controlar
 
 Usar 1.93 dolares para pobreza extrema
 
 fixed2 <- lm(StringencyIndex ~ Pob_extrem + Pib_per_prctg +
                 + log(infections)+ log(Decesos)+ Camas_hosp+
                 pob_65mayor  + log(densidad_pob), data = medidas)
 plot_model(fixed2)
 
## El modelo de pobreza en la poblacion no muestra significacncia Estadistica
 
 hipotesis
 H4
 
 fixed3 <- plm(StringencyIndex ~ Cred_pob + Camas_hosp +log( densidad_pob) +
                 pob_65mayor+ Pib_per_prctg+log(Decesos) + log(infections)
                
    ,  medidas, index = c("CountryName", "year"), model = "within")
 
 summary(fixed3)
 
 
 marg_eff <- margins(fixed3)
 
 plot(marg_eff, type= "h", main="Modelo 4 Fixed",
      xlab = 'Variables', ylab = "Politicas de contención")+ abline(h = 0, col = "blue")
 
 
 
 fixed3 <- lm(StringencyIndex ~ Cred_pob + Camas_hosp +log( densidad_pob) +
                 pob_65mayor+ Pib_per_prctg+log(Decesos) + log(infections)
               
               ,  medidas)
 
 summary(fixed3)
 
 plot_model(fixed3)
 
 dim(medidas)
 medidas <- pdata.frame(medidas)
 
 ###robustez multiplicar por 100
 
 names(medidas)
 
 ##### Modelo explicando la informalida por pais por medio de una variable proxy 
 ####Credito formal en la poblacion. Mayor informalidad mayor medida
 ##de contención es una relacion directa de causalidad
 ### variable Cred_pob es en terminos porcentuales de 1 a 100, 
 ####valores continuos ejem 33.3, 35.9 77.43... n+.1
 
 
 ############ Modelo para hipoteis H5 
 #### Utilizamos El default spred en porcentaje Caer en impago del pais
 ##para asociar que un mayor defaul baja la politica de contencion, paises en
 ### desarrollo manejan sus creditos pais para salvaguardar la economia.
 
 Hipotesis
 H5
 
 fixed4 <- plm(StringencyIndex ~ Defaul_spred+ log(densidad_pob) + ###Defaul Spred se ajusta mejor
    pob_65mayor+ Pib_per_prctg+log(Decesos) + log(infections)+
      Camas_hosp, medidas, index = c("CountryName", "year"), model = "within")
 
 predict.plm(model, newdata = newdata_pdata)
 
 
 library(plm)
 newdata_pdata <- pdata.frame(newdata, index = c("CountryName", "year"))
 
 summary(fixed4)
 
 plot_model(fixed4)
 
 library(sjPlot)
 
 # Asumiendo que 'fixed' es tu modelo
 plot_model(fixed4, type = "pred",terms = "Defaul_spred", axis.title = "Espacio fiscal")
 
 
 marg_eff <- margins(fixed)
 
 plot(marg_eff, type= "h", main="Modelo 2 Fixed",
      xlab = 'Variables', ylab = "Politicas de contención")+ abline(h = 0, col = "blue")
 
 
 
 medidas <- medidas %>%
   mutate(cds=CDS / 100)
 
 
 fixed4 <- plm(StringencyIndex ~ cds+ log(densidad_pob) + ###Defaul Spred se ajusta mejor
                 pob_65mayor+ Pib_per_prctg+log(Decesos) + log(infections)+
                 Camas_hosp, medidas)
 
 summary(fixed4)
 ##################################
 
 ####  Hacer el grafico que contenga los EM de los 4 modelos fixedn
 install.packages("sjPlot")
 library(sjPlot)
 library(ggplot2)
 library(gridExtra)
 
 # Crea los gráficos de efectos marginales para cada modelo
 plot1 <- plot_model(fixed1, type = "pred")
 plot2 <- plot_model(fixed2, type = "pred")
 plot3 <- plot_model(fixed3, type = "pred")
 plot4 <- plot_model(fixed4, type = "pred")
 

 p
 
 # Combina los gráficos en una cuadrícula
 grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

 plot_model(fixed1, type = "pred", terms = "Robustez_I")
 
 # Crea los gráficos de efectos marginales para cada modelo
 plot1 <- plot_model(fixed1, type = "eff", terms = names(coef(fixed1)[-1]))
 plot2 <- plot_model(fixed2, type = "eff", terms = names(coef(fixed2)[-1]))
 plot3 <- plot_model(fixed3, type = "eff", terms = names(coef(fixed3)[-1]))
 plot4 <- plot_model(fixed4, type = "eff", terms = names(coef(fixed4)[-1]))
 
 
 # Crea los gráficos de efectos marginales para las primeras cuatro variables de cada modelo
 plot1 <- plot_model(fixed1, type = "eff", terms = names(coef(fixed1))[-1][1:1])
 plot2 <- plot_model(fixed2, type = "eff", terms = names(coef(fixed2))[-1][1:1])
 plot3 <- plot_model(fixed3, type = "eff", terms = names(coef(fixed3))[-1][1:1])
 plot4 <- plot_model(fixed4, type = "eff", terms = names(coef(fixed4))[-1][1:1])
 
 plot1
 # Crear coefplots para cada modelo Este ESSSSS
 plot1 <- plot_model(fixed1, type = "eff", show.values = TRUE, show.p = TRUE)
 plot2 <- plot_model(fixed2, type = "eff", show.values = TRUE, show.p = TRUE)
 plot3 <- plot_model(fixed3, type = "eff", show.values = TRUE, show.p = TRUE)
 plot4 <- plot_model(fixed4, type = "eff", show.values = TRUE, show.p = TRUE)
 
 plot1
 # Combina los gráficos en una cuadrícula
 grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
 
 # Combina los gráficos en una cuadrícula
 grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
 
 Asume que "fixed1" es tu modelo con efectos fijos
 # Calcular los efectos marginales
 marginal_effects <- margins(fixed)
 
 # Crear el gráfico
 plot(marginal_effects)
 # Combina los gráficos en una cuadrícula
 grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
 
 
 # Asegúrate de tener el paquete effects instalado
 # install.packages("effects")
 
 # Cargar la librería
 library(effects)
 
 # Asume que "fixed1" es tu modelo con efectos fijos
 # Calcular los efectos
 eff <- effect("Robustez_I", fixed1)
 
 # Crear el gráfico
 plot(eff)
 
 
 
#### Modelo alternativo utilizando CDS Credit Defaul Swap
 names(medidas) 
 
 
 fixed5 <- plm(StringencyIndex ~ CDS + log(densidad_pob) +log(infections)+
                  pob_65mayor+ Pib_per_prctg+log(Decesos)
               , medidas, index = c("CountryName", "year"), model = "within")
 
 summary(fixed5)
 
 install.packages("performance")
 
 library(performance)
 R.Version()
 
 ?version
 
 r2(fixed5)

 ### elomdelo tambien resulta estadisticamente significativo
 
 ###Realizar sus efectos marginales
 
 ############
 ########### Variable Intrumenta Modelo IV
 #########
 
 library(readr)
 Investigacion_2 <- read_csv("Bases_datos_doctorado_investigacion/Investigacion_2.csv")
 Inves_cov <- read_csv("Bases_datos_doctorado_investigacion/Inves_cov.csv")
 Investigacion_2 <- subset(Investigacion_2, year !=2019) 
 
 promedio_instrumental <- aggregate(CDS ~ CountryName, data = Investigacion_2, FUN = mean)
 promedio_instrumental <- aggregate(Defaul_spred ~ CountryName, data = Inves_2, FUN = mean) ##3CDS
 
 ###Hacemos el pormedio de los CDS para meterlo en un solo periodo y equiparar con
 ## la Diferencia de votacion
 
 datos <- merge(Inves_cov, promedio_instrumental, by = "CountryName") ##unir ambas bases
 
 ivmodel <- ivreg(Diferencia_voto ~ StringencyIndex*Crecimiento | CDS*Crecimiento, data = datos)
 summary(ivmodel)
 
 iv_model <- ivreg(Diferencia_voto ~ CDS*Crecimiento, data = datos)#3regresion intrumental
 summary((iv_model))
 ########################## Modelo con De
 
 ivmodel <- ivreg(Diferencia_voto ~ StringencyIndex | Defaul_spred*Crecimiento, data = datos)
 summary(ivmodel)
 
 iv_model <- ivreg(Diferencia_voto ~ Defaul_spred*Crecimiento, data = datos)#3regresion intrumental
 summary((iv_model))
 
 ###No hay un resultado estadisti significativo de la IV
 
 install.packages("sandwich")
 library(sandwich)
 
 iv_model <- ivreg(formula = Diferencia_voto ~ Defaul_spred*Crecimiento, data = datos)
 
 summary(iv_model, robust = "CR1")
 
 
 # Ajuste por errores estándar robustos
 robust_iv_model <- coeftest(iv_model, vcov = sandwich, cluster = factor(Continente))
 
 # Obtención de los coeficientes y estadísticas de prueba
 summary(robust_iv_model)
 
 El valor del CDS se expresa en puntos base (basis points), que representan 
 una centésima parte de un punto porcentual. Los puntos base se utilizan para
 medir el costo de protección o el riesgo percibido de incumplimiento. 
 Cuanto mayor sea el valor del CDS en puntos base, mayor se considera el riesgo
 de crédito asociado al país o entidad.
 
 Los rangos de valores típicos para los CDS pueden variar según el país o entidad específica y las condiciones del mercado. Generalmente, se considera que los CDS tienen los siguientes rangos de valores:
   
   Bajos: Menos de 100 puntos base. Indican un bajo riesgo percibido de incumplimiento y una mayor confianza en la capacidad de pago de la deuda.
 Moderados: Entre 100 y 500 puntos base. Sugieren un riesgo moderado de incumplimiento y cierta preocupación sobre la capacidad de pago de la deuda.
 Altos: Entre 500 y 1,000 puntos base. Reflejan un alto riesgo percibido de incumplimiento y una preocupación significativa sobre la capacidad de pago de la deuda.
 Muy altos: Más de 1,000 puntos base. Indican un riesgo extremadamente alto de incumplimiento y una falta de confianza en la capacidad de pago de la deuda.
 ##################################################################
 #####
 #### Default Spred
 ###################################################################
 El Default Spread, o "spread" por su término en inglés, se refiere al diferencial de tasas de interés entre los bonos soberanos de un país y los bonos libres de riesgo, generalmente los bonos del Tesoro de Estados Unidos. Este diferencial representa el rendimiento adicional que los inversionistas exigen para asumir el riesgo crediticio asociado con la deuda soberana de un país en comparación con una inversión libre de riesgo.
 
# El Default Spread se mide en puntos base, al igual que los CDS. Los puntos base representan una centésima parte de un punto porcentual. En el caso del Default Spread, se calcula tomando la diferencia entre el rendimiento de los bonos soberanos y los bonos del Tesoro de Estados Unidos y se expresa en puntos base.
 
 Los rangos de valores típicos para el Default Spread pueden variar según el país y las condiciones del mercado. Sin embargo, en general, se pueden considerar los siguientes rangos y su connotación:
   
   
   
   Graficas de valores diferentes 
 
 #paquete esquisse recomendable para plots, examinacion de graficos
 install.packages("esquisse")
 library(esquisse)
 esquisser(Investigacion_2)
 