#Autores
#Kevin Macias
#Cristian  Cardona Vega

#Ejercicio 1
hembras <- c(183.2 ,184.1 ,183.0
             ,204.3,176.5 ,179.0 
             ,188.3 ,186.8 ,202.2
             ,182.5 ,190.0 ,178.1 
             ,193.2 ,180.4 ,184.3 
             ,189.2 ,189.1 ,203.1
             ,166.8 ,196.3 ,193.3
             ,187.3 ,185.8 ,189.3 
             ,195.5 ,202.4 ,210.8 );

machos <- c(140.9 	,173.9 	,118.9 
            ,121.7 	,177.4 	,140.0 
            ,173.8 	,154.8 	,192.7 
            ,154.5 	,177.5 	,134.4 
            ,109.2 	,153.4 	,175.0 
            ,150.7 	,138.7 	,169.8 
            ,203.3 	,136.7 	,153.9 
            ,163.0 	,165.3 	,176.7 
            ,137.7 	,126.7 	,150.0);
#a-

hist(hembras,breaks = 5, main = "Distribución de tamaño en especies de langostinos hembra", 
     xlab = "tamaño mm", ylab = "Frecuencia",
     col = "blue")

hist(machos,breaks = 5, main = "Distribución de tamaño en especies de langostinos macho", 
     xlab = "tamaño mm", ylab = "Frecuencia",
     col = "green", add=TRUE)
#segun el resultado del histograma, podemos deducir que los datos del tamaño de
#los machos están mas concentrados entre los 160-180 (asimetrica positiva), y los datos de las
#hembras están más concentrados hacia la mediana y su distribución es normal, 

#b-
  #promedio
  promedio_hembras = mean(hembras);
  promedio_hembras

  promedio_machos = mean(machos);
  promedio_machos
  
  sd_hembras <- sd(hembras);
  sd_hembras
  
  sd_machos <- sd(machos);
  sd_machos
  
  #Se puede concluir que los datos de las hembras están más agrupados con respecto
  #a la media, por lo tanto son más cercanos al valor esperado, en cambio, las muestras
  #obtenidas de los machos, están más dispersas respecto a su media.
  
  #C
  intervalo_hembras <- t.test(hembras,
                      conf.level = 0.97)$conf.int;
  intervalo_hembras
  
  #El 97% de las hembras tienen una medida de entre los 184.7 mm a los 193.4 mm
  
  intervalo_machos <- t.test(machos,
                            conf.level = 0.97)$conf.int;
  intervalo_machos
  
  #El 97% de los machos tienen una medida de entre los 144.2 mm a los 164.6 mm
  
  #D
  bplot_hembras = boxplot(hembras, main='tamaño de las hembras') 
  bplot_machos = boxplot(machos, main='tamaño de los machos') 
  #Las hmbras tienen una distribucion simetrica, en donde el 25% de las hembras
  #tiene medidas por debajo de 183mm y el 75% medidas por debajo de 185mm
  
  ##Los machos tienen una asimetria positiva, en donde el 50% de los machos,
  #tiene una medida entre los 140mm y los 175mm
  
  #E
  shapiro.test(hembras);
  #p>0.05, entonces tenemos una distribución normal en las hembras  
  shapiro.test(machos)
  #p>0.05, tenemos una distribución normal en los machos
  
  
  #Ejercicio 2
  p <- 0.015;
  n <- 900
  
  #a
  prob_menos_25_falsos <- pbinom(25, size = n, prob = p)
  prob_menos_25_falsos 
  
  #La probabilidad de que a lo sumo salgan 25 falsos es de 99%
  
  #b
  prob_mayor_20_falsos <- pbinom(20, size = n, prob = p, lower.tail = FALSE);
  prob_mayor_30_falsos <- pbinom(30, size = n, prob = p, lower.tail = FALSE);
  
  prob_entre_20_y_30 <- prob_mayor_20_falsos - prob_mayor_30_falsos;
  
  prob_entre_20_y_30;
  
  #La probabilidad de que salgan entre 20 y 30 falsos es de
  #0.033 -> 3.3%
  
  #c
  prob_mayor_10_falsos <- pbinom(10, size = n, prob = p, lower.tail = FALSE);
  prob_mayor_10_falsos;#La probabilidad de que mas de 10 sean falsos es de
  #0.79 -> 79%
  
  plot(dbinom(1:25, size = n, prob = p), type = "h", lwd = 2,
       main = "Función de probabilidad binomial",
       ylab = "P(X = x)", xlab = "Número de éxitos")
  
  lines(dbinom(20:30, size = n, prob = p), type = "h",
        lwd = 2, col = rgb(1,0,0, 0.7))
  
  # n = 80, p = 0.3
  lines(dbinom(10:n, size = n, prob = p), type = "h",
        lwd = 2, col = rgb(1,0,0, 0.7)) 
  
  #Ejercicio 3
  
  ## x= habitantes que superaran los 92 años de edad
  #p(x>92) la varianza = 25 la desviacion estandar= 5 media =76  q=92
  1-pnorm(92,76,5)
  par(mfrow = c(1,2))
  x <- seq(60, 95, 0.1)
  plot(x, dnorm(x, mean = 76, sd = 5), type = "l",
       ylim = c(0, 0.08), xlab = "Edad", ylab = "Frecuencia", 
       main=expression(paste("Distribución normal ",mu==76," ", sigma==5)), 
       lwd = 2, col = "red")
  regionX=seq(92,95,0.01)            
  xP <- c(92,regionX,95)  
  yP <- c(0,dnorm(regionX,76,5),0)
  polygon(xP,yP,col="orange1")
  text(90, 0.02, "P(x>92)")
  text(90, 0.01, "0.0687%")
  abline(v = 76)
  
  # cuantos viviran menos de 55 y mas de 75 años de edad
  #p(x<55) para lo de menos de 55
  
  pnorm(55,76,5)
  
  #p(x>75) para los de mas de 75 años
  1-pnorm(75,76,5)
  x <- seq(50, 90, 0.1)
  plot(x, dnorm(x, mean = 76, sd = 5), type = "l",
       ylim = c(0, 0.08), xlab = "Edad", ylab = "Frecuencia", 
       main=expression(paste("Distribución normal ",mu==76," ", sigma==5)), 
       lwd = 2, col = "red")
  regionX1=seq(50,55,0.01)            
  xP <- c(50,regionX1,55)  
  yP <- c(0,dnorm(regionX1,76,5),0)
  polygon(xP,yP,col="orange1")
  regionX2=seq(75,90,0.01)            
  xP <- c(75,regionX2,90)  
  yP <- c(0,dnorm(regionX2,76,5),0)
  polygon(xP,yP,col="orange1")
  text(55, 0.02, "P(x<55)")
  text(55, 0.01, "0.0013%")
  text(80, 0.02, "P(x>75)")
  text(80, 0.01, "57.926%")
  abline(v = 76)
  ((1-pnorm(75,76,5))+pnorm(55,76,5))*100
  
  #a. R/ El 0.0687% de la población superarán los 92 años
  
  #b. R/ Un 57.927% de la poblacion viviran 55 años o más de 75 años