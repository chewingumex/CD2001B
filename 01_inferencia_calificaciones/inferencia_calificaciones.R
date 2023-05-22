library(yaml)
library(tidyverse)
library(hrbrthemes)


##################################
################# leer los nombres

nombres <- read_yaml('Documents/CD2001B/01_inferencia_calificaciones/nombres.yaml')

##################################################
### asignarles calificaciones random entre min max

min_cali <- 7

max_cali <- 9.5

calificaciones <- seq(from = min_cali,
                      to = max_cali,
                      length.out = length(nombres)
)

calificaciones_finales <-
  tibble(
    nombres = nombres,
    calificaciones = calificaciones
  )

##################################################
####################### convertirlo en una función


calificador_automatico <- function(min_cali, max_cali, nombres){
  
  return(
    tibble(
      nombres = nombres,
      calificaciones =  runif(n = length(nombres), 
                              min = min_cali, 
                              max = max_cali
      )
    )
  )
  
}

##################################################
################################ jugar a la ruleta

calificador_automatico(min_cali = min_cali, max_cali = max_cali, nombres = nombres)


##################################################
################################ con mas justicia?

# 9.2
media <- mean(min_cali,max_cali)

desviacion_estandar <- 0.5

calificaciones_justas <-
  rnorm(n=length(nombres), 
        mean = media, 
        sd = desviacion_estandar
  )

calificador_mas_justo <- function(n, mean, sd, nombres){
  
  return(
    tibble(
      nombres = nombres,
      calificaciones =  rnorm(n=length(nombres), 
                              mean = media, 
                              sd = desviacion_estandar)
    )
  )
  
}

calificador_mas_justo(n=length(nombres), 
                      mean = media, 
                      sd = desviacion_estandar, 
                      nombres=nombres)


####################################################
################################ checar que sea neta

calificaciones_finales <-
  calificador_mas_justo(n=length(nombres), 
                        mean = media, 
                        sd = desviacion_estandar, 
                        nombres=nombres
  )

calificaciones_finales %>% 
  ggplot(aes(x=calificaciones)) +
  geom_histogram(fill='lightblue', color='darkgrey', alpha = 0.6, bins=15) +
  geom_vline(aes(xintercept=mean(calificaciones)), linetype='dotted', lwd = 1.5, color='blue') +
  geom_density(color = "red", lwd = 1.5, linetype='dotted') +
  theme_ipsum()


#########################################################
################################# el Tec, me va a cachar?
#########################################################


mean(calificaciones_finales$calificaciones)

# 9.1

## supongamos que la media del Tec es 8.9

media_tec <- 8.9

library(infer)

ci_1 <-
calificaciones_finales %>% 
  specify(response = calificaciones) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  get_ci(level = 0.95)


### intervalo de confianza

calificaciones_finales %>% 
  ggplot(aes(x=calificaciones)) +
  geom_histogram(fill='lightblue', color='darkgrey', alpha = 0.6, bins=15) +
  geom_vline(aes(xintercept=mean(calificaciones)), linetype='dotted', lwd = 1.5, color='blue') +
  geom_vline(aes(xintercept=ci_1$lower_ci), linetype='dashed', lwd = 1.5, color='darkgreen') +
  geom_vline(aes(xintercept=ci_1$upper_ci), linetype='dashed', lwd = 1.5, color='darkgreen') +
  geom_density(color = "red", lwd = 1.5, linetype='dotted') +
  theme_ipsum()


### nivel de confianza

ci_list <- list()

for (repetition in 1:100){
  
  ci_list[[repetition]] <- 
    calificaciones_finales %>% 
    specify(response = calificaciones) %>% 
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean") %>%
    get_ci(level = 0.95)
  
}

  ggplot(reduce(ci_list, 
                bind_rows) %>% 
           mutate(n = 1:nrow(.),
                  media_tec = media_tec) %>% 
           mutate(cae_dentro = between(media_tec,lower_ci,upper_ci))) +
  geom_segment(aes(x=lower_ci,
                   xend=upper_ci,
                   y = n,
                   yend = n,
                   color=cae_dentro)) +
  geom_point(aes(x = upper_ci, y = n, color=cae_dentro)) +
  geom_point(aes(x = lower_ci, y = n, color=cae_dentro)) +
  geom_vline(xintercept = media_tec, color = "darkgray") +
    xlab("intervalos de confianza") +
    ylab("muestra bootstrap") +
    theme_ipsum() +
    theme(
      legend.position = 'bottom'
    )
  



  
  
  
