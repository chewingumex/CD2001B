library(tidyverse)
library(readxl)
library(hrbrthemes)

no_donaciones <- read_excel('~/Documents/CD2001B/reto/Datos Encuesta sobre no donaciones.xlsx', sheet = 'Datos sin procesar')


# quitar todas las variables que no sirven
no_donaciones <- 
no_donaciones %>% 
  select(-`Seq. Número`,
         -`Referencia externa`, 
         -`Tiempo necesario para completar (segundos)`,
         - Duplicar,
         -`Código de país`,
         -`Estado de respuesta`,
         -`Lista de correo`,
         -`Correo electrónico del encuestado`,
         -starts_with("Variable personalizada"))


# quitar espacios en blanco al final de los nombres de las variables... neta....
# nombrar a las variables de forma ordenada, corta y util

no_donaciones <- 
  no_donaciones %>% 
  rename_with(.fn = str_trim, .cols = everything()) %>% 
  rename(., 
         'donador_pasado' = '¿Ha donado alguna vez en el pasado?',
         'considerar_donar' = '¿Ha considerado donar en el último año?',
         'rechazar_donar' = '¿Ha rechazado la solicitud de alguna organización para ser donante?',
         'org_negado_servicio' = '¿Alguna organización de la sociedad civil ha negado alguna vez un servicio?',
         'razonesno_recursos' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Falta de recursos financieros',
         'razonesno_tiempo' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Falta de tiempo o personal',
         'razonesno_conocimiento' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Falta de conocimiento sobre organizaciones o causas dignas de donar',
         'razonesno_politicas' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Políticas que limitan las donaciones a ciertos tipos de organizaciones o causas',
         'razonesno_legal' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Restricciones legales',
         'razonesno_burocracia' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Políticas internas o burocracia',
         'razonesno_otro' = '¿Cuáles son las razones por las que no ha donado en el pasado? - Otro',
         'incentivado_informacion' = '¿Cómo cree que podría ser incentivado para donar en el futuro? - Mayor información sobre las organizaciones o causas a las que se puede donar',
         'incentivado_flexibilidad' = '¿Cómo cree que podría ser incentivado para donar en el futuro? - Mayor flexibilidad en las políticas de donaciones',
         'incentivado_financiamiento' = '¿Cómo cree que podría ser incentivado para donar en el futuro? - Mayor financiamiento para poder donar',
         'incentivado_otro' = '¿Cómo cree que podría ser incentivado para donar en el futuro? - Otro',
         'id'= 'ID de respuesta',
         'region'='Región')

# recodificar los valores 
no_donaciones <-
no_donaciones %>% 
  mutate(
    across(
      where(is.numeric), ~ case_when(. == 2 ~ 0,
                                     . == 1 ~ 1)
    )
) %>% 
  mutate(
    across(
      contains("otro"), ~ case_when(!is.na(.) ~ 1)
      )
    )
  

# ejemplo -- inicio de una prueba de hipotesis con un t-test pareado?

no_donaciones %>% 
  group_by(donador_pasado, considerar_donar) %>% 
  summarise(
    n = n()
  )

## visualizar distribuciones de razones por las cuales no donar 
## para grupos de donantes y no donantes

no_donaciones %>%
  select(donador_pasado,
         starts_with('razonesno')) %>% 
  pivot_longer(names_transform = ~ str_remove(., 'razonesno_'),
               cols = starts_with('razonesno'),
               names_to = 'razon no donar',
               values_to = 'donante'
               ) %>%
  drop_na(donante) %>% 
  ggplot(aes(donante, group = `razon no donar`, fill = `razon no donar`)) +
  geom_bar(stat = 'count', 
           position = 'dodge',
           alpha=0.7) +
  theme_ipsum() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'bottom') +
  facet_wrap(~donador_pasado)
  



