library(tidyverse)
library(readxl)
library(hrbrthemes)

df <- 
  read_xlsx('~/Documents/CD2001B/reto/Datos Encuesta sobre donaciones_clean .xlsx',
          sheet = 'Datos sin procesar')

diccionario <-
  read_xlsx('~/Documents/CD2001B/reto/Datos Encuesta sobre donaciones_clean .xlsx',
            sheet = 'diccionario')

# 1 - empresa 
# 2 - fundacion donante
# 3 - profesionista
# 4 - estudiante
# 5 - institucion educativa
# 6 - asociacion religiosa

# contar datos unicos 
df %>% 
  select(entidad_1) %>% 
  table


df %>% 
  select(entidad_1:entidad_32) %>% 
  filter(entidad_1 == 1) %>% 
  map_dfc(.,
      as.numeric) %>% 
  pivot_longer(cols = entidad_2:entidad_32,
               names_to = 'estado',
               values_to = 'si') %>% 
  drop_na(si) %>% 
  left_join(
    diccionario,
    by = c('estado'='codigo')
  ) %>% 
  mutate(descripcion = str_remove(descripcion,'entidad opera'))