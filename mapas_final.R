library(dplyr)
library(ggplot2)
library(readxl)
# mapa
library(sf)
library(tidyverse)
library(stringi)
library(patchwork)
library(fuzzyjoin)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Recordar cargar el fun_mapas.R

# Es donde se encuentran todas las funciones usadas para generar mapas
source('fun_mapas.R')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Creacion del data frame general

# Nombres de las hojas del archivo excel
hojas_laft <- c("CONTEXTO ESTRUCTURAL", "CONTEXTO POLÍTICO", "CONTRABANDO", 
                "SECTOR PRIMARIO Y MIMERO", "TERRORISMO", "CONFLICTO", 
                "CRIMINALIDAD", "NARCOTRAFICO")

# Definir el data frame
df <- hojas_laft %>% 
  
  map(~ read_excel("LAFT.xlsx", sheet = .x)) %>% 
  
  reduce(full_join, by = 'CODIGO') %>% 
  
  separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Nota: Primero se crean los pares de mapas y despues se juntan en 2
# Todos los mapas fueron exportados como pdfs en 8x8 pulgadas

# Mapa de indice de probreza (Contexto estructural)


# Creacion del mapa
mapa_ipm <- generar_mapa_municipal(map_muni, df, IPM_2018)

mapa_ipm <- mapa_ipm + 
  scale_fill_gradient(low = "#dadada", high = "#de425b",
                      na.value = "#3B3B3B", 
                      name = "Nivel de IPM") + 
  theme(legend.position = "right") +                              
  labs(subtitle = "(1) Indice de Pobreza Multidimensional", x = 'Longitud', y = 'Latitud')



# Mapa de riesgo electoral (Contexto politico)

# Agregar orden a  
df <- df %>% mutate(RIESGO_ELEC = 
                            factor(RIESGO_ELEC, c("Bajo", "Medio", "Alto", "Extremo") ))

# Creacion del mapa
mapa_ries_po <- generar_mapa_municipal(map_muni, df, RIESGO_ELEC)

mapa_ries_po <- mapa_ries_po + 
  scale_fill_manual(
    values = c(
      'Extremo' = '#de425b',
      'Alto' = '#e36f79',
      'Medio' = '#e59498',
      'Bajo' = '#dadada'
    ),
    na.value = "#3B3B3B",         
    name = "Nivel de Riesgo"     
  ) + 
  theme(legend.position = "right") +                              
  labs(subtitle = "(2) Distribución del Riesgo Electoral", x = 'Longitud', y = 'Latitud')




# Condicion de municipio fronterizo (Contrabando)

mapa_front <- generar_mapa_municipal(map_muni, df, FRONTERA)

mapa_front <- mapa_front + 
        scale_fill_manual(
          values = c(
            'NO' = '#dadada',
            'SI' = '#538f49'
          ),
          name = 'Frontera'
          ) +
        theme(legend.position = 'right') +
        labs(subtitle = '(1) Condición de frontera', x = 'Longitud', y = 'Latitud')




# Mapa de titulos mineros (Sector primario y minero)

mapa_min <- generar_mapa_municipal(map_muni, df, TITULOS_MIN_2023)

mapa_min$layers[[1]]$aes_params$colour <- '#808080'
mapa_min$layers[[1]]$aes_params$linewidth <- 0.0001

mapa_min <- mapa_min + 
        scale_fill_gradient(
          low = "#dadada", high = "#de425b",
          name = 'Titulos Mineros'
          ) +
        theme(legend.position = 'right') +
        labs(subtitle = '(2) Distribución de los títulos mineros', x = 'Longitud', y = 'Latitud')



# Mapas que se presentan en el documento:

# Primer mapa (bayes_plot_1)
print((mapa_ipm | mapa_ries_po) +
      plot_annotation(caption = 'Nota: Los municipios grises son datos faltantes',
                      theme = theme(
                        plot.caption = element_text(hjust = 0.5,       
                                                    size = 10,         
                                                    face = "italic")
                      )))

# Segundo mapa (bayes_plot_3)
print((mapa_front | mapa_min) + 
        plot_annotation(caption = 'Nota: Los municipios grises son datos faltantes',
        theme = theme(
        plot.caption = element_text(hjust = 0.5,       
                                    size = 10,         
                                    face = "italic")
        ))
      )


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mapas de la variables de observacion de manifestaciones

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Nota: Primero se crean los pares de mapas y despues se juntan en 2

# Hectareas de cultivos de coca

mapa_coca <- generar_mapa_municipal(map_muni, df, H_COCA_2022)

mapa_coca$layers[[1]]$aes_params$colour <- '#808080'
mapa_coca$layers[[1]]$aes_params$linewidth <- 0.0001

mapa_coca <- mapa_coca + 
    scale_fill_gradient(
      low = "#dadada", high = "#de425b",
      na.value = '#3B3B3B',
      name = 'Hectáreas de coca'
    ) +
    theme(legend.position = 'right') +
    labs(subtitle = '(1) Distribución de las hectáreas de coca', x = 'Longitud', y = 'Latitud')


# Incidencia del conflicto armado

mapa_incid <- generar_mapa_municipal(map_muni, df, IND_INCIDENCIA_CONFLIC_2021)

mapa_incid$layers[[1]]$aes_params$colour <- '#808080'
mapa_incid$layers[[1]]$aes_params$linewidth <- 0.0001

mapa_incid <- mapa_incid + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Valor del índice'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(2) Niveles del índice de incidencia del conflicto armado', x = 'Longitud', y = 'Latitud')




# tasa de extorsion

mapa_ext <- generar_mapa_municipal(map_muni, df , EXTOR_2023)

mapa_ext$layers[[1]]$aes_params$colour <- '#808080'
mapa_ext$layers[[1]]$aes_params$linewidth <- 0.0001

mapa_ext <- mapa_ext + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Cantidad de casos'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(1) Distribución de los casos de extorsión (2023)', x = 'Longitud', y = 'Latitud')




# Indice de riesgo de victimizacion

mapa_vitm <- generar_mapa_municipal(map_muni, df, RISK_VICTIM_2022)

mapa_vitm$layers[[1]]$aes_params$colour <- '#808080'
mapa_vitm$layers[[1]]$aes_params$linewidth <- 0.0001

mapa_vitm <- mapa_vitm + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Nivel de índice'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(2) Índice de riesgo de victimización', x = 'Longitud', y = 'Latitud')





# Unir los mapas

# Tercer mapa (bayes_plot_4)
print((mapa_coca | mapa_incid) + 
        plot_annotation(caption = 'Nota: Los municipios grises son datos faltantes',
                        theme = theme(
                          plot.caption = element_text(hjust = 0.5,       
                                                      size = 10,         
                                                      face = "italic")
                        ))
)


# cuarto mapa (bayes_plot_5)
print((mapa_ext | mapa_vitm) + 
        plot_annotation(caption = 'Nota: Los municipios grises son datos faltantes',
                        theme = theme(
                          plot.caption = element_text(hjust = 0.5,       
                                                      size = 10,         
                                                      face = "italic")
                        ))
)







# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mapas departamentales de los puntajes 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Nota: Es importante cargar el shape de municipios para que funcione 
# correctamente esta parte del codigo

# Se carga con el source del principio

# Cargar puntajes
Y <- readRDS('punt_y.rds')
X <- readRDS('punt_x.rds')

# Separar departamentos y municipios 
X <- X %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')
Y <- Y %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')

# Hallar el promedio departamental de cada puntaje
X_prom <- X %>% group_by(departamento) %>% 
  summarise(prom_ce = mean(punt_ce), prom_cp = mean(punt_cp), prom_con = mean(punt_con), 
            prom_min = mean(punt_min)) %>% 
  select(departamento, prom_ce, prom_cp, prom_con, prom_min)

Y_prom <- Y %>% group_by(departamento) %>% 
  summarise(prom_ter = mean(punt_ter), prom_confli = mean(punt_confli), prom_crimi = mean(punt_crimi), 
            prom_narco = mean(punt_narco)) %>% 
  select(departamento, prom_ter, prom_confli, prom_crimi, prom_narco)


# %%%%%%%%%%%%%%%%%%%%%%%  Grupo  1  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%% Terrorismo  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_ter <- generar_map_dep(map_dep, Y_prom, map_muni, prom_ter)

mapa_ter <- mapa_ter + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(a) Puntaje en terrorismo', x = 'Longitud', y = 'Latitud')




# %%%%%%%%%%%%%%%%%%%%%%%%% Conflicto  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_confli <- generar_map_dep(map_dep, Y_prom, map_muni, prom_confli)

mapa_confli <- mapa_confli + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(b) Puntaje en Conflicto Armado', x = 'Longitud', y = 'Latitud')





# %%%%%%%%%%%%%%%%%%%%%%%%% Criminalidad  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_crimi <- generar_map_dep(map_dep, Y_prom, map_muni, prom_crimi)

mapa_crimi <- mapa_crimi + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(c) Puntaje en Criminalidad', x = 'Longitud', y = 'Latitud')





# %%%%%%%%%%%%%%%%%%%%%%%%% Narcotrafico  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_narco <- generar_map_dep(map_dep, Y_prom, map_muni, prom_narco)

mapa_narco <- mapa_narco + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(d) Puntaje en Narcotráfico', x = 'Longitud', y = 'Latitud')






# %%%%%%%%%%%%%%%%%%%%%%%  Grupo  2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%% Contexto estructurales  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_ce <- generar_map_dep(map_dep, X_prom, map_muni, prom_ce)

mapa_ce <- mapa_ce + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(a) Puntaje en Contexto Estructural', x = 'Longitud', y = 'Latitud')




# %%%%%%%%%%%%%%%%%%%%%%%%% Contexto politico  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_cp <- generar_map_dep(map_dep, X_prom, map_muni, prom_cp)

mapa_cp <- mapa_cp + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(b) Puntaje en Contexto Político', x = 'Longitud', y = 'Latitud')




# %%%%%%%%%%%%%%%%%%%%%%%%% Contrabando  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_con <- generar_map_dep(map_dep, X_prom, map_muni, prom_con)

mapa_con <- mapa_con + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(c) Puntaje en Contrabando', x = 'Longitud', y = 'Latitud')




# %%%%%%%%%%%%%%%%%%%%%%%%% Sector primario y minero  %%%%%%%%%%%%%%%%%%%%%%%%

mapa_min <- generar_map_dep(map_dep, X_prom, map_muni, prom_min)

mapa_min <- mapa_min + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Puntaje'
  ) +
  theme(legend.position = 'right') +
  labs(subtitle = '(d) Puntaje en Sector Primario', x = 'Longitud', y = 'Latitud')




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Grupo 1 (bayes_plot_punt_1)
print((mapa_ter | mapa_confli) / (mapa_crimi | mapa_narco))

# Grupo 2 (bayes_plot_punt_2)
print((mapa_ce | mapa_cp) / (mapa_con | mapa_min))






# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mapas departamentales de los efectos espaciales

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Recodar cargar la cadena del modelo 3
# Los shapes se cargan con el source()

# Cargar la cadena
cadena_md_3 <- readRDS('Cadenas/cadena_md_3.rds')

# Cargar datos para extraer los nombres
Y <- readRDS('punt_y.rds')

# Extraer los nombres
Y_deptos <- Y %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-') %>% 
  arrange(departamento)


# La funcion: calcular estadisticas esta en 'fun_criterios_info.R'
source('fun_criterios_info.R')

phi_stats <- calcular_estadisticas(cadena_md_3$phi_d)

# Crear el data frame
phi_final <- data.frame(
  departamento = Y_deptos %>% distinct(departamento),
  media = phi_stats$Media,
  IC_2.5 = phi_stats$IC_2.5,
  IC_97.5 = phi_stats$IC_97.5
)

# Es el grafico bayes_plot_espa_1

# Crear mapa departamental
mapa_efec_dep <- generar_map_dep(map_dep, phi_final, map_muni, media)

# Modificar sus caracteristicas
mapa_efec_dep <- mapa_efec_dep + 
  scale_fill_gradient(
    low = "#dadada", high = "#de425b",
    na.value = '#3B3B3B',
    name = 'Valor del Efecto'
  ) +
  theme(legend.position = 'right') +
  labs(x = 'Longitud', y = 'Latitud')

print(mapa_efec_dep)




