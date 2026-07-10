library(dplyr)
library(ggplot2)
library(readxl)
# mapa
library(sf)
library(tidyverse)
library(corrplot)

# ======================================================================

# Grupo 1 (Covariables explicativas)
df_ce <- read_excel("LAFT.xlsx", sheet = "CONTEXTO ESTRUCTURAL") # x_1
df_cp <- read_excel("LAFT.xlsx", sheet = "CONTEXTO POLÍTICO") # x_2
df_con <- read_excel("LAFT.xlsx", sheet = "CONTRABANDO")  # x_3
df_min <- read_excel("LAFT.xlsx", sheet = "SECTOR PRIMARIO Y MIMERO")  # x_4


# Grupo 1 (Representacion directa del LAFT)
df_ter <- read_excel("LAFT.xlsx", sheet = "TERRORISMO") # y_1
df_confli <- read_excel("LAFT.xlsx", sheet = "CONFLICTO") #y_2
df_crimi <- read_excel("LAFT.xlsx", sheet = "CRIMINALIDAD") #y_3
df_narco <- read_excel("LAFT.xlsx", sheet = "NARCOTRAFICO") # y_4



# Nombres de las hojas del archivo excel
hojas_laft <- c("CONTEXTO ESTRUCTURAL", "CONTEXTO POLÍTICO", "CONTRABANDO", 
                "SECTOR PRIMARIO Y MIMERO", "TERRORISMO", "CONFLICTO", 
                "CRIMINALIDAD", "NARCOTRAFICO")

# Definir el data frame
df <- hojas_laft %>% 
  
  map(~ read_excel("LAFT.xlsx", sheet = .x)) %>% 
  
  reduce(full_join, by = 'CODIGO') %>% 
  
  separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')

# ======================================================================


# ---------------------------------
# Grupo 1
# ---------------------------------

# Para Clasificacion de riesgo electoral

df$RIESGO_ELEC <- factor(df$RIESGO_ELEC, 
                                 levels = c("Bajo", "Medio", "Alto", "Extremo"))

# Este se ve bien (Incluida) (bayes_plot_2)
ggplot(data = df, aes(x = RIESGO_ELEC , y = IPM_2018)) + 
  geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  theme_minimal()

