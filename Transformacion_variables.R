library(dplyr)
library(tidyverse)
library(fastDummies)
library(moments)

#install.packages("fastDummies")
#install.packages('moments')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cargar los datos

# Grupo 1 (Representacion directa del LAFT)
df_ter <- read_excel("LAFT.xlsx", sheet = "TERRORISMO") # y_1
df_confli <- read_excel("LAFT.xlsx", sheet = "CONFLICTO") #y_2
df_crimi <- read_excel("LAFT.xlsx", sheet = "CRIMINALIDAD") #y_3
df_narco <- read_excel("LAFT.xlsx", sheet = "NARCOTRAFICO") # y_4


# Grupo 2 (Covariables explicativas)
df_ce <- read_excel("LAFT.xlsx", sheet = "CONTEXTO ESTRUCTURAL") # x_1
df_cp <- read_excel("LAFT.xlsx", sheet = "CONTEXTO POLÍTICO") # x_2
df_con <- read_excel("LAFT.xlsx", sheet = "CONTRABANDO")  # x_3
df_min <- read_excel("LAFT.xlsx", sheet = "SECTOR PRIMARIO Y MIMERO")  # x_4


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Funciones

# Funcion para calcular el sesgo
Calc_Sesgo <- function(datos) {
  sesgo <- datos %>% 
    select(where(is.numeric)) %>% 
    
    # la funcion skewness de la libreria moments calcula el coeficiente de asimetria
    summarise(across(everything(), ~ moments::skewness(.x, na.rm = TRUE))) %>%
    
    tidyr::pivot_longer(cols = everything(), 
                        names_to = "Variable", 
                        values_to = "Coeficiente_Sesgo") %>%

    arrange(desc(abs(Coeficiente_Sesgo)))
  
  print(sesgo)
}

# Funcion para agregar los puntajes de cada bloque tematico
agregar_puntaje_bloque <- function(df_maestro, df_bloque, nombre_puntaje) {
  
  # Calcular el puntaje del bloque
  df_temp <- df_bloque %>%
    mutate(
      # Para poder asignar un nombre manualmente
      !!sym(nombre_puntaje) := rowMeans(pick(where(is.numeric)), na.rm = TRUE)
    ) %>%
    # Tomamos las variables de interes
    select(CODIGO, !!sym(nombre_puntaje))
  
  # Unir el resultado
  df_actualizado <- df_maestro %>%
    left_join(df_temp, by = 'CODIGO')
  
  return(df_actualizado)
}

# Funcion de estandarizacion (la funcion scale me dio algunos problemas)
estandarizar <- function(datos){
  datos <- datos %>% 
    mutate(
      across(
        .cols = where(is.numeric), 
        .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
      )
    )
  return(datos)
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Grupo 1

# Inicializar el data frame de los puntajes de y
punt_y <- df_ter %>% select(CODIGO)


# %%%%%%%%%%%%%%%%%%%%  Terrorismo  %%%%%%%%%%%%%%%%%%%%
 
# Verificar el sesgo de las variables
Calc_Sesgo(df_ter)

# Aplicar transformacion logaritmica a todas las conlumnas ya que todas tienen 
# un alto sesgo (> |1.0|)
df_ter <- df_ter %>% mutate(across(where(is.numeric), ~ as.numeric(log(.x+1)) ))

# Estandarizar 
df_ter <- estandarizar(df_ter)

# Agregar puntaje
punt_y <- agregar_puntaje_bloque(punt_y, df_ter, 'punt_ter')


# %%%%%%%%%%%%%%%%%%%%  Conflicto %%%%%%%%%%%%%%%%%%%%

# Calcular el sesgo de las variables
Calc_Sesgo(df_confli)

# Todas las variables tiene un alto sesgo
df_confli <- df_confli %>% mutate(across(where(is.numeric), ~ as.numeric(log(.x+1)) ))

# Estandarizar las variables
df_confli <- estandarizar(df_confli)

# Agregar puntaje
punt_y <- agregar_puntaje_bloque(punt_y, df_confli, 'punt_confli')



# %%%%%%%%%%%%%%%%%%%%  Criminalidad  %%%%%%%%%%%%%%%%%%%%  

# Calcular el sesgo
Calc_Sesgo(df_crimi)

# Dada la alta cantidad de variables en este bloque se recomienda visualizar con
#View(Calc_Sesgo(df_crimi))

# Todas las variables son de conteo y tienen un sesgo muy alto
df_crimi <- df_crimi %>% mutate(across(where(is.numeric), ~ as.numeric(log(.x+1)) ))

# Estandarizacion
df_crimi <- estandarizar(df_crimi)

# Agregar puntaje
punt_y <- agregar_puntaje_bloque(punt_y, df_crimi, 'punt_crimi')



# %%%%%%%%%%%%%%%%%%%%  Narcotrafico  %%%%%%%%%%%%%%%%%%%%

# Calcular el sesgo
Calc_Sesgo(df_narco)

# Todas las variables tienen un sesgo muy alto
df_narco <- df_narco %>% mutate(across(where(is.numeric), ~ as.numeric(log(.x+1)) ))

# Estandarizacion
df_narco <- estandarizar(df_narco)

# Agregar puntaje
punt_y <- agregar_puntaje_bloque(punt_y, df_narco, 'punt_narco')


# %%%%%%%%%%%%%%%%%%%%  resumen  %%%%%%%%%%%%%%%%%%%%
# La variable punt_ter corresponde a y_1
# La variable punt_confli corresponde a y_2
# La variable punt_crimi corresponde a y_3
# La variable punt_narco corresponde a y_4


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




# Grupo 2

# Inicializar el data frame de los puntajes de x
punt_x <- df_ter %>% select(CODIGO)



# %%%%%%%%%%%%%%%%%%%%  Contexto Estructural  %%%%%%%%%%%%%%%%%%%%

# Calcular el sesgo
Calc_Sesgo(df_ce)

# Las variables de conteo corresponden a las 2 primeras y las que tienen un sesgo,
# alto mayor que |1| son las que le siguen
con_ce <- c('POBLACIÓN', 'BANCOS_2022', 'COBERT_INTERNET_2018', 'COBERT_ENER_ELEC_2019',
            'TASA_ANALF_2018')

# Se transforman unicamente las variables anteriores
df_ce <- df_ce %>% mutate(across(all_of(con_ce), ~ as.numeric(log(.x+1)) ))

df_ce <- estandarizar(df_ce)

# Agregar puntaje
punt_x <- agregar_puntaje_bloque(punt_x, df_ce, 'punt_ce')



# %%%%%%%%%%%%%%%%%%%%  Contexto politico  %%%%%%%%%%%%%%%%%%%%

# Cambiamos el nombre de la variable porque tenia un espacio
colnames(df_cp)[3] <- "ZONA_FRANCA"

# Todas son binarias o categoricas, por lo que se crean bariables dummy para 
# las variables binarias

df_cp <- df_cp %>%
  dummy_cols(
    select_columns = c('PRESIDENCIA', 'ZONA_FRANCA'),
    remove_selected_columns = TRUE,  # Borra la columna original
    remove_first_dummy = TRUE        # quita una de las dummy 
  )

# Ahora para la categorica, como es ordinal se convierte a escala numerica
df_cp <- df_cp %>%
  mutate(
    RIESGO_ELEC_NUM = case_when(
      RIESGO_ELEC == "Bajo" ~ 1,
      RIESGO_ELEC == "Medio" ~ 2,
      RIESGO_ELEC == "Alto" ~ 3,
      RIESGO_ELEC == "Extremo" ~ 4
    )) %>%
  # Se quita la categorica
  select(-RIESGO_ELEC)

# Ahora si se puede estandarizar
df_cp <- estandarizar(df_cp)

# Agregar puntaje
punt_x <- agregar_puntaje_bloque(punt_x, df_cp, 'punt_cp')



# %%%%%%%%%%%%%%%%%%%%  Contrabando  %%%%%%%%%%%%%%%%%%%%

# primero se modifican los nombres
colnames(df_con)[3] <- "PUERTO_MARITIMO"
colnames(df_con)[5] <- "PUERTO_FLUVIAL"

# Como todas son binarias, se usaran variables dummies
df_con <- df_con %>%
  dummy_cols(
    select_columns = c('FRONTERA', 'PUERTO_MARITIMO', 'AEROPUERTO', 'PUERTO_FLUVIAL'),
    remove_selected_columns = TRUE,  # Borra la columna original
    remove_first_dummy = TRUE        # quita una de las dummy 
  )

# Se estandarizan las variables
df_con <- estandarizar(df_con)

# Se agrega el puntaje
punt_x <- agregar_puntaje_bloque(punt_x, df_con, 'punt_con')

# %%%%%%%%%%%%%%%%%%%%  Mineria  %%%%%%%%%%%%%%%%%%%%

# Calcular el sesgo
Calc_Sesgo(df_min)

# Como todas las variables tienen alto sesgo
df_min <- df_min %>% mutate(across(where(is.numeric), ~ as.numeric(log(.x+1)) ))

# Estandarizar variables
df_min <- estandarizar(df_min)

punt_x <- agregar_puntaje_bloque(punt_x, df_min, 'punt_min')

# %%%%%%%%%%%%%%%%%%%%  resumen  %%%%%%%%%%%%%%%%%%%%
# La variable punt_ce corresponde a x_1
# La variable punt_cp corresponde a x_2
# La variable punt_con corresponde a x_3
# La variable punt_min corresponde a x_4



# %%%%%%%%%%%%%%%%%%%%  opcional  %%%%%%%%%%%%%%%%%%%%
# Eliminar los data frames de los bloques para limpiar memoria
rm(list = c('df_ce', 'df_con', 'df_confli', 'df_cp',
            'df_crimi', 'df_min', 'df_narco', 'df_ter'))



# %%%%%%%%%%%%%%%%%%%%  Exportación final  %%%%%%%%%%%%%%%%%%%%
# Guardamos el data frame de los puntajes
saveRDS(punt_y, file = "punt_y.rds")

saveRDS(punt_x, file = "punt_x.rds")

