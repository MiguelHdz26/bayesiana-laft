library(dplyr)
library(ggplot2)
library(readxl)
# mapa
library(sf)
library(tidyverse)
library(stringi)
library(patchwork)
library(fuzzyjoin)



# Funciones generales para los mapas


# Función para modificar los nombres de los departamentos y municipios
mod_nombres <- function(datos, col_depto, col_muni) {
  
  # Funcion interna para estandarizar texto
  aplanar_texto <- function(texto) {
    texto %>%
      str_to_lower() %>%                              # Minúsculas
      stri_trans_general("Latin-ASCII") %>%           # Quitar tildes
      str_remove_all("[[:punct:]]") %>%               # Quitar puntuación
      str_remove_all("\\s+")                          # Quitar TODOS los espacios
  }
  
  # Procesar siempre la columna de departamentos
  datos_limpios <- datos %>%
    mutate(depto_limpio = aplanar_texto({{ col_depto }}))
  
  # Si se proporcionó el argumento col_muni, se procesa
  if (!missing(col_muni)) {
    datos_limpios <- datos_limpios %>%
      mutate(muni_limpio = aplanar_texto({{ col_muni }}))
  }
  
  return(datos_limpios)
}


# Funcion para unir el mapa con los datos
unir_map_df <- function(mapa, datos){
  # Datos temporales
  
  # Son los municipios donde en ambos df coinciden sus nombres
  mapa_perfecto <- inner_join(mapa, datos, by = c("depto_limpio", "muni_limpio"))
  # Municipios del mapa que no estan en los datos
  mapa_huerfano <- anti_join(mapa, datos, by = c("depto_limpio", "muni_limpio"))
  # Municipios de los datos que no estan en el mapa
  datos_huerfanos <- anti_join(datos, mapa, by = c("depto_limpio", "muni_limpio"))
  
  # Busca nombres de municipios semejantes
  mapa_rescatado <- stringdist_left_join(
    x = mapa_huerfano, 
    y = datos_huerfanos, 
    by = c("depto_limpio", "muni_limpio"),
    method = "jw",
    max_dist = 0.15,
    distance_col = "distancia"
  )
  
  # Limpiar el df de variables auxiliares creadas por el paso anterior
  mapa_rescatado_limpio <- mapa_rescatado %>%
    select(-ends_with(".y"), -distancia) %>% 
    rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))
  
  # Agrega los demas municipios
  mapa_final <- bind_rows(mapa_perfecto, mapa_rescatado_limpio)
  
  return(mapa_final)
}

generar_mapa_municipal <- function(mapa, datos, variable){
  # Creacion de variables auxiliares
  datos <- mod_nombres(datos, departamento, municipio)
  mapa <- mod_nombres(mapa, DEPTO, MPIO_CNMBR)
  
  mapa_muni <- unir_map_df(mapa, datos)
  
  # Determinar las coordenadas
  # San Andres
  is_and <- mapa_muni %>% 
    filter(muni_limpio == 'sanandres' & depto_limpio == 'archipielagodesanandresprovidenciaysantacatalina')
  # Providencia
  is_prov <- mapa_muni %>% 
    filter(muni_limpio == 'providencia' & depto_limpio == 'archipielagodesanandresprovidenciaysantacatalina')
  
  # Cambio de coordenadas de la isla de providencia para que quede cerca a San Andres
  st_geometry(is_prov) <- (st_geometry(is_prov) * 1) + c(-0.28, -0.8)
  is_prov <- st_set_crs(is_prov, st_crs(mapa_muni))
  
  # Se juntan en un solo objeto ambas islas y se modifican sus coordenadas
  islas_bloque <- bind_rows(is_and, is_prov)
  st_geometry(islas_bloque) <- (st_geometry(islas_bloque) * 10) + c(739, -114)
  islas_bloque <- st_set_crs(islas_bloque, st_crs(mapa_muni))
  
  grafico <- ggplot() +
    # Mapa de Colombia sin San Andres y providencia
    geom_sf(data = mapa_muni %>% filter(depto_limpio != 'archipielagodesanandresprovidenciaysantacatalina'
                                        | is.na(depto_limpio)), 
            aes(fill = {{ variable }}), color = "#616161", linewidth = 0.1) +
    # Se agregan las islas
    geom_sf(data = islas_bloque, aes(fill = {{ variable }}), color = "#616161", linewidth = 0.1) +
    theme_minimal() 
  
  return(grafico)
}

# Funcion para generar los mapas departamentales
generar_map_dep <- function(map_dep, datos, map_muni, variable){
  
  # Agregar los nombres planos para compararlos
  datos <- mod_nombres(datos, departamento)
  map_dep <- mod_nombres(map_dep, DPTO_CNMBR)
  map_muni <- mod_nombres(map_muni, DEPTO, MPIO_CNMBR)
  
  # Unir los datos con el mapa departamental
  mapa_total <-  map_dep %>% left_join(datos, by = c('depto_limpio' = 'depto_limpio'))
  
  # Tomamos el shape a nivel municipal de ambas islas
  is_and <- map_muni %>% 
    filter(muni_limpio == 'sanandres' & depto_limpio == 'archipielagodesanandresprovidenciaysantacatalina')
  # Providencia
  is_prov <- map_muni %>% 
    filter(muni_limpio == 'providencia' & depto_limpio == 'archipielagodesanandresprovidenciaysantacatalina')
  
  # Cambio de coordenadas de la isla de providencia para que quede cerca a San Andres
  st_geometry(is_prov) <- (st_geometry(is_prov) * 1) + c(-0.28, -0.8)
  is_prov <- st_set_crs(is_prov, st_crs(map_dep))
  is_and <- st_transform(is_and, st_crs(map_dep))
  
  # Se juntan en un solo objeto ambas islas y se modifican sus coordenadas
  islas_bloque <- bind_rows(is_and, is_prov)
  st_geometry(islas_bloque) <- (st_geometry(islas_bloque) * 10) + c(739, -114)
  islas_bloque <- st_set_crs(islas_bloque, st_crs(map_dep))
  
  # Esta islas_bloque no tiene las variables, por lo qu es necesario agregarlos
  
  # Seleccionamos el nombre exacto del departamento de San Andres
  depto_sa_limpio <- map_dep %>% 
    filter(str_detect(depto_limpio, "andres")) %>% 
    pull(depto_limpio) %>% 
    unique()
  
  # Convertimos 'datos' en un data.frame plano y eliminamos su geometry
  # Para no modificar el geometry de islas_bloque
  datos_planos <- as.data.frame(datos)
  if ("geometry" %in% colnames(datos_planos)) datos_planos$geometry <- NULL
  
  # Le asignamos la llave a las islas y traemos las demas variables
  islas_bloque <- islas_bloque %>%
    mutate(depto_limpio = depto_sa_limpio) %>%
    left_join(datos_planos, by = "depto_limpio")
  
  # Crear el grafico
  grafico <- ggplot() +
    # Mapa de Colombia sin San Andres y providencia
    geom_sf(data = mapa_total %>% filter(depto_limpio != 'archipielagodesanandresprovidenciaysantacatalina'
                                         | is.na(depto_limpio)), 
            aes(fill = {{ variable }}), color = "#616161", linewidth = 0.1) +
    # Se agregan las islas
    geom_sf(data = islas_bloque, aes(fill = {{ variable }}), color = "#616161", linewidth = 0.1) +
    theme_minimal() 
  
  return(grafico)
}

# Cargar los shapes

# Municipal
map_muni <- st_read('deptos/MGN_MPIO_POLITICO.shp')

map_muni <- map_muni %>% rename(DEPTO = DPTO_CNMBR,
                                MPIO_CNMBR = MPIO_CNMBR) %>% select(DEPTO, MPIO_CNMBR, geometry)

map_muni[ which(map_muni$MPIO_CNMBR == 'MIRITÍ - PARANÁ')  ,2] = 'MIRITI'


# Cargar shape departamental
map_dep <- st_read("deptos/MGN_DPTO_POLITICO.shp")
map_dep <- map_dep %>% select(DPTO_CNMBR, geometry)


print('Shapes cargados')
