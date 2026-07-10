library(factoextra)
library(ggplot2)
library(dplyr)
# install.packages('forcats')
library(forcats)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Es necesario cargar el archivo 'fun_criterios_info.R'
source('fun_criterios_info.R')

# Para los mapas
source('fun_mapas.R')


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Procesamiento para k means a nivel departamental

# Cargar la cadena
cadena_md_3 <- readRDS('Cadenas/cadena_md_3.rds')

# Cargar Y para sacar los nombres
Y <- readRDS('punt_y.rds')

# Organizar los nombres
Y <- Y %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-') %>% 
  arrange(departamento)

# Extraer unicamente los nombres de los departamentos
Y_deptos <- as.matrix(Y %>% distinct(departamento))


# Crear matriz vacía para almacenar el riesgo departamental
matriz_varsigma <- matrix(NA, nrow = nrow(cadena_md_3$R), ncol = length(Y_deptos))
# Asignar los nombres de los departamentos
colnames(matriz_varsigma) <- Y_deptos


# Calcular el promedio por departamento
for (depto in Y_deptos) {
  # Identificar las columnas de los municipios que pertenecen a este departamento
  indices_mun <- which( Y %>% select(departamento) == depto)
  
  # Si el departamento tiene más de un municipio, promediamos las filas
  if(length(indices_mun) > 1) {
    matriz_varsigma[, depto] <- rowMeans(cadena_md_3$R[, indices_mun])
  } else {
    # Si por alguna razón tiene un solo municipio
    matriz_varsigma[, depto] <- cadena_md_3$R[, indices_mun]
  }
}


# Extraemos sus estadisticas
est_varsigma <- calcular_estadisticas(matriz_varsigma)
est_varsigma$departamento <- rownames(est_varsigma)

# Grafico para comprobar la cantidad de clusters
# Para reproducibilidad
set.seed(123456789) 

# Grafico (bayes_plot_kmeans_codo)
grafico_codo <- fviz_nbclust(as.matrix(est_varsigma$Media), kmeans, method = "wss") +
  labs(
    title = "",
    x = "Número de Clústeres (k)",
    y = "Suma de Cuadrados Intra-Clúster (WSS)"
  ) +
  theme_minimal()

print(grafico_codo)

# Con el grafico concluimos que  k = 4

# Para reproducibilidad
set.seed(123456789)
modelo_km <- kmeans(as.matrix(est_varsigma$Media), centers = 4, nstart = 25)

# Ordenar
est_varsigma <- est_varsigma %>%
  mutate(
    # Guardar el clúster original asignado
    Cluster_Kmeans = as.factor(modelo_km$cluster),
    # Reordenar los clústeres basándose en el valor de la Media
    Cluster_Kmeans = fct_reorder(Cluster_Kmeans, Media)
  )

# Renombrar los niveles
levels(est_varsigma$Cluster_Kmeans) <- c("Riesgo Bajo", "Riesgo Medio", "Riesgo Alto", "Riesgo Extremo")

# Verificar cuántos departamentos quedaron en cada grupo
table(est_varsigma$Cluster_Kmeans)


# Generar mapa (bayes_plot_kmeans_1)
mapa_kmeans_dep <- generar_map_dep(map_dep, est_varsigma, map_muni, Cluster_Kmeans)

mapa_kmeans_dep$layers[[1]]$aes_params$colour <- '#262121'

mapa_kmeans_dep <- mapa_kmeans_dep +
  scale_fill_manual(
      values = c("Riesgo Extremo" = "#de425b",
                 "Riesgo Alto" = "#e36f79",
                 "Riesgo Medio" = "#e59498",
                 "Riesgo Bajo" = "#dadada"
                 ),
      name = "Nivel de Riesgo LAFT"
    ) +
    labs(
      x = 'Longitud', y = 'Latitud'
    ) +
    theme_minimal() +
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      #panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

print(mapa_kmeans_dep)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Procesamiento para k means a nivel municipal


# Crear estadisticas
est_R <- calcular_estadisticas(cadena_md_3$R)

# Conformar data frame
est_R <- data.frame(
  departamento = Y %>% select(departamento),
  municipio = Y %>% select(municipio),
  Media = est_R$Media 
)

# Para reproducibilidad
set.seed(123456789)

# Grafico (bayes_plot_kmeans_codo_2)
grafico_codo_2 <- fviz_nbclust(as.matrix(est_R$Media), kmeans, method = "wss") +
  labs(
    title = "",
    x = "Número de Clústeres (k)",
    y = "Suma de Cuadrados Intra-Clúster (WSS)"
  ) +
  theme_minimal()

print(grafico_codo_2)

# Para reproducibilidad
set.seed(123456789)
modelo_km_muni <- kmeans(as.matrix(est_R$Media), centers = 4, nstart = 25)

# Ordenar
est_R <- est_R %>%
  mutate(
    # Guardar
    Cluster_Kmeans = as.factor(modelo_km_muni$cluster),
    # Reordenar
    Cluster_Kmeans = fct_reorder(Cluster_Kmeans, Media)
  )

# Renombrar los niveles 
levels(est_R$Cluster_Kmeans) <- c("Riesgo Bajo", "Riesgo Medio", "Riesgo Alto", "Riesgo Extremo")

# Verificar la distribución de los municipios
table(est_R$Cluster_Kmeans)



# Generar mapa (bayes_plot_kmeans_2)
mapa_kmeans_mun <- generar_mapa_municipal(map_muni, est_R, Cluster_Kmeans)

# Cambiar el  color de las lineas y el grosor
# mapa_kmeans_mun$layers[[1]]$aes_params$colour <- '#F5F2F2'
mapa_kmeans_mun$layers[[1]]$aes_params$linewidth <- 0.00001

# Modificar mapa
mapa_kmeans_mun <- mapa_kmeans_mun +
  scale_fill_manual(
    values = c("Riesgo Extremo" = "#de425b",
               "Riesgo Alto" = "#e36f79",
               "Riesgo Medio" = "#e59498",
               "Riesgo Bajo" = "#dadada"
    ),
    name = "Nivel de Riesgo LAFT"
  ) +
  labs(
    x = 'Longitud', y = 'Latitud'
  ) +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.5))) +
  theme(
    #axis.text = element_blank(),
    #axis.ticks = element_blank(),
    #panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(mapa_kmeans_mun)








