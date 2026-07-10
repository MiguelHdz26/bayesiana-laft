library(dplyr)
library(tidyverse)
#install.packages("coda")
library(coda)
#install.packages('spdep')
library(spdep)
library(ggplot2)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Preámbulo

source('fun_criterios_info.R')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Calcular estadisticas de los parametros

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Cargar la cadena 
cadena_md_3 <- readRDS('Cadenas/cadena_md_3.rds')


# Hallar las estadisticas de los parametros

est_beta <- calcular_estadisticas(cadena_md_3$beta)
est_sig2 <- calcular_estadisticas(cadena_md_3$sig2)

est_mu <- calcular_estadisticas(cadena_md_3$mu)
est_tau2 <- calcular_estadisticas(cadena_md_3$tau2)
est_tau2 <- calcular_estadisticas(cadena_md_3$lambda)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Graficos de log - verosimiltud

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cargar las cadenas
cadena_md_1 <- readRDS('Cadenas/cadena_md_1.rds')
cadena_md_2 <- readRDS('Cadenas/cadena_md_2.rds')
cadena_md_3 <- readRDS('Cadenas/cadena_md_3.rds')

# Rango para las graficas
yrange <- range(cadena_md_1$loglik, cadena_md_3$loglik)

# Este es el grafico (bayes_plot_log_6)

# Entorno de la grafica
par(mfrow = c(1, 3))

# Modelo 1
plot(cadena_md_1$loglik, type = "l", col = "gray50", 
     xlab = "Iteración", ylab = "Log-likelihood", 
     main = "", ylim = yrange)
# Agregar la media
abline(h = mean(cadena_md_1$loglik), lty = 2, col = "black")
# Subtitulo
mtext("(a) MLR-LAFT", side = 1, line = 4) 


# Modelo 2
plot(cadena_md_2$loglik, type = "l", col = "gray50", 
     xlab = "Iteración", ylab = "Log-likelihood", 
     main = "", ylim = yrange)
abline(h = mean(cadena_md_2$loglik), lty = 2, col = "black")
mtext("(b) MLR-LAFT-ED", side = 1, line = 4)


# Modelo 3
plot(cadena_md_3$loglik, type = "l", col = "gray50", 
     xlab = "Iteración", ylab = "Log-likelihood", 
     main = "", ylim = yrange)
abline(h = mean(cadena_md_3$loglik), lty = 2, col = "black")
mtext("(c) MLR-LAFT-EED", side = 1, line = 4)

# Cerrar entorno
dev.off()



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Grafico oruga para efectos departamentales

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cargar datos para extraer los nombres
Y <- readRDS('punt_y.rds')

# Extraer los nombres
Y <- Y %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-') %>% 
  arrange(departamento)

# La funcion: calcular estadisticas esta en 'fun_criterios_info.R'
phi_stats <- calcular_estadisticas(cadena_md_3$phi_d)

# Crear el data frame
phi_final <- data.frame(
  departamento = Y %>% distinct(departamento),
  media = phi_stats$Media,
  IC_2.5 = phi_stats$IC_2.5,
  IC_97.5 = phi_stats$IC_97.5
)

# Para que se vea mejor el grafico
phi_final[which(phi_final$departamento == 
                  'ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA'), 1] = 'SAN ANDRES'

# Colores de los intervalos
phi_final <- phi_final %>%
  mutate(
    Color_Sig = case_when(
      IC_2.5 > 0 ~ "Significativo Positivo",
      IC_97.5 < 0 ~ "Significativo Negativo",
      TRUE ~ "No Significativo"
    )
  )


# Grafico bayes_plot_cater_1
 
# Aprovechando que ya tengo aca la cadena y los datos voy a hacer una grafica mas
grafico_phi <- ggplot(phi_final, aes(x = media, y = reorder(departamento, media), color = Color_Sig)) +
  # Línea vertical de referencia en 0
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgray", linewidth = 0.8) +
  # Dibujar los intervalos de credibilidad 
  geom_errorbar(aes(xmin = IC_2.5, xmax = IC_97.5), linewidth = 0.6, width = 0) +
  # Dibujar los puntos de la media posterior
  geom_point(size = 1.8) +
  # Asignar los colores exactos de la imagen de referencia
  scale_color_manual(values = c(
    "Significativo Positivo" = "#3B82F6", 
    "No Significativo" = "black",
    "Significativo Negativo" = "#EF4444"
  )) +
  # Etiquetas y títulos
  labs(
    x = "Media Posterior",
    y = NULL 
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

# Visualizar
print(grafico_phi)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Grafico oruga para el riesgo R_i a nivel departamental

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Suponiendo que Y ya se cargo
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

# Para que se vea mejor
colnames(matriz_varsigma)[4] = 'SAN ANDRES'

# Extraemos sus estadisticas
est_varsigma <- calcular_estadisticas(matriz_varsigma)
est_varsigma$departamento <- rownames(est_varsigma)

# Crear la variable de significancia para los colores
est_varsigma <- est_varsigma %>%
  mutate(
    Color_Sig = case_when(
      IC_2.5 > 0 ~ "Significativo Positivo",
      IC_97.5 < 0 ~ "Significativo Negativo",
      TRUE ~ "No Significativo"
    )
  )

# Grafico bayes_plot_cater_2

# Grafico oruga
grafico_varsigma <- ggplot(est_varsigma, aes(x = Media, y = reorder(departamento, Media), color = Color_Sig)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgray", linewidth = 0.8) +
  geom_errorbar(aes(xmin = IC_2.5, xmax = IC_97.5), linewidth = 0.6, width = 0) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c(
    "Significativo Positivo" = "#3B82F6", # Azul
    "No Significativo" = "black",
    "Significativo Negativo" = "#EF4444"  # Rojo
  )) +
  labs(
    x = "Media Posterior",
    y = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13)
  )


print(grafico_varsigma)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# mapa para el riesgo R_i a nivel departamental

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Terciles
cortes_terciles <- quantile(est_varsigma$Media, probs = c(1/3, 2/3))

# Agregar la clasificacion
est_varsigma <- est_varsigma %>%
  mutate(
    riesgo_clasif = case_when(
      Media <= cortes_terciles[1] ~ "Riesgo Bajo",
      Media > cortes_terciles[1] & Media <= cortes_terciles[2] ~ "Riesgo Medio",
      Media > cortes_terciles[2] ~ "Riesgo Alto"
    ),
    # Convertir a factor
    riesgo_clasif = factor(riesgo_clasif, levels = c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo"))
  )

# Invocar las  funciones de los mapas y carga los shapes
source('fun_mapas.R')


# Para que no de errores al general el mapa
est_varsigma[
  which(est_varsigma$departamento == 'SAN ANDRES'), 7] = 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA'

# Generar mapa (bayes_plot_riesgo_1)
mapa_riesgo_dep <- generar_map_dep(map_dep, est_varsigma, map_muni, riesgo_clasif)

mapa_riesgo_dep$layers[[1]]$aes_params$colour <- '#262121'

# Personalizar mapa
mapa_riesgo_dep <- mapa_riesgo_dep +
  scale_fill_manual(
         values = c("Riesgo Bajo" = "#dadada",
                     "Riesgo Medio" = "#e59498",
                     "Riesgo Alto" = "#e36f79"),
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


print(mapa_riesgo_dep)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# mapa para el riesgo R_i a nivel municipal

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Crear estadisticas
est_R <- calcular_estadisticas(cadena_md_3$R)

# Conformar data frame
est_R <- data.frame(
  departamento = Y %>% select(departamento),
  municipio = Y %>% select(municipio),
  Media = est_R$Media 
)

# Terciles
cortes_terciles <- quantile(est_R$Media, probs = c(1/3, 2/3))

# Agregar la clasificacion
est_R <- est_R %>%
  mutate(
    riesgo_clasif = case_when(
      Media <= cortes_terciles[1] ~ "Riesgo Bajo",
      Media > cortes_terciles[1] & Media <= cortes_terciles[2] ~ "Riesgo Medio",
      Media > cortes_terciles[2] ~ "Riesgo Alto"
    ),
    # Convertir a factor
    riesgo_clasif = factor(riesgo_clasif, levels = c("Riesgo Alto", "Riesgo Medio", "Riesgo Bajo"))
  )

# Generar mapa (bayes_plot_riesgo_2)
mapa_riesgo_muni <- generar_mapa_municipal(map_muni, est_R, riesgo_clasif)

# Cambiar el  color de las lineas y el grosor
# mapa_riesgo_muni$layers[[1]]$aes_params$colour <- '#F5F2F2'
mapa_riesgo_muni$layers[[1]]$aes_params$linewidth <- 0.00001

# Configurar la estetica
mapa_riesgo_muni <- mapa_riesgo_muni +
  scale_fill_manual(
      values = c("Riesgo Bajo" = "#dadada",
                 "Riesgo Medio" = "#e59498",
                 "Riesgo Alto" = "#e36f79"),
      name = "Nivel de Riesgo LAFT"
    ) +
    labs(
      x = 'Longitud', y = 'Latitud'
    ) +
    guides(fill = guide_legend(override.aes = list(color = "black", linewidth = 0.5))) +
    theme_minimal() +
    theme(
      #axis.text = element_blank(),
      #axis.ticks = element_blank(),
      #panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

# es 8x8 pulgadas
print(mapa_riesgo_muni)
