library(dplyr)
library(tidyverse)
library(loo)

# Funciones generales
calcular_dic <- function(vector_loglik) {
  # Calcular el vector de devianza
  devianza <- -2 * vector_loglik
  
  # Calcular la complejidad del modelo (pD)
  pD <- var(devianza) / 2
  
  # Calcular el DIC final
  DIC <- mean(devianza) + pD
  
  return(list(DIC = DIC, pD = pD))
}



# Funcion de los criterios LOO y WAIC
loo_waic_loglik_puntual <- function(cadena, Y, N = 1122, J = 4) {
  S <- nrow(cadena$R) 
  matriz_ll <- matrix(NA, nrow = S, ncol = N * J)
  Y_vec <- as.numeric(Y)
  
  # Halla la log veriosimilitud puntual
  for (i in 1:S) {
    R_i <- cadena$R[i, ]
    lambda_i <- cadena$lambda[i, ]
    mu_i <- cadena$mu[i, ]
    tau2_i <- cadena$tau2[i, ]
    
    m_log <- sweep(matrix(R_i) %*% t(lambda_i), MARGIN = 2, STATS = mu_i, FUN = "+")
    sd_log <- matrix(sqrt(tau2_i), nrow = N, ncol = J, byrow = TRUE)
    
    # Calcular la verosimilitud puntual
    matriz_ll[i, ] <- dnorm(x = Y_vec, mean = as.numeric(m_log), sd = as.numeric(sd_log), log = TRUE)
  }
  
  # Calcular el WAIC
  waic_mod <- loo::waic(matriz_ll)
  
  # Calcular el LOO
  loo_mod <- loo::loo(matriz_ll)
  
  return(list(
    WAIC = waic_mod,
    LOO = loo_mod
  ))
}

# Nota: esta funcion aplica para todos los modelos ya que los cambios se hacen
# unicamente en R_i, porque se esta usando la segunda parte, la ecuacion de medicion


# Función para extraer todas las estadísticas de una matriz MCMC
calcular_estadisticas <- function(matriz_mcmc) {

  if(is.null(dim(matriz_mcmc))) {
    matriz_mcmc <- matrix(matriz_mcmc, ncol = 1)
  }
  
  # Media y Desviación Estándar
  Media <- apply(matriz_mcmc, 2, mean)
  SD <- apply(matriz_mcmc, 2, sd)
  
  # Coeficiente de Variación
  CV <- SD / abs(Media) 
  
  # Intervalos de Credibilidad al 95%
  IC_inf <- apply(matriz_mcmc, 2, quantile, probs = 0.025)
  IC_sup <- apply(matriz_mcmc, 2, quantile, probs = 0.975)
  
  # Probabilidad Direccional P(beta > 0)
  P_mayor_0 <- apply(matriz_mcmc, 2, function(x) mean(x > 0))
  
  # Guardar
  resultados <- data.frame(
    Media = round(Media, 3),
    SD = round(SD, 3),
    CV = round(CV, 3),
    IC_2.5 = round(IC_inf, 3),
    IC_97.5 = round(IC_sup, 3),
    P_mayor_0 = round(P_mayor_0, 3)
  )
  
  return(resultados)
}
