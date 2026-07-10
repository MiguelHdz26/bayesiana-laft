library(dplyr)
library(tidyverse)
#install.packages("coda")
library(coda)
#install.packages('spdep')
library(spdep)

modelo_3_sin_alpha <- function(X, Y, B, n_burn, thin, sigma_0, beta_0, a_sig, 
                               b_sig, a_lambda, b_lambda, a_tau, b_tau, a_mu, b_mu,
                               a_omega, b_omega, n_d, D, m_d, W){
  
  # Dimensiones
  N <- nrow(Y) 
  total_iter <- B + n_burn
  n_guardados <- floor(B / thin) # Para el thinning
  
  # Constantes
  XtX <- crossprod(X) 
  prec_0 <- solve(sigma_0)
  id_depto <- rep(1:D, n_d)
  alpha_sigma <- (N/2) + a_sig
  alpha_tau <- (N/2) + a_tau 
  alpha_omega <- ( (D - 1) / 2 ) + a_omega # hiperparametro extra y el b
  
  # Definir almacenamientos (Sin alpha)
  beta_samples <- matrix(NA, nrow = n_guardados, ncol = ncol(X))
  sig2_samples <- numeric(n_guardados)
  R_samples <- matrix(NA, nrow = n_guardados, ncol = N)
  lambda_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  tau2_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  mu_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  loglik_samples  <- numeric(n_guardados)
  # Nuevas
  phi_d_samples <- matrix(NA, nrow = n_guardados, ncol = D)
  omega2_samples <- numeric(n_guardados)
  
  # Inicializaciones
  beta <- beta_0
  sig2 <- a_sig
  lambda <- c(1, rep(0.5, 3))
  tau2 <- rep(1, 4)
  mu <- rep(a_mu, 4)
  R <- rowMeans(Y)
  phi_d <- rep(0, D)
  omega2 <- 1 / rgamma(n=1, shape = a_omega, rate = b_omega)
  Y_pred_sum <- matrix(0, nrow = N, ncol = 4)
  
  for (b in 1:total_iter){
    
    # Para beta (Actualizada)
    v_beta <- solve( (1/sig2) * (XtX) + prec_0 )

    Phi <- rep(phi_d, n_d) 
    diff_beta <- (R - Phi)
    m_beta <- v_beta %*% (1/sig2 * crossprod(X, diff_beta) + prec_0 %*% beta_0 )      
    beta <- c(mvtnorm::rmvnorm(1, mean = m_beta, sigma = v_beta))
    
    
    
    # Para sigma^2 (Actualizada)
    # alpha_sigma constante
    beta_sigma <- (1/2) * (sum( (R - (Phi + (X %*% beta)) )^2 ) + b_sig)
    sig2 <- 1 / rgamma(1, shape = alpha_sigma, rate = beta_sigma)
    
    
    
    # Para R_i (Actualizada)
    v_r <- 1 / ( sum( (lambda^2) / tau2 ) + (1 /sig2))
    Y_centrada <- sweep(Y, MARGIN = 2, STATS = mu, FUN = '-')
    m_r <- v_r * ((Y_centrada %*% (lambda / tau2) ) + ( (Phi + (X %*% beta)) / sig2 ) )
    R <- rnorm(n = N, mean = m_r, sd = sqrt(v_r))
    
    
    
    # Para lambda_j (No cambia)
    v_lambda <- 1 / ((sum(R^2)/tau2) + 1/b_lambda)
    m_lambda <-  v_lambda * ( (crossprod(Y_centrada, R) / tau2) + (a_lambda / b_lambda) )
    # Seleccionamos de 2 a 4 para que no actualice el 1
    lambda[2:4] <- rnorm(3, mean = m_lambda[2:4], sd = sqrt(v_lambda[2:4]))
    
    
    
    # Para tau^2_j (No cambia)
    # alpha_tau es constante
    dif_tau <- ( Y_centrada - ( R %*% t(lambda) ) )^2
    beta_tau <- b_tau + ( (colSums( dif_tau ) ) /2)
    tau2 <- 1/ rgamma(n=4, shape = alpha_tau, rate = beta_tau)
    
    
    # Para mu_j  (No cambia)
    v_mu <- 1 / ( (N / tau2) + (1/ b_mu) )
    dif_mu <- Y - (R %*% t(lambda)) 
    m_mu <- v_mu * ( (colSums(dif_mu) / tau2) + (a_mu / b_mu) )
    mu <- rnorm(n = 4, mean = m_mu, sd = sqrt(v_mu))
    
    
    
    # Para phi_d
    v_phi <- 1 / ( (n_d / sig2) + (m_d / omega2) )
    
    diff_phi <- R - (X %*% beta) 
    
    for(d in 1:D){
      
      # Hallar los vecinos
      id_vecinos <- which(W[d, ] == 1) # Recordando que 1 es vecino
      
      # Suma unicamente los phi de los vecinos
      sum_phi <- sum(phi_d[id_vecinos]) 
      
      # Suma unicamente las diferencias del departamento d
      sum_diff_phi <- sum(diff_phi[id_depto == d])
      
      # Media para cada departamento
      m_phi_d <- v_phi[d] * ( (sum_diff_phi / sig2) + (sum_phi / omega2) )
      
      # Actualizar cada phi
      phi_d[d] <- rnorm(1, mean = m_phi_d, sd = sqrt(v_phi[d]))
    }
    
    # Una vez se han calculdo todos los phi, se pone la condicion de indentificabilidad
    phi_d <- phi_d - mean(phi_d)
    
    
    # Nota: El vector completo de phi_d (Phi) se re escribe en beta, por eso no lo vuelvo a anotar
    
    
    
    # Para omega2 (nueva) (Revisar)
    # Alpha es constante
    diff_omega <- ( sum(m_d * phi_d^2) - as.numeric(crossprod(phi_d, W %*% phi_d)) )/2
    
    # Actualizamos omega2
    beta_omega <- b_omega + diff_omega
    omega2 <- 1 / rgamma(n = 1, shape = alpha_omega, rate = beta_omega)
    
    
    # Guardado con thinning
    if (b > n_burn){
      
      paso <- b - n_burn
      
      if(paso %% thin == 0){
        
        i <- paso / thin
        # Guardar las muestras
        beta_samples[i, ] <- beta
        sig2_samples[i] <- sig2
        R_samples[i, ] <- R
        lambda_samples[i, ] <- lambda
        tau2_samples[i, ] <- tau2
        mu_samples[i, ] <- mu
        phi_d_samples[i, ] <- phi_d
        omega2_samples[i] <- omega2
        
        # log verosimilitud
        m_log <- sweep(R %*% t(lambda), MARGIN = 2, STATS = mu, FUN = "+")
        sd_log <- matrix(sqrt(tau2), nrow = N, ncol = 4, byrow = TRUE)
        loglik_samples[i] <- sum(dnorm(x = Y, mean = m_log, sd = sd_log, log = TRUE))
        
        
        Y_rep <- rnorm(n = N * 4, mean = as.numeric(m_log), sd = as.numeric(sd_log))
        
        # Volver a darle forma de matriz (N x 4)
        Y_rep_mat <- matrix(Y_rep, nrow = N, ncol = 4)
        
        # Acumular la suma
        Y_pred_sum <- Y_pred_sum + Y_rep_mat
      }
    }
    
    # Avance
    if (b %% ceiling(total_iter / 10) == 0) {
      cat(paste0("Iteración ", b, " de ", total_iter, " (", round(100 * b / total_iter), "%)\n"))
    }
    
  }
  
  colnames(beta_samples) <- paste0("beta_", 1:ncol(X))
  colnames(lambda_samples) <- paste0("lambda_", 1:4)
  colnames(tau2_samples) <- paste0("tau2_", 1:4)
  colnames(mu_samples) <- paste0("mu_", 1:4)
  colnames(R_samples) <- paste0("muni_", 1:N)
  
  # Media posterior de las predicciones
  Y_hat <- Y_pred_sum / n_guardados
  colnames(Y_hat) <- paste0("y_hat_b", 1:4)
  
  # MSE (Error Cuadrático Medio)
  MSE_b <- colMeans((Y - Y_hat)^2)
  names(MSE_b) <- paste0("MSE_b", 1:4)
  
  # MAE (Error Absoluto Medio)
  # colMeans calcula 1/n * sumatoria automáticamente
  MAE_b <- colMeans(abs(Y - Y_hat))
  names(MAE_b) <- paste0("MAE_b", 1:4)
  
  # R-cuadrado (Coeficiente de Determinación)
  # Numerador: Suma de los errores al cuadrado
  SS_res <- colSums((Y - Y_hat)^2)
  
  # Denominador: Suma total de cuadrados (varianza real de los datos)
  # Usamos sweep para restarle la media de cada delito (colMeans(Y)) a los datos reales (Y)
  SS_tot <- colSums(sweep(Y, MARGIN = 2, STATS = colMeans(Y), FUN = "-")^2)
  
  R2_b <- 1 - (SS_res / SS_tot)
  names(R2_b) <- paste0("R2_b", 1:4)
  
  return(list(
    beta = beta_samples,
    sig2 = sig2_samples,
    R = R_samples,
    lambda = lambda_samples,
    tau2 = tau2_samples,
    mu = mu_samples,
    phi_d = phi_d_samples,
    omega2 = omega2_samples,
    loglik = loglik_samples,
    
    Y_hat = Y_hat, 
    MSE = MSE_b,    
    MAE = MAE_b,
    R2 = R2_b
  ))
  
}



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Se carga el shape departamental para hallar W
map_dep <- st_read("deptos/MGN_DPTO_POLITICO.shp")
map_dep <- map_dep %>% select(DPTO_CNMBR, geometry) %>% arrange(DPTO_CNMBR)

# Segun el shape busca que departamentos comparten puntos en su geometry
list_vecinos <- spdep::poly2nb(map_dep, queen = TRUE)

# Lo hace un arreglo matricial
W <- spdep::nb2mat(list_vecinos, style = 'B', zero.policy = TRUE)

# Hallar la cantidad de vecinos
m_d <- rowSums(W)


# Cargar el data frame con los datos
Y <- readRDS('punt_y.rds')
X <- readRDS('punt_x.rds')

X <- X %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')
Y <- Y %>% separate(col = CODIGO, into = c('departamento', 'municipio'), sep = '-')

X <- X %>% arrange(departamento)
Y <- Y %>% arrange(departamento)

n_d <- X %>% count(departamento) %>% select(n)
n_d <- as.vector(n_d$n)

D <-  as.numeric(n_distinct(X[1]))

X <- as.matrix(X[-c(1,2)])
Y <- as.matrix(Y[-c(1,2)])

# inicializar 
sigma_0 = 100 * diag(4)
beta_0 = rep(0, 4)

# Establecer una semilla para replicabilidad
set.seed(123456789)

# Generar cadena
# Tarda 4.6 min 276 segundos
inicio <- Sys.time()
cadena_md_3 <- modelo_3_sin_alpha(X, Y, 400000, 30000, 40, sigma_0, beta_0, 0.01, 0.01,
                               0, 100, 0.01, 0.01, 0, 10, 0.1, 0.1, n_d, D, n_d, W)

fin <- Sys.time()
fin - inicio

# Guardar la cadena
saveRDS(cadena_md_3, file = file.path("Cadenas", "cadena_md_3.rds"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cargar la cadena
cadena_md_3 <- readRDS('Cadenas/cadena_md_3.rds')


# Tamanos efectivos de muestras
summary(coda::effectiveSize(cadena_md_3$beta))
summary(coda::effectiveSize(cadena_md_3$sig2))
summary(coda::effectiveSize(cadena_md_3$R))
summary(coda::effectiveSize(cadena_md_3$lambda[, -1]))
summary(coda::effectiveSize(cadena_md_3$tau2))
summary(coda::effectiveSize(cadena_md_3$mu))
summary(coda::effectiveSize(cadena_md_3$phi_d))
summary(coda::effectiveSize(cadena_md_3$omega2))


# Errores estandar de Montecarlo
summary(apply(X = cadena_md_3$beta, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$beta)))
summary(apply(X = cadena_md_3$R, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$R)))
summary(apply(X = cadena_md_3$lambda[, -1], MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$lambda[, -1])))
summary(apply(X = cadena_md_3$tau2, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$tau2)))
summary(apply(X = cadena_md_3$mu, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$mu)))
summary(apply(X = cadena_md_3$phi_d, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_3$phi_d)))

sd(cadena_md_3$sig2) / sqrt(coda::effectiveSize(cadena_md_3$sig2))
sd(cadena_md_3$omega2) / sqrt(coda::effectiveSize(cadena_md_3$omega2))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Criterios de informacion

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Recordar cargar el archivo fun_criterios_info.R
source("fun_criterios_info.R") 


# DIC
dic_md3 <- calcular_dic(cadena_md_3$loglik)
print(dic_md3)

# WaIC
# Se demora un poco en correr porque genera una matriz de 1122 X 4488
criterios_md3 <- loo_waic_loglik_puntual(cadena_md_3, Y)
criterios_md3$WAIC

# LOO
criterios_md3$LOO



# Desempeño predictivo
mean(cadena_md_3$MSE)
mean(cadena_md_3$MAE)
mean(cadena_md_3$R2)
