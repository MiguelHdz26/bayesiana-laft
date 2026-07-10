library(dplyr)
library(tidyverse)
#install.packages("coda")
library(coda)

# %%%%%%%%%%%%%%%%%%%%% Def. Muestreador  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modelo_2_sin_alpha <- function(X, Y, B, n_burn, thin, sigma_0, beta_0, a_sig, 
                               b_sig, a_lambda, b_lambda, a_tau, b_tau, a_mu, b_mu,
                               a_omega, b_omega, n_d, D){
  
  # Dimensiones
  N <- nrow(Y) # Optimizado (mejor que ncol(t(Y)))
  total_iter <- B + n_burn
  n_guardados <- floor(B / thin) # Para el thinning
  
  # Constantes
  XtX <- crossprod(X) # Optimizado (equivalente a t(X) %*% X)
  prec_0 <- solve(sigma_0)
  alpha_sigma <- (N/2) + a_sig
  alpha_tau <- (N/2) + a_tau 
  alpha_omega <- (D/2) + a_omega # hiperparametro extra y el b
  
  # Definir almacenamientos (Sin alpha)
  beta_samples <- matrix(NA, nrow = n_guardados, ncol = ncol(X))
  sig2_samples <- numeric(n_guardados)
  R_samples <- matrix(NA, nrow = n_guardados, ncol = N)
  lambda_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  tau2_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  mu_samples <- matrix(NA, nrow = n_guardados, ncol = 4)
  loglik_samples  <- numeric(n_guardados)
  # Nuevas
  u_d_samples <- matrix(NA, nrow = n_guardados, ncol = D)
  omega2_samples <- numeric(n_guardados)
  
  # Inicializaciones
  beta <- beta_0
  sig2 <- a_sig
  lambda <- c(1, rep(0.5, 3))
  tau2 <- rep(1, 4)
  mu <- rep(a_mu, 4)
  R <- rowMeans(Y)
  u_d <- rep(0, D)
  omega2 <- 1 / rgamma(n=1, shape = a_omega, rate = b_omega)
  Y_pred_sum <- matrix(0, nrow = N, ncol = 4)
  
  for (b in 1:total_iter){
    
    # Para beta (Actualizada)
    v_beta <- solve( (1/sig2) * (XtX) + prec_0 )
    # Se usa crossprod(X, R) que es más rápido que t(X) %*% R
    U <- rep(u_d, n_d) # Podria no funcionar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    diff_beta <- (R - U)
    m_beta <- v_beta %*% (1/sig2 * crossprod(X, diff_beta) + prec_0 %*% beta_0 )      
    beta <- c(mvtnorm::rmvnorm(1, mean = m_beta, sigma = v_beta))
    
 
    
    # Para sigma^2 (Actualizada)
    # alpha_sigma constante
    beta_sigma <- (1/2) * (sum( (R - (U + (X %*% beta)) )^2 ) + b_sig)
    sig2 <- 1 / rgamma(1, shape = alpha_sigma, rate = beta_sigma)
    

    
    # Para R_i (Actualizada)
    v_r <- 1 / ( sum( (lambda^2) / tau2 ) + (1 /sig2))
    Y_centrada <- sweep(Y, MARGIN = 2, STATS = mu, FUN = '-')
    m_r <- v_r * ((Y_centrada %*% (lambda / tau2) ) + ( (U + (X %*% beta)) / sig2 ) )
    R <- rnorm(n = N, mean = m_r, sd = sqrt(v_r))
    

    
    # Para lambda_j
    v_lambda <- 1 / ((sum(R^2)/tau2) + 1/b_lambda)
    m_lambda <-  v_lambda * ( (crossprod(Y_centrada, R) / tau2) + (a_lambda / b_lambda) )
    # Seleccionamos de 2 a 4 para que no actualice el 1
    lambda[2:4] <- rnorm(3, mean = m_lambda[2:4], sd = sqrt(v_lambda[2:4]))
    
 
    
    # Para tau^2_j
    # alpha_tau es constante
    dif_tau <- ( Y_centrada - ( R %*% t(lambda) ) )^2
    beta_tau <- b_tau + ( (colSums( dif_tau ) ) /2)
    tau2 <- 1/ rgamma(n=4, shape = alpha_tau, rate = beta_tau)
    
    
    # Para mu_j 
    v_mu <- 1 / ( (N / tau2) + (1/ b_mu) )
    dif_mu <- Y - (R %*% t(lambda)) 
    m_mu <- v_mu * ( (colSums(dif_mu) / tau2) + (a_mu / b_mu) )
    mu <- rnorm(n = 4, mean = m_mu, sd = sqrt(v_mu))
    

    
    # Para U_d (nueva)
    v_u <- 1 / ((n_d/sig2) + (1/omega2))
    sum_errores <- as.vector(tapply((R - (X %*% beta)), rep( c(1:D), n_d), sum))
    m_u <- v_u * ( (sum_errores)/sig2 )
    u_d <- rnorm(n = D, mean = m_u, sd = sqrt(v_u))
    

    
    # Para omega2 (nueva)
    # alpha_omega es constante
    beta_omega <- ( sum(u_d^2) * (1/2) ) + b_omega
    omega2 <- 1/ rgamma(n=1, shape = alpha_omega, rate = beta_omega)
    
    
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
        u_d_samples[i, ] <- u_d
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
    u_d = u_d_samples,
    omega2 = omega2_samples,
    loglik = loglik_samples,
    
    Y_hat = Y_hat, 
    MSE = MSE_b,    
    MAE = MAE_b,
    R2 = R2_b
  ))
  
}


# %%%%%%%%%%%%%%%%%%% Uso %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


# Se demora 202 segundos aproximadamente en correr
inicio <- Sys.time()
cadena_md_2 <- modelo_2_sin_alpha(X, Y, 400000, 30000, 40, sigma_0, beta_0, 0.01, 0.01,
                               0, 100, 0.01, 0.01, 0, 10, 0.1, 0.1, n_d, D)
fin <- Sys.time()
fin - inicio

# Guardar la cadena
saveRDS(cadena_md_2, file = file.path("Cadenas", "cadena_md_2.rds"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cargar la cadena
cadena_md_2 <- readRDS('Cadenas/cadena_md_2.rds')


# Tamanos efectivos de muestras
summary(coda::effectiveSize(cadena_md_2$beta))
summary(coda::effectiveSize(cadena_md_2$sig2))
summary(coda::effectiveSize(cadena_md_2$R))
summary(coda::effectiveSize(cadena_md_2$lambda[, -1]))
summary(coda::effectiveSize(cadena_md_2$tau2))
summary(coda::effectiveSize(cadena_md_2$mu))
summary(coda::effectiveSize(cadena_md_2$u_d))
summary(coda::effectiveSize(cadena_md_2$omega2))
summary(coda::effectiveSize(cadena_md_2$loglik))


# Errores estandar de Montecarlo
summary(apply(X = cadena_md_2$beta, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$beta)))
summary(apply(X = cadena_md_2$R, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$R)))
summary(apply(X = cadena_md_2$lambda[, -1], MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$lambda[, -1])))
summary(apply(X = cadena_md_2$tau2, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$tau2)))
summary(apply(X = cadena_md_2$mu, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$mu)))
summary(apply(X = cadena_md_2$u_d, MARGIN = 2, FUN = sd)/sqrt(coda::effectiveSize(cadena_md_2$u_d)))

sd(cadena_md_2$sig2) / sqrt(coda::effectiveSize(cadena_md_2$sig2))
sd(cadena_md_2$omega2) / sqrt(coda::effectiveSize(cadena_md_2$omega2))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Criterios de informacion

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Recordar cargar el archivo fun_criterios_info.R
source("fun_criterios_info.R") 


# DIC
dic_md2 <- calcular_dic(cadena_md_2$loglik)
print(dic_md2)

# WaIC
# Se demora un poco en correr porque genera una matriz de 1122 X 4488
criterios_md2 <- loo_waic_loglik_puntual(cadena_md_2, Y)
criterios_md2$WAIC

# LOO
criterios_md2$LOO


# Desempeño predictivo
mean(cadena_md_2$MSE)
mean(cadena_md_2$MAE)
mean(cadena_md_2$R2)
