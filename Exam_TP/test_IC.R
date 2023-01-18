setwd("/home/user/Bureau/UTC_GI01/SY02/Exam_TP/")

load("cctp_A2022.RData")

ls()




get_IC_sigma_nmu <- function(echantillon, alpha){
  #cat("1")
  n <- length(echantillon)
  IC_min <- ((n-1) * sd(echantillon)**2) / qchisq(1-alpha/2, df=n-1)
  IC_max <- ((n-1) * sd(echantillon)**2) / qchisq(alpha/2, df=n-1)
  return(c(IC_min, IC_max))
}

get_IC_sigma_mu <- function(echantillon, alpha, mu){
  #cat("2")
  n <- length(echantillon)
  sig_emv2 <- (1/n) * sum((echantillon - mu)**2)
  IC_min <- (n * sig_emv2) / qchisq(1-alpha/2, df=n)
  IC_max <- (n * sig_emv2) / qchisq(alpha/2, df=n)
  return(c(IC_min, IC_max))
}

get_IC_mu_nsigma <- function(echantillon, alpha){
  #cat("3")
  n <- length(echantillon)
  IC_min <- mean(echantillon) - qt(1-alpha/2, df=n-1) * sqrt(sd(echantillon)**2 / n)
  IC_max <- mean(echantillon) + qt(1-alpha/2, df=n-1) * sqrt(sd(echantillon)**2 / n)
  return(c(IC_min, IC_max))
}

get_IC_mu_sigma <- function(echantillon, alpha, sigma){
  #cat("4")
  n <- length(echantillon)
  IC_min <- mean(echantillon) - qnorm(1-alpha/2) * sqrt(sigma**2 / n)
  IC_max <- mean(echantillon) + qnorm(1-alpha/2) * sqrt(sigma**2 / n)
  return(c(IC_min, IC_max))
}

get_IC_mu_nsigma_nongo <- function(echantillon, alpha){
  #cat("5")
  n <- length(echantillon)
  IC_min <- mean(echantillon) - qnorm(1-alpha/2) * sqrt(sd(echantillon)**2 / n)
  IC_max <- mean(echantillon) + qnorm(1-alpha/2) * sqrt(sd(echantillon)**2 / n)
  return(c(IC_min, IC_max))
}


get_IC <- function(echantillon, alpha, autre_param, param_estim, nongo=FALSE){
  if (param_estim == "sigma"){
    #cat("Estimation de sigma")
    
    # estimation de sigma avec mu inconnu 
    if (autre_param == FALSE){
      return(get_IC_sigma_nmu(echantillon, alpha))
    }
    
    # estimation de sigma avec mu connu
    else{
      return(get_IC_sigma_mu(echantillon, alpha, mu=autre_param))
    }
  }
  else{
    #cat("estimation de mu")
    
    # estimation de mu avec sigma inconnu
    if (autre_param == FALSE){
      if (nongo == TRUE){
        return(get_IC_mu_nsigma_nongo(echantillon, alpha))
      }
      return(get_IC_mu_nsigma(echantillon, alpha))
    }
    
    # estimation de mu avec sigma connu
    else{
      return(get_IC_mu_sigma(echantillon, alpha, sigma=autre_param))
    }
  }
}


alpha <- 0.01

# cas unilatéral
#alpha <- 2 * alpha

mu <- 5
var <- 4
sigma <- sqrt(var)
sigma <- 2
n <- 100


alpha <- 0.01
echantillon <- IC207
IC207

alpha <- 0.10
echantillon <- IC428
var(IC428)

# ESTIMATION MU
# SIGMA CONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = sigma, param_estim = "mu")

# SIGMA INCONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = FALSE, param_estim = "mu")
t.test(echantillon, conf.level = 1 - alpha)$conf.int

# ESTIMATION SIGMA²
# ON ESTIME LA VARIANCE, PAS L'ECART TYPE, PENSER A FAIRE LA RACINE CARREE AU BESOIN
# MU CONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = mu, param_estim = "sigma")

# MU INCONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = FALSE, param_estim = "sigma")



# ESTIMATION DE MU CAS NON GAUSSIEN
# SIGMA CONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = sigma, param_estim = "mu", nongo = TRUE)

# SIGMA INCONNU
get_IC(echantillon = echantillon, alpha = alpha, autre_param = FALSE, param_estim = "mu", nongo = TRUE)
