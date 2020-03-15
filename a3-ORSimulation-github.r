############################################################
# R-project                                                #
# Program      : a3-ORSimulation.r                         #
# Protocol     :                                           #
# Date         : 2020-03-08                                #
# Last         :                                           #
# Programmer   : yoshifumi ukyo                            #
#                                                          #
############################################################
# [Ver.0000]                                               #
# Memorandom   :                                           #
#                                                          #
############################################################


#----- clean-up working directory 
rm(list = ls(all = TRUE))
#----- library assignment 
base_dir <- ""
setwd(base_dir)
set.seed(4989)


library(VGAM)
library(reshape2)
library(survival)


sparseOR <- function(k, phi, n = 5, m = 5, rho){ 
  
  p2_ini <- 0.2
  nIter <- 1000
  d_sim <- data.frame(nIter = 1:nIter, mh = 0, cmle = 0, mle = 0, clog = 0)
  
  for (j in 1:nrow(d_sim)){ 
    
    tab <- array(dim = c(2, 2, k))
    p2 <- p2_ini 
    
    for (s in 1:k){
      p2 <- p2 + 0.6/k
      p1 <- (p2 * phi) / (1 - p2 + p2 * phi) 
      n11 <- rbetabinom(n = 1, size = n, prob = p2, rho = rho)
      m11 <- rbetabinom(n = 1, size = m, prob = p1, rho = rho)
      tab[2, 1, s] <- n11 
      tab[1, 1, s] <- m11 
      tab[2, 2, s] <- n - n11 
      tab[1, 2, s] <- m - m11 
    }
    
    dat1 <- melt(tab) 
    dat1 <- data.frame(x = rep(dat1$Var1 - 1, dat1$value), 
                       y = rep(dat1$Var2 - 1, dat1$value), 
                       k = rep(dat1$Var3, dat1$value))
    
    mh <- mantelhaen.test(tab)
    cmle <- mantelhaen.test(tab, exact = TRUE)
    mle  <- glm(formula = y ~ factor(x) + factor(k), 
                family  = binomial(link = "logit"), 
                data = dat1)
    clog <- clogit(formula = y ~ factor(x) + strata(k), 
                data = dat1)
    
    d_sim$mh[j]   <- mh$estimate 
    d_sim$cmle[j] <- cmle$estimate
    d_sim$mle[j]  <- exp(mle$coefficients[2])
    d_sim$clog[j] <- exp(clog$coefficients)
    
  }
  
  return(d_sim)
}


sim_summary <- expand.grid(k = seq(from = 25, to = 100, by = 5), 
                   phi = c(2, 5, 10), 
                   rho = c(0.0, 0.2, 0.5, 0.8))

sim_summary <- data.frame(sim_summary, 
                          mh = 0, mh_low = 0, mh_upp = 0, mh_inf = 0, 
                          cmle = 0, cmle_low = 0, cmle_upp = 0, cmle_inf = 0, 
                          mle = 0, mle_low = 0, mle_upp = 0, mle_inf = 0, 
                          clog = 0, clog_low = 0, clog_upp = 0, clog_inf = 0)


for (i in 1:nrow(sim_summary)){ 
  
  dat <- sparseOR(k = sim_summary$k[i], phi = sim_summary$phi[i], 
                  n = 5, m = 5, rho = sim_summary$rho[i]) 
  #----- mh 
  mh_est <- subset(dat, mh != Inf)
  sim_summary$mh[i]     <- mean(mh_est$mh) 
  sim_summary$mh_low[i] <- quantile(x = mh_est$mh, probs = 0.025)
  sim_summary$mh_upp[i] <- quantile(x = mh_est$mh, probs = 0.975)
  sim_summary$mh_inf[i] <- nrow(subset(dat, mh == Inf)) / nrow(dat) 
  #----- cmle 
  cmle_est <- subset(dat, cmle != Inf)
  sim_summary$cmle[i]     <- mean(cmle_est$cmle)
  sim_summary$cmle_low[i] <- quantile(x = cmle_est$cmle, probs = 0.025)
  sim_summary$cmle_upp[i] <- quantile(x = cmle_est$cmle, probs = 0.975)
  sim_summary$cmle_inf[i] <- nrow(subset(dat, cmle == Inf)) / nrow(dat) 
  #----- mle
  mle_est <- subset(dat, mle != Inf)
  sim_summary$mle[i]     <- mean(mle_est$mle)
  sim_summary$mle_low[i] <- quantile(x = mle_est$mle, probs = 0.025)
  sim_summary$mle_upp[i] <- quantile(x = mle_est$mle, probs = 0.975)
  sim_summary$mle_inf[i] <- nrow(subset(dat, mle == Inf)) / nrow(dat)
  #----- clog
  clog_est <- subset(dat, clog != Inf)
  sim_summary$clog[i]     <- mean(clog_est$clog)
  sim_summary$clog_low[i] <- quantile(x = clog_est$clog, probs = 0.025)
  sim_summary$clog_upp[i] <- quantile(x = clog_est$clog, probs = 0.975)
  sim_summary$clog_inf[i] <- nrow(subset(dat, clog == Inf)) / nrow(dat)
  
}


#----- save simulation 
write.table(x = sim_summary, file = paste0(base_dir, "/data/a3-ORSimulation.csv"), 
            sep = ",", row.names = FALSE, col.names = TRUE)



