############################################################
# R-project                                                #
# Program      : a1-betabinom.r                            #
# Protocol     :                                           #
# Date         : <2020/02/25 (Tue) 16:58>                  #
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


############################################################
# beta-binomial                                            #
############################################################

rho <- c(0.05, 0.25, 0.5, 0.75, 0.95)

for (i in 1:length(rho)){ 
  library(VGAM)
  d_sim <- rbetabinom(n = 10000, size = 100, prob = 0.2, rho = rho[i])
  d_sim <- data.frame(rho = rho[i], aval = d_sim / 100)
  d_sim$mean <- mean(d_sim$aval)
  if (i == 1) {d_add <- d_sim}
  if (i > 1) {d_add <- rbind(d_add, d_sim)}
}

d_add <- data.frame(rho = paste0("rho=", d_add$rho), aval = d_add$aval, mean = d_add$mean)

library(ggplot2)
p <- ggplot(data = d_add, aes(x = aval))
p <- p + theme_bw() 
p <- p + facet_wrap( ~ rho, ncol = 5)
p <- p + geom_histogram(binwidth = 0.05, colour = "black") 
p <- p + geom_vline(data = d_add, aes(xintercept = mean, linetype = "dashed", colour = "red"))
p <- p + xlab("prob by beta-binomial (p=0.2)")
p <- p + ylab("frequency")
p <- p + theme(legend.position = "none")
p <- p + scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0.0, 0.5, 1.0))
p <- p + scale_y_continuous(limits = c(0, 8000), breaks = c(0, 2000, 4000, 6000))
p <- p + theme(strip.text = element_text(size = 12, colour = "black"))
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a1-Prob-Betabinom.png"), plot = p, dpi = 400, w = 6, h = 3)


############################################################
# beta distribution                                        #
############################################################

prob <- 0.2
rho  <- c(0.05, 0.25, 0.5, 0.75, 0.95) 

for (i in 1:length(rho)){ 
  alpha <- prob * (1 - rho[i])/rho[i]
  beta  <- (1 - prob) * (1 - rho[i])/rho[i]
  d_sim <- rbeta(n = 10000, shape1 = alpha, shape2 = beta)
  d_sim <- data.frame(rho = rho[i], alpha = alpha, beta = beta, aval = d_sim)
  d_sim$mean <- mean(d_sim$aval)
  if (i == 1) {d_add <- d_sim}
  if (i > 1) {d_add <- rbind(d_add, d_sim)}
}

d_add$cItem <- paste0("rho=", d_add$rho, " ,a=", round(d_add$alpha, digits = 1), " ,b=", round(d_add$beta, digits = 1))


library(ggplot2)
p <- ggplot(data = d_add, aes(x = aval))
p <- p + theme_bw() 
p <- p + facet_wrap( ~ cItem, ncol = 5)
p <- p + geom_histogram(binwidth = 0.05)
p <- p + geom_vline(data = d_add, aes(xintercept = mean, linetype = "dashed", colour = "red"))
p <- p + xlab("prob by beta distribution (p=0.2)")
p <- p + ylab("frequency")
p <- p + theme(legend.position = "none")
p <- p + scale_x_continuous(limits = c(-0.1, 1.1), breaks = c(0.0, 0.5, 1.0))
p <- p + scale_y_continuous(limits = c(0, 8000), breaks = c(0, 2000, 4000, 6000))
p <- p + theme(strip.text = element_text(size = 8, colour = "black"))
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a1-Prob-beta.png"), plot = p, dpi = 400, w = 6, h = 3)


