############################################################
# R-project                                                #
# Program      : a3-ORSimVisualize.r                       #
# Protocol     :                                           #
# Date         :                                           #
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



d1 <- read.csv(file = paste0(base_dir, "/data/a3-ORSimulation-1.csv"), sep = ",", header = TRUE)
d2 <- read.csv(file = paste0(base_dir, "/data/a3-ORSimulation-2.csv"), sep = ",", header = TRUE)
d3 <- read.csv(file = paste0(base_dir, "/data/a3-ORSimulation-3.csv"), sep = ",", header = TRUE)

d_all <- rbind(d1, d2, d3)


library(dplyr)
d_mh <- d_all %>% 
  select(k, phi, rho, mh, mh_low, mh_upp) %>% 
  mutate(item = "mh", est = mh, low = mh_low, upp = mh_upp) %>% 
  select(item, k, phi, rho, est, low, upp) %>% 
  ungroup() 

d_cmle <- d_all %>% 
  select(k, phi, rho, cmle, cmle_low, cmle_upp) %>% 
  mutate(item = "cmle", est = cmle, low = cmle_low, upp = cmle_upp) %>% 
  select(item, k, phi, rho, est, low, upp) %>% 
  ungroup() 

d_mle <- d_all %>% 
  select(k, phi, rho, mle, mle_low, mle_upp) %>% 
  mutate(item = "mle", est = mle, low = mle_low, upp = mle_upp) %>% 
  select(item, k, phi, rho, est, low, upp) %>% 
  ungroup() 

d_clog <- d_all %>% 
  select(k, phi, rho, clog, clog_low, clog_upp) %>% 
  mutate(item = "clog", est = clog, low = clog_low, upp = clog_upp) %>% 
  select(item, k, phi, rho, est, low, upp) %>% 
  ungroup() 


d_ana <- rbind(d_mh, d_cmle, d_mle)
d_ana$rho <- paste0("rho = ", d_ana$rho)

#----- save simulation result 
write.table(x = d_ana, file = paste0(base_dir, "/data/a3-ORSimResult.csv"), 
            sep = ",", row.names = FALSE, col.names = TRUE)


#----- phi = 2 
library(ggplot2)
p <- ggplot(data = subset(d_ana, est < 20 & phi == 2), aes(x = k, y = est, colour = item))
p <- p + theme_bw()
p <- p + facet_wrap( ~ rho, ncol = 4)
p <- p + geom_line(size = 1.5, position = position_dodge(width = 0.5))
p <- p + geom_hline(yintercept = 2.0, linetype = "dashed", color = "red", size = 1.0)
p <- p + scale_x_continuous(limits = c(20, 100), breaks = seq(from = 20, to = 100, by = 20))
p <- p + scale_y_continuous(limits = c(0, 7), breaks = seq(from = 0, to = 7, by = 2))
p <- p + xlab("Number of stratum")
p <- p + ylab("Estimate of phi")
p <- p + theme(legend.position = c(0.05, 0.98), legend.justification = c(0.05, 0.98))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.text = element_text(size = 12, colour = "black"))
p <- p + theme(strip.text = element_text(size = 12, colour = "black"))
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a3-SimPhi2.png"), dpi = 400, plot = p, w = 8, h = 4) 



#----- phi = 5 
library(ggplot2)
p <- ggplot(data = subset(d_ana, est < 40 & phi == 5), aes(x = k, y = est, colour = item))
p <- p + theme_bw()
p <- p + facet_wrap( ~ rho, ncol = 4)
p <- p + geom_line(size = 1.5, position = position_dodge(width = 0.5))
p <- p + geom_hline(yintercept = 5.0, linetype = "dashed", color = "red", size = 1.0)
# p <- p + scale_y_log10() 
p <- p + scale_x_continuous(limits = c(20, 100), breaks = seq(from = 20, to = 100, by = 20))
p <- p + scale_y_continuous(limits = c(0, 40), breaks = seq(from = 0, to = 40, by = 5))
p <- p + xlab("Number of stratum")
p <- p + ylab("Estimate of phi")
p <- p + theme(legend.position = c(0.05, 0.98), legend.justification = c(0.05, 0.98))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.text = element_text(size = 12, colour = "black"))
p <- p + theme(strip.text = element_text(size = 12, colour = "black"))
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a3-SimPhi5.png"), dpi = 400, plot = p, w = 8, h = 4) 



#----- phi = 10 
library(ggplot2)
p <- ggplot(data = subset(d_ana, item %in% c("mh", "cmle") & phi == 10), aes(x = k, y = est, colour = item))
p <- p + theme_bw()
p <- p + facet_wrap( ~ rho, ncol = 4)
p <- p + geom_line(size = 1.5, position = position_dodge(width = 0.5))
p <- p + geom_hline(yintercept = 10.0, linetype = "dashed", color = "red", size = 1.0)
# p <- p + scale_y_log10() 
p <- p + scale_x_continuous(limits = c(20, 100), breaks = seq(from = 20, to = 100, by = 20))
# p <- p + scale_y_continuous(limits = c(0, 40), breaks = seq(from = 0, to = 40, by = 5))
p <- p + xlab("Number of stratum")
p <- p + ylab("Estimate of phi")
p <- p + theme(legend.position = c(0.05, 0.98), legend.justification = c(0.05, 0.98))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.text = element_text(size = 12, colour = "black"))
p <- p + theme(strip.text = element_text(size = 12, colour = "black"))
p <- p + theme(axis.title = element_text(size = 12, colour = "black"))
p <- p + theme(axis.text = element_text(size = 12, colour = "black"))
ggsave(file = paste0(base_dir, "/output/a3-SimPhi10.png"), dpi = 400, plot = p, w = 8, h = 4) 



