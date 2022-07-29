rm(list=ls())

library(MASS)
library(tidyverse)

n_sim <- 10000

cv <- 0 # covariances
ind_var <- 1 # variances, diagonal entries


m1 <- list()
m2 <- list()

sd1 <- list()
sd2 <- list()

for (iSim in seq(n_sim)){
        
        
        if (iSim %% 500 == 0){
                
                print(iSim)
        }
        
        all_data <- mvrnorm(n = 100,
                            mu = c(0.9,
                                   0,
                                   0.5),
                            Sigma = matrix(c(ind_var,cv,cv,
                                              cv,ind_var,cv,
                                              cv,cv,ind_var),3,3),
                            empirical = F)
        
        m1[iSim] <- mean(all_data[,1] - all_data[,2])
        m2[iSim] <- mean(all_data[,3] - all_data[,2])
        
        sd1[iSim] <- sd(all_data[,1] - all_data[,2])
        sd2[iSim] <- sd(all_data[,3] - all_data[,2])
}

res1 <- data.frame(unlist(m1),unlist(sd1),unlist(m2),unlist(sd2))
names(res1) <- c('m1','sd1','m2','sd2')

# Get the effect size
res1 <- res1 %>%
        mutate(d1 = m1/sd1,
               d2 = m2/sd2)


# Plot them
res1 %>%
        ggplot(aes(x = '1',y=d1)) +
        geom_violin() +
        geom_boxplot(width = 0.2)

res1 %>%
        ggplot(aes(x = '1',y=d2)) +
        geom_violin() +
        geom_boxplot(width = 0.2)

# Check correlation between the effect sizes
cor(res1$d1,res1$d2)
