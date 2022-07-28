rm(list=ls())
pacman::p_load(rio,
               tidyverse,
               MASS)


ind_var <- 1/2
cv <- 0

Sigma <- matrix(c(ind_var,cv,cv,
                  cv,ind_var,cv,
                  cv,cv,ind_var),3,3)

a <- mvrnorm(n = 1000000,
        c(10,10.5,10),
        Sigma)


print(mean(a[,2] - a[,1]))
print(sd(a[,1] - a[,2]))
print(sd(a[,1]))
print(sd(a[,2]))