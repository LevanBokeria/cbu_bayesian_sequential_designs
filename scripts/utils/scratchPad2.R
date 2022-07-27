rm(list=ls())
pacman::p_load(rio,
               tidyverse,
               MASS)


Sigma <- matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),3,3)

a <- mvrnorm(n = 100,
        c(10,10.5,10),
        Sigma)


print(mean(a[,2] - a[,1]))
print(sd(a[,1] - a[,2]))


print(mean(a[,2] - a[,3]))
print(sd(a[,2] - a[,3]))
