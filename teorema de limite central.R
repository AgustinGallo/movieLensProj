#First Part of assesmente 5 multyple choices and penalization
options(digits = 3)
p_correct=1/5
p_incorrect=1-p_correct
n <- 44
ev <- 1*p_correct + (-.25*p_incorrect)
se <- (sqrt(n))*abs(1 - -.25)*sqrt(p_correct*p_incorrect)
p_h8<-1-pnorm(8, ev, se)

set.seed(21, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  sum(sample(c(1, -.25),44, replace = TRUE, prob = c(p_correct, p_incorrect)))
})
mean(S>8)

#Second Part of assesmente 5 multyple choices and penalization
pScore35 <- function(p){
  p_incorrect=1-p
  ev <- n*(1*p + 0*p_incorrect)
  se <- (sqrt(n))*abs(1 - 0)*sqrt(p*p_incorrect)
  1-pnorm(35, ev, se)
}

p_correct=1/4
p_incorrect=1-p_correct
n <- 44
ev <- n*(1*p_correct + 0*p_incorrect)
se <- (sqrt(n))*abs(1 - 0)*sqrt(p_correct*p_incorrect)
p_h30<-1-pnorm(30, ev, se)

p <- seq(0.25, 0.95, 0.05)
Scores<-sapply(p, pScore35)

#Roulette Special
p_correct=5/38
p_incorrect=1-p_correct
n <- 500
ev <- n*(6*p_correct + -1*p_incorrect)
se <- (sqrt(n))*abs(6 - -1)*sqrt(p_correct*p_incorrect)
ev_1 <- (6*p_correct + -1*p_incorrect)
se_1 <- abs(6 - -1)*sqrt(p_correct*p_incorrect)
se_mean <- abs(6 - -1)*sqrt(p_correct*p_incorrect)/sqrt(n)
p_m0<-1-pnorm(0, ev, se)

#Final Assesment
options(digits = 3)
library(tidyverse)
library(dslabs)
p_death_fem_50<-(death_prob %>% filter(age==50 & sex=="Female") %>% select(prob))
p <- p_death_fem_50[1,1]
loss_death <- -150000
profit <- 1150
n=1000
ev_1 <- p*loss_death+profit*(1-p)
se_1 <- abs(loss_death-profit)*sqrt(p*(1-p))
ev_sum<-n*ev_1
se_sum<-sqrt(n)*se_1
pnorm(0, ev_sum, se_sum)

p_death_male_50<-(death_prob %>% filter(age==50 & sex=="Male") %>% select(prob))
p <- p_death_male_50[1,1]
n <- 1000
g <- (700000 - n*p*(-150000))/(n*(1-p))
se_sum <- sqrt(n)*abs(g- -150000)*sqrt(p*(1-p))
pnorm(0,700000,se_sum)

p_death_male_50<-(death_prob %>% filter(age==50 & sex=="Male") %>% select(prob))
p_death_fem_50<-(death_prob %>% filter(age==50 & sex=="Female") %>% select(prob))
p_f <- p_death_fem_50[1,1]
p_m <- p_death_male_50[1,1]
loss_death <- -150000
profit <- 1150
n=1000
p <- .015
ev_1 <- p*loss_death+profit*(1-p)
ev_sum <- n*ev_1
se_1 <- abs(loss_death-profit)*sqrt(p*(1-p))
se_sum <- sqrt(n)*se_1

p<- seq(0.01, 0.03, 0.0025)
p_loss <- function(p){
  ev_1 <- p*loss_death+profit*(1-p)
  ev_sum <- n*ev_1
  se_1 <- abs(loss_death-profit)*sqrt(p*(1-p))
  se_sum <- sqrt(n)*se_1
  pnorm(-1000000,ev_sum, se_sum)
}
id<-sapply(p,p_loss)
p[id>.9]

set.seed(25, sample.kind = "Rounding")
p <- .015
loss_death <- -150000
profit <- 1150
Results <- sample(c(profit,loss_death), n, replace = TRUE, prob = c(1-p,p))
sum(Results)/1e6

set.seed(27, sample.kind = "Rounding")
B <- 10000
Results <- replicate(B, {
  preview <- sample(c(profit,loss_death), n, replace = TRUE, prob = c(1-p,p))
  sum(preview)
})
mean(Results < -1000000)

z <- qnorm(0.05)
l <- -150000
p <- 0.015
n <- 1000
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
ev_1 <- l*p + x*(1-p)
ev_sum <- n*ev_1
se_1 <- abs(l-x)*sqrt(p*(1-p))
set.seed(29, sample.kind = "Rounding")
B <- 10000
Results <- replicate(B, {
  prob<-p+sample(seq(-0.01, 0.01, length = 100), 1)
  draw <- sample(c(x,l), n, replace = TRUE, prob = c(1-prob,prob))
  sum(draw)
})
mean(Results)
mean(Results < -1000000)
