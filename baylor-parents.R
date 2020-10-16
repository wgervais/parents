# libraries ====

library(tidyverse)
library(rethinking)
library(tidybayes)
library(haven)

# functions ====
dig <- 2


bigger <- function(first, second, digits) {
  pr.big <- ifelse(first > second, 1, 0) %>% mean %>% round(digits = dig)
  pr.big[pr.big == 1] <- '> 0.99'
  pr.big[pr.big == 0] <- '< 0.01'
  return(pr.big)
}


biggerZero <- function(input, digits) {
  pr.big <- ifelse(input > 0, 1, 0) %>% mean %>% round(digits = dig)
  return(pr.big)
}



# a couple of quick functions for pulling HPDIs


low <- function(x){
  v <- HPDI(x, prob=.89)[1]
  return(v)
}


high <- function(x){
  v <- HPDI(x, prob=.89)[2]
  return(v)
}


printB <- function(input) {
  p <- input %>% mean %>% round(digits = dig)
  l <- input %>% low %>% round(digits = dig)
  h <- input %>% high %>% round(digits = dig)
  paste0(p, ", [", l, ", ", h, "]")
}


col1 <- "darkviolet"
col2 <- "darkturquoise"
col3 <- "darkslategrey"
col4 <- "deeppink"
col5 <- "#fed503" 
col6 <- "#0065a5"
# col7 <- "orange3"

text <- "black"
border <- "transparent"
grids <- "lightgrey"
backgrounds <- "transparent"
fontfam <- "Times"
effect <- "black"
angle <- 360-29
hjust <- 0
vjust <- 1

themeME <-  theme_bw() +
  theme(text = element_text(family=fontfam),
        #legend.position = "none",
        axis.text = element_text(size = 16, color = text),
        axis.title = element_text(size = 20, color = text),
        axis.text.x = element_text(angle= angle, vjust=vjust, hjust=hjust, color = text),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color=grids),
        panel.background = element_rect(fill = backgrounds),
        plot.background = element_rect(color = border, fill = backgrounds))  


# data ====

full <- read.csv("baylor-w1.csv")
summary(full)

parent <- full %>%
  select(
    fatherRel = Q31A,
    motherRel = Q31B,
    god = Q21,
    gender = Q51,
    rel12 = Q32,
    attend12 = Q33,
    region = REGION,
    income = I_INCOME,
    educ = I_EDUC,
    age = I_AGE,
    politics = I_POLITICS,
    attend = Q5,
    pray = Q16) %>%
  drop_na() %>%
  mutate(fatherNone = ifelse(fatherRel == 7, 1, 0),
    motherNone = ifelse(motherRel == 7, 1, 0),
    parentDiff = ifelse(fatherRel == motherRel, 0, 1),
    female = ifelse(gender == 2, 1, 0),
    atheist = ifelse(god == 5, 1, 0),
    nonbeliever = ifelse(god == 5 | god == 6, 1, 0 ),
    god_no_op = ifelse(god == 6, 1, 0),
    parentNones = fatherNone + motherNone,
    incomeZ = scale(income)[,],
    educZ = scale(educ)[,],
    ageZ = scale(age)[,],
    rel12Z = scale(rel12)[,],
    attend12Z = scale(attend12)[,],
    femC = scale(female, scale=F, center=T)[,],
    believer = ifelse(god != 5 & god !=6, 1, 0),
    parentDiffC = scale(parentDiff, center=T, scale=F)[,],
    parentNonesC = scale(parentNones, center=T, scale=F)[,],
    motherNoneC = scale(motherNone, center=T, scale=F)[,],
    fatherNoneC = scale(fatherNone, center=T, scale=F)[,],
    god2 = factor(god),
    god3 = recode(god2, "5" = "6", "6" = "5"),
    god3 = as.numeric(god3),
    godBelief = 6-god3,
    godZ = scale(godBelief)[,],
    attendZ = scale(attend)[,],
    prayZ = scale(pray)[,]
  ) %>%
  data.frame

summary(parent)
nrow(parent)


# parents different rel ====

# relig items, mixed model ====

parent.l <- parent %>%
  pivot_longer(godZ:prayZ, names_to = "measure", values_to = "relScore") %>%
  mutate(measure = factor(measure)) %>%
  data.frame

summary(parent.l)



lin.m <- map2stan(
  alist(
    relScore ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff_measure[measure]*parentDiffC + b_nones*parentNonesC + b_dn_int*parentDiffC*parentNonesC +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_nones, b_dn_int, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    c(a_measure,b_diff_measure)[measure] ~ dmvnorm2(c(a, b_diff),sigma_measure,Rho_measure),
    sigma_measure ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1),
    Rho_measure ~ dlkjcorr(4)
  ), data=parent.l, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.97)
)

plot(lin.m)
precis(lin.m, depth=2)

lin.p <- data.frame(extract.samples(lin.m))






lin.m2 <- map2stan(
  alist(
    relScore ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff*parentDiffC + b_nones*parentNonesC + b_dn_int*parentDiffC*parentNonesC +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_nones, b_dn_int, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    a_measure[measure] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data=parent.l, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(lin.m2)
precis(lin.m2, depth=2)


compare(lin.m, lin.m2) # model 1 a bit better but not a big diff

diff.p <- data.frame(extract.samples(lin.m))

diff.p <- diff.p %>%
  mutate(belParents = b_diff + b_dn_int * min(parent.l$parentNonesC))
  
precis(diff.p) # reliable effect of parents with different religion, even when both are believers

table(parent$fatherRel, parent$motherRel) # most common differences are a protestant and a catholic


# without interaction ====
lin.m2.ni <- map2stan(
  alist(
    relScore ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff*parentDiffC + b_nones*parentNonesC  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_nones, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    a_measure[measure] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data=parent.l, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(lin.m2.ni)
precis(lin.m2.ni, depth=2)

compare(lin.m2.ni, lin.m2, lin.m) # drop the interaction


diff.p.ni <- data.frame(extract.samples(lin.m2.ni))

diff.p.ni 

precis(diff.p.ni) # reliable effect of parents with different religion, even when both are believers
biggerZero(diff.p.ni$b_diff)
table(parent$fatherRel, parent$motherRel) 


# different rel, atheist ID ====

diff.bin <- map2stan(
  alist(
    atheist ~ dbinom(1, p),
    logit(p) <- a + b_diff*parentDiffC + b_nones*parentNonesC + b_dn_int*parentDiffC*parentNonesC +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_nones, b_dn_int, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1)
  ), data=parent.l, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(diff.bin)
precis(diff.bin)



diff.bin.ni <- map2stan(
  alist(
    atheist ~ dbinom(1, p),
    logit(p) <- a + b_diff*parentDiffC + b_nones*parentNonesC  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_nones,  b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1)
  ), data=parent.l, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(diff.bin.ni)
precis(diff.bin.ni)

compare(diff.bin, diff.bin.ni) # better off without interaction

bin.ni <- data.frame(extract.samples(diff.bin.ni)) %>%
  mutate(sames = (a + b_diff*min(parent.l$parentDiffC)) %>% logistic(),
         diffs = (a + b_diff*max(parent.l$parentDiffC)) %>% logistic())

biggerZero(bin.ni$b_diff)
precis(bin.ni)
# parents ====

# binary
# checked models with and without interaction, better without

rentals.bin <- map2stan(
  alist(
    atheist ~ dbinom(1, p),
    logit(p) <- a + b_mom*motherNone + b_dad*fatherNone + b_intx*motherNone*fatherNone  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_mom, b_dad, b_intx, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1)
  ), data=parent, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(rentals.bin)
precis(rentals.bin)


# drop interaction

rentals.bin2 <- map2stan(
  alist(
    atheist ~ dbinom(1, p),
    logit(p) <- a + b_mom*motherNone + b_dad*fatherNone +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_mom, b_dad, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1)
  ), data=parent, iter=14000, 
  warmup=2000, WAIC = F, chains=1, cores = 1,
  control=list(adapt_delta=0.85)
)

compare(rentals.bin, rentals.bin2) # better without interaction

precis(rentals.bin2)

rent.p <- data.frame(extract.samples(rentals.bin2))

rent.p <- rent.p %>%
  mutate(neither = (a %>% logistic),
         mom = (a + b_mom) %>% logistic,
         dad = (a + b_dad) %>% logistic,
         both = (a + b_dad + b_mom) %>% logistic)

precis(rent.p)





# isolate to Christians, look for protestant/catholic dip ====

parentl.lx <- parent.l %>%
  filter(fatherRel == 1 | fatherRel ==2,
         motherRel == 1 | motherRel == 2)

summary(parentl.lx)


xian.m <- map2stan(
  alist(
    relScore ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff*parentDiffC +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    a_measure[measure] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data=parent.l, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(xian.m)
precis(xian.m)

