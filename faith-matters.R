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


fms_full <- read_spss("faith-matters-2006.SAV")
summary(fms_full)


fms <- fms_full %>%
  select(parentSame = RELIGFM, # 0 no 1 yes 3 missing 8-9 NA
         bothParentRel = RLTRDFM1,
         dadRel = RLTRDFM2,
         momRel = RLTRDFM3,
         age = AGE,
         income = INCOME, # 1 least 9 most
         educ = EDUC, # 1 least 8 most 98-99 NA
         ethnicity = ETHNIC6,
         gender = GENDER, # 1 male 2 female
         god    = BELGOD, # 0 don't believe, 4 sure 8-9 missing
         pray = PRAY, # 1 never, 6 most, 8-9 NA
         attend = RELATEND # 1 lots, 9 never, 98-99 missing
  ) %>%
  mutate(parentSame = na_if(parentSame, 3),
         parentSame = na_if(parentSame, 8),
         parentSame = na_if(parentSame, 9),
         educ = na_if(educ, 98),
         educ = na_if(educ, 99),
         god = na_if(god, 8),
         god = na_if(god, 9),
         pray = na_if(pray, 8),
         pray = na_if(pray, 9),
         attend = na_if(attend, 98),
         attend = na_if(attend, 99),
         female = ifelse(gender == 2, 1, 0),
         attend = 10-attend,
         godZ = scale(god)[,],
         prayZ = scale(pray)[,],
         attendZ = scale(attend)[,],
         femC = scale(female, center = T, scale = F)[,],
         educZ = scale(educ)[,],
         incomeZ = scale(income)[,],
         ageZ = scale(age)[,],
         ethnicity = factor(ethnicity),
         parentDiff = ifelse(parentSame == 0, 1, 0),
         parentDiffC = scale(parentDiff, scale=F, center=T)[,]
         
         )
         
         
fms.l <- fms %>%
  select(godZ:ageZ,
         parentDiff,
         parentDiffC
         ) %>%
  drop_na() %>%
  pivot_longer(godZ:attendZ, names_to = "measure", values_to = "score") %>%
  mutate(measure = factor(measure)) %>%
  data.frame

summary(fms.l)



# mixed model ====
# random intercept

fms.m <- map2stan(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff*parentDiffC   +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    a_measure[measure] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data=fms.l, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.85)
)

plot(fms.m)
precis(fms.m)



# random slope

fms.mr <- map2stan(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- a_measure[measure] + b_diff_measure[measure]*parentDiffC  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    c(a_measure,b_diff_measure)[measure] ~ dmvnorm2(c(a, b_diff),sigma_measure,Rho_measure),
    sigma_measure ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1),
    Rho_measure ~ dlkjcorr(2)
  ), data=fms.l, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.97)
)

plot(fms.mr)
precis(fms.mr, depth=2)

compare(fms.m, fms.mr) # similar, probably good enough to run the random intercept. too mjuch wonky in the chains


fms.mr.p <- data.frame(extract.samples(fms.mr))
