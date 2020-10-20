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

issp_f <- read.csv("issp-98.csv")
summary(issp_f)


issp <- issp_f %>%
  select(momRel = MOMREL, # 90+ are NA
         dadRel = DADREL, # 90+ are NA
         momAttend = MOMATTND, # 1-never 9-lots, 98-98 NA
         dadAttend = DADATTND, # 1-never 9-lots, 98-98 NA
         god = BELFGOD1, # 1- don't, 6- sure, 8-DK, 9 NA
         pray = PRAY, # 1-nver to 11-lots, 98-DN, 99-NA
         attend = ATTEND, # 1-weekly+, 6-never, 8-DK, 9=NA
         age = AGE,
         gender = I_SEX, #1-m, 2-F,
         income = INCOME,
         educ = DEGREE, #99 NA,
         country = COUNTRY
         ) %>%
  mutate(momRel = na_if(momRel, 98),
         momRel = na_if(momRel, 99),
         dadRel = na_if(dadRel, 98),
         dadRel = na_if(dadRel, 99),
         momAttend = na_if(momAttend, 98),
         momAttend = na_if(momAttend, 99),
         dadAttend = na_if(dadAttend, 98),
         dadAttend = na_if(dadAttend, 99),
         god = na_if(god, 8),
         god = na_if(god, 9),
         atheist = ifelse(god == 1, 1, 0),
         pray = na_if(pray, 98),
         pray = na_if(pray, 99),
         attend = na_if(attend, 8),
         attend = na_if(attend, 0),
         attend = 7-attend,
         female = ifelse(gender == 2, 1, 0),
         educ = na_if(educ, 99),
         godZ = scale(god)[,],
         prayZ = scale(pray)[,],
         attendZ = scale(attend)[,],
         femC = scale(female, center = T, scale = F)[,],
         educZ = scale(educ)[,],
         incomeZ = scale(income)[,],
         ageZ = scale(age)[,],
         parentSame = ifelse(momRel == dadRel, 1, 0),
         parentDiff = ifelse(parentSame == 0, 1, 0),
         parentDiffC = scale(parentDiff, scale=F, center=T)[,],
         country = factor(country),
         religiosity = (godZ + prayZ + attendZ)/3,
         religZ = scale(religiosity)[,],
         momNone = ifelse(momRel == 90, 1, 0),
         dadNone = ifelse(dadRel == 90, 1, 0),
         parentsNone = momNone + dadNone,
         momAttendZ = scale(momAttend)[,],
         dadAttendZ = scale(dadAttend)[,],
         parAttend = momAttend + dadAttend,
         parAttendZ = scale(parAttend)[,]) %>%
  drop_na() %>%
  data.frame
summary(issp)
nrow(issp)

relPars <- issp %>% 
  filter(parentsNone == 0) %>%
  data.frame

summary(relPars)

# analyze across countries, rand slopes

issp.lin.ri <- map2stan(
  alist(
    religZ ~ dnorm(mu, sigma),
    mu <- a_country[country] + b_diff*parentDiffC   +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    a_country[country] ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ), data=relPars, iter=4000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.9)
)


plot(issp.lin.ri)
precis(issp.lin.ri)


# random slopes and intercepts  ----
issp.rsri <- map2stan(
  alist(
    religZ ~ dnorm(mu, sigma),
    mu <- a_country[country] + b_diff_country[country]*parentDiffC + 
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    c(a_country, b_diff_country)[country] ~ dmvnorm2(c(a, b_diff),sigma_country,Rho_country),
    sigma_country ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1),
    Rho_country ~ dlkjcorr(2)
  ), data=relPars, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.9)
)

plot(issp.rsri)
precis(issp.rsri, depth=2)




# whole thing with interactions
issp.int.rsri <- map2stan(
  alist(
    religZ ~ dnorm(mu, sigma),
    mu <- a_country[country] + b_diff_country[country]*parentDiffC + 
      b_parA_country[country]*parAttendZ + b_int_country[country]*parentDiffC*parAttendZ +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    c(a, b_diff, b_parA, b_int, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
    c(a_country, b_diff_country, b_parA_country, b_int_country)[country] ~ dmvnorm2(c(a, b_diff, b_parA, b_int),sigma_country,Rho_country),
    sigma_country ~ dcauchy(0,1),
    sigma ~ dcauchy(0,1),
    Rho_country ~ dlkjcorr(2)
  ), data=relPars, iter=14000, 
  warmup=2000, WAIC = T, chains=2, cores = 2,
  control=list(adapt_delta=0.9)
)

plot(issp.int.rsri)
precis(issp.int.rsri, depth=2)