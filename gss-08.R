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

gss08 <- read.csv("gss-08.csv")
summary(gss08)

gss <- gss08 %>%
  select(momRel = MARELIG, # 0- inapplic, 1- prot,  2- cath, 3- j, 4-none, 5-oth, 8-dk, 9-no ans
         dadRel = PARELIG, # same
         god = GOD, # 1-don't b, 2- don't know, 3- nonpesonal...6-know, 8-dk, 9-na 
         attend = ATTEND, #0- never...8 lots, 9-NA
         pray = PRAY, #1- several daily...6-never, 8- DK, 9-NA
         educ = EDUC, #0-20, 98-DK, 99-NA
         income = INCOME, #1...12 (25k?),13-refuse, 98-DK
         gender = I_GENDER,
         age = AGE) %>%
  mutate(educ = na_if(educ, 98),
         educ = na_if(educ, 99),
         atheist = ifelse(god == 1, 1, 0),
         income = na_if(income, 13),
         income = na_if(income, 98),
         female = ifelse(gender == 2, 1, 0),
         god = na_if(god, 6),
         god = na_if(god, 8),
         god = na_if(god, 9),
         momRel = na_if(momRel, 8),
         momRel = na_if(momRel, 9),
         momRel = na_if(momRel, 0),
         dadRel = na_if(dadRel, 8),
         dadRel = na_if(dadRel, 9),
         dadRel = na_if(dadRel, 0),
         attend = na_if(attend, 9),
         pray = na_if(pray, 8),
         pray = na_if(pray, 9),
         parentDiff = ifelse(momRel == dadRel, 0, 1),
         momNone = ifelse(momRel == 4, 1, 0),
         dadNone = ifelse(dadRel == 4, 1, 0),
         noneN = momNone + dadNone,
         godZ = scale(god)[,],
         prayZ = scale(pray)[,],
         attendZ = scale(attend)[,],
         parentDiffC = scale(parentDiff, scale = F, center=T)[,],
         femC = scale(female, scale = F, center=T)[,],
         ageZ = scale(age)[,],
         educZ = scale(educ)[,],
         incomeZ = scale(income)[,])

gss.l <- gss %>%
  select(godZ:incomeZ, noneN) %>%
  pivot_longer(godZ:attendZ, names_to = "measure", values_to = "score") %>%
  mutate(measure = factor(measure)) %>%
  drop_na() %>%
  data.frame

summary(gss.l)


# FUCK. almost no data for parental religion. drop thisone.


