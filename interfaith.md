---
title: "Mixed-Faith Parents and Atheism"
output:
   #pdf_document: default
   rmarkdown::html_document:
    toc: true
    toc_float: true
    keep_md: true
    theme: readable
  # word_document: default
#always_allow_html: true
    
bibliography: BAMLabLib.bib
csl: apa.csl
header-includes:
   #- \usepackage{draftwatermark}
   #- \SetWatermarkText{PREPRINT}
   #- \SetWatermarkLightness{.97}
   - \usepackage{setspace}
   - \usepackage{lineno}
   #- \linenumbers
   - \usepackage{fancyhdr}
   - \pagestyle{fancy}
   - \fancyhead[R]{Atheists & Apostates}
   - \usepackage{footnote}
   - \renewcommand{\thefootnote}{\roman{footnote}}
# indent: true
# toc: true
author: false
   

  
date: "November 2020"
---
<style type="text/css">

body, td {
   font-size: 16px;
   font-family: Garamond;
}
/* Headers */
h1,h2,h3,h4,h5,h6{
  font-family: Garamond;
}
code.r{
  font-size: 12px;
}
pre {
  font-size: 12px
}
</style>



# Big Question

Do kids of mixed-faith parents end up less religious thann kids of same-faith parents?

## Theory

Per a lot of cultural evolutionary theory, it's super plausible that kids of mixed-faith parents might end up less religious. People follow a bunch of different social learning cues [@rendellCognitiveCultureTheoretical2011]. Put them together, and the transmission of religious belief probably requires some crucial cultural inputs [@banerjeeWouldTarzanBelieve2013; @gervaisCulturalTransmissionFaith2011]. Credibility enhancing displays [@henrichEvolutionCostlyDisplays2009] are super important for transmitting belief [@gervaisLearnedFaithInfluences2015; @lanmanReligiousActionsSpeak2017], and a lack of religious CREDs is a primary predictor of atheism and secularism [@willardTestingTheoriesSecularization2017; @gervaisOriginsReligiousDisbeliefinpress].


Mixed faith parents present an interesting and strong test case. Here, in the strongest case, we have kids getting cues from both parents to believe in a God...but they get slightly differenrt cues about which God to believe in. Our cultural evolution-informed account predicts lower religiosity and an uptick of atheism. Other accounts -- for example a strong version of the byproduct or naturalness of religion approach from evolutionary psychology and cognitive science of religion [@barrettWhyWouldAnyone2004; @boyerBeingHumanReligion2008; @mccauleyNaturalnessReligionUnnaturalness2000] -- don't easily make such a prediction.

I found 3 different archival datasets that can test this hypothesis.


# Baylor Religion Survey, Wave I



The Baylor Religion Survey ([LINK HERE](https://thearda.com/Archive/Files/Descriptions/BRS2005.asp)) has measures of individual religiosity, and questions about parental religious affiliation. After wrangling, there were 1494 respondents.

## Measures

### Outcome Measure

I grabbed measures of self-rated belief in God, prayer frequency, and religious attendance to make a religiosity composite. Can also code atheism by people who respond "I don't believe in anything beyond the physical world" when asked their belief about God. 

### Parental Religion

It asks separately about religious affiliation of mom and dad. I just coded whether mom and dad match. The VAST majority of mixed faith parents are examples where, for instance, mom is Catholic and dad is Protestant. This is awesome...it's the strongest possible test of our idea.

### Other covariates

I also grabbed the usual suspects: 

- gender
- age
- income
- education
- politics


## The Models

First off, I did a Bayesian mixed effects model predicting religiosity (composited as a within-participant cluster of the religiosity measures) from:

- the basic covariates
- dummy code for whether or not parents matched religious affiliation
- a count of how many parents are nonreligious

This last bit is nice because I can set that at zero to see model predicted effect of mixed-faith parents when both are religious in some way. BOOM. 







I checked a bunch of different model specifications (fixed and random slopes, with and without interactions), and the best was random intercepts, but fixed effect on religiosity measures. No interaction with number of nonreligious parents. But all the models were really consistent. There was nice robustness to model perturbations.

Code for the linear model...


```r
relScore ~ dnorm(mu, sigma),

mu <- a_measure[measure] + b_diff*parentDiffC +
      b_nones*parentNonesC  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    
c(a, b_diff, b_nones, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
a_measure[measure] ~ dnorm(0,1),
sigma ~ dcauchy(0,1)
```




I did the same robustness monkeying around for a binary model predicting atheism.

Binary model...


```r
atheist ~ dbinom(1, p),
logit(p) <- a + b_diff*parentDiffC + b_nones*parentNonesC  +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    
c(a, b_diff, b_nones,  b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1)
```


## Results




Results were super consistent across models. In the overall sample, kids of mixed-faith parents were reliably less religious on the composite, $\beta$ = -0.11, [-0.17, -0.05], $Pr(\beta < 0 | data)$ > 0.99. On the overall binary model, kids of mixed faith parents were more likely to be atheists, $Odds Ratio$ = 1.25, [0.94, 1.56], $Pr(OR > 1 | data)$ = 0.91.

Finally, as the STRONGEST possible test, I did both of those models, but isolating only the participants for which BOTH parents were Christian. Now it's really just looking at Protestant/Catholic splits. Here again, kids of mixed-faith-but-still-both-Christian parents were reliably less religious on the composite, $\beta$ = -0.19, [-0.25, -0.14], $Pr(\beta < 0 | data)$ > 0.99. On the binary model, it predicts an atheism probability of 0.04, [0.03, 0.04] for kids of same-faith parents, but 0.06, [0.05, 0.08] for kids of mixed-faith Christian parents, $Pr(atheism | mixed > atheism | same)$ = > 0.99. The odds of atheism increase by a factor of 1.65, [1.23, 2.04] for kids of mixed-faith Christian parents. That's pretty nifty...kids get consistent cues to believe in a Christian God. But if the parents are mixed in their brand of Christianity, the odds of hardline atheism go up like 65%.
 
# Faith Matters Survey

 

 
Next up is the 2006 Faith Matters Survey ([LINK HERE](https://thearda.com/Archive/Files/Descriptions/FTHMATT.asp)). After some wrangling and tidying, there are 3108 USAmericans interviewed by phone.

## Measures

### Outcome

Again, using a composite religion outcome of God, prayer, and attendance. The God measure was less conducive to looking at atheism classification.

### Mixed Faith

This one was a bit coarser than Baylor. It just asked "How about your family growing up? Did both your parents have the same religious preference as each other?"

### Covariates

The usual suspects.

- age
- gender
- education
- income

## The Model

Again I tinkered with a bunch of variants and settled on a random intercept model (clustered at participant level for religion measures). It was robust to tweaks though. Here she is:


```r
score ~ dnorm(mu, sigma),
    
mu <- a_measure[measure] + b_diff*parentDiffC   +
      b_fem*femC +
      b_age*ageZ +
      b_educ*educZ +
      b_inc*incomeZ,
    
c(a, b_diff, b_fem, b_age, b_educ, b_inc) ~ dnorm(0,1),
a_measure[measure] ~ dnorm(0,1),
sigma ~ dcauchy(0,1)
```







## Results

As with the Baylor survey, kids of mixed-faith parents were less religious than kids of same-faith parents, $\beta$ = -0.07, [-0.11, -0.03], $Pr(\beta < 0 | data)$ > 0.99.

# International Social Science Survey, 1998






Finally, there's the 1998 ISSP ([LINK HERE](https://thearda.com/Archive/Files/Descriptions/ISSPRG.aspp)). After wrangling, there are 27756 participants from 32 countries. Mostly Western. NOTE: For these analyses I'm only keeping participants where BOTH parents are religious. Nothing is driven by some sneaky heathen parents.

## Measures

### Outcome

A composite religion outcome of God, prayer, and attendance. I averaged them, rather than doing the mixed effects clustering. Because the data was huge and my laptop isn't up to the huge Bayesian 3 level modeling.

### Mixed Faith

Parent religious affiliation had lots of options, [LINK](https://thearda.com/Archive/Files/Search/ISSPRG_AN.asp?File=&Filename=ISSPRG&SearchTerms=parents+mother+father&Type=2). I coded a dummy variable for if they're the same.

### Covariates

The usual suspects.

- age
- gender
- education
- income

Also, for the multilevel model, clustered by Country.

## The Model

Again, I tried a lot of slight variations for robustness. Best option ended up being random intercepts and slopes of mixed-faith parenting by country. I modeled covariates as fixed.

Thar she blows:





```r
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
```

## Results

A lot more to unpack here. But for the overall, once you account for country-level clustering and whatnot, there's an overall pattern whereby kids of mixed-faith (but again **both religious**) parents are less religious than kids of same-faith parents, $\beta$ = -0.13, [-0.2, -0.06], $Pr(\beta < 0 | data)$ > 0.99.

But this difference itself varies a lot across countries! And, perhaps interestingly, the observed negative association between mixed-faith parents and religiosity was magnified in more strongly in more religious countries, as evidenced by a negative slope-intercept association, -0.21, [-0.55, 0.12], $Pr(\beta_{slope/intercept} < 0)$ = 0.84. But that's a bit tenuous and unexpected, and we're only dealing with ~30 countries with restricted range. Nothing to write home about.  

Overall trend is there, but there's also a lot of between-country heterogeneity that is itself related to baseline levels of religiosity. Kids of mixed-faith parents are less religious, especially in countries that are more generally religious. Check out the posterior densities for the $\beta$ by country!

![Across 30 countries, children of interfaith religious parents tend to be less religious than children of same-faith parents. Posterior densities, point estimate (posterior mode), and 95% highest density interval for each country. Height of curve on y-axis represents posterior density, or estimate credibility.](interfaith_files/figure-html/issp beta plot-1.png)

# Overall

Across 2 US samples and one international one, we've got consistent evidence that kids of mixed-faith parents are less religious. This holds even when both parents are religious. heck, in the Baylor US sample, mixed-faith-but-both-Christian parents were more likely to have full-on hardline atheist kids. 

Here's the posterior densities for the overall associaitons in each sample.

Nifty, eh?

![Mixed faith parents predicts reduced religiosity in the Baylor Religion Survey, the Faith Matters Survey, and the International Social Survey Program's 1998 Religion II survey. Posterior densities in the standardized beta, curve height depicts estimate credibility. Bars contain most credible estimate (posterior mode), 66% highest density interval, and 95% highest density interval.](interfaith_files/figure-html/combine posterior plot-1.png)

\pagebreak

# References
