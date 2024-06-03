devtools::load_all()

# require(devtools)
devtools::install_github("filippogambarota/criticalvalue")

library(psych)
library(psychTools)

data("holzinger.swineford")
Holz <- holzinger.swineford

## Correlation

cc<-cor.test(Holz$t01_visperc, Holz$t02_cubes)
critical(cc)
critical.htest(cc)

## T-test

tt <- t.test(Holz$t01_visperc [ Holz$female == 1], Holz$t01_visperc [ Holz$female == 2])
critical(tt)
critical.htest(tt)

## Linear regression

ll <- lm( t01_visperc ~ ageyr , data = Holz)
summary(critical(ll))

## T-test da n

n <- 20

critical_t2s(n1 = n , n2 = n , hypothesis = "less")

## Paired t-test

critical_t2sp(n = n , hypothesis = "less")

## Critical correlation

critical_cor(n = n,hypothesis = "less", test = "z")


## Critical Coefficient

critical_coef(seb = .1, n = 20, p = .01, conf.level = .95, hypothesis = "less")












