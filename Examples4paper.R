devtools::load_all()

# require(devtools)
devtools::install_github("filippogambarota/criticalvalue")

library(psych)
library(psychTools)

data("holzinger.swineford")
Holz <- holzinger.swineford

tt <- t.test(Holz$t01_visperc [ Holz$female == 1], Holz$t01_visperc [ Holz$female == 2])
critical.htest(tt)



