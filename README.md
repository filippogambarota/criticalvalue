
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalvalue

<!-- badges: start -->
<!-- badges: end -->

The goal of criticalvalue is to …

## Installation

You can install the development version of criticalvalue like so:

``` r
# require(devtools)
devtools::install_github("filippogambarota/criticalvalue")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(criticalvalue)
devtools::load_all()
#> ℹ Loading criticalvalue

# t-test

x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)
critical(ttest)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x and y
#> t = 1.9365, df = 57.688, p-value = 0.05771
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.01941557  1.16849325
#> sample estimates:
#> mean of x mean of y 
#> 0.8212300 0.2466912 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.500004 |dc| = 0.1425943 |bc| = 0.4487234 
#> g = 0.4734645 |gc| = 0.1350256

# cor.test
ctest <- cor.test(x, y)
critical(ctest)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = -0.85668, df = 28, p-value = 0.3989
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.4917713  0.2126989
#> sample estimates:
#>        cor 
#> -0.1598169 
#> 
#> |== Critical Value ==| 
#> |rc| = 0.3610069

# linear model (unstandardized)
z <- rnorm(30)
q <- rnorm(30)

dat <- data.frame(x, y, z, q)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit)
fit
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Coefficients:
#> (Intercept)            x            q            z  
#>     0.38673     -0.21990     -0.47079      0.08601  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>   0.5509830   0.4278069   0.5455706   0.6974748
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.4922 -0.7973 -0.1638  0.6485  2.6393 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)  0.38673             0.55098    0.26805   1.443   0.1610  
#> x           -0.21990             0.42781    0.20812  -1.057   0.3004  
#> q           -0.47079             0.54557    0.26542  -1.774   0.0878 .
#> z            0.08601             0.69747    0.33932   0.253   0.8019  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.168 on 26 degrees of freedom
#> Multiple R-squared:  0.1367, Adjusted R-squared:  0.03707 
#> F-statistic: 1.372 on 3 and 26 DF,  p-value: 0.2733

# linear model (standardized)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit, standardize = TRUE)
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.0932 -0.6696 -0.1376  0.5447  2.2168 
#> 
#> Coefficients:
#>               Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)  3.374e-17           3.683e-01  1.792e-01   0.000   1.0000  
#> x           -2.043e-01           3.974e-01  1.933e-01  -1.057   0.3004  
#> q           -3.544e-01           4.107e-01  1.998e-01  -1.774   0.0878 .
#> z            5.285e-02           4.286e-01  2.085e-01   0.253   0.8019  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9813 on 26 degrees of freedom
#> Multiple R-squared:  0.1367, Adjusted R-squared:  0.03707 
#> F-statistic: 1.372 on 3 and 26 DF,  p-value: 0.2733
```
