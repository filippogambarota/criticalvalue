
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
#library(criticalvalue)
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
#> t = 0.85477, df = 52.493, p-value = 0.3966
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.2770570  0.6884066
#> sample estimates:
#> mean of x mean of y 
#> 0.4720055 0.2663307 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.2207006 |dc| = 0.5179983 |bc| = 0.4827318 
#> g = 0.2175297 |gc| = 0.5105559

# cor.test
ctest <- cor.test(x, y)
critical(ctest)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 0.2489, df = 28, p-value = 0.8053
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.3186784  0.4004755
#> sample estimates:
#>        cor 
#> 0.04698521 
#> 
#> |== Critical Value ==| 
#> |rc| = 0.3610069

# linear model (unstandardized)
z <- rnorm(30)
q <- rnorm(30)

dat <- data.frame(x, y, z, q)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit)
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.4642 -0.5740  0.1517  0.6684  1.7520 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)  0.27637             0.47319    0.23021   1.201   0.2408  
#> x            0.04114             0.53101    0.25833   0.159   0.8747  
#> q           -0.36313             0.39688    0.19308  -1.881   0.0713 .
#> z            0.05388             0.43445    0.21136   0.255   0.8008  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.06 on 26 degrees of freedom
#> Multiple R-squared:  0.1234, Adjusted R-squared:  0.02223 
#> F-statistic:  1.22 on 3 and 26 DF,  p-value: 0.3223

# linear model (standardized)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit, standardize = TRUE)
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ z.x + z.q + z.z, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.4642 -0.5740  0.1517  0.6684  1.7520 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)  0.26633             0.39791    0.19358   1.376   0.1806  
#> z.x          0.06305             0.81379    0.39590   0.159   0.8747  
#> z.q         -0.74115             0.81004    0.39408  -1.881   0.0713 .
#> z.z          0.10085             0.81322    0.39562   0.255   0.8008  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.06 on 26 degrees of freedom
#> Multiple R-squared:  0.1234, Adjusted R-squared:  0.02223 
#> F-statistic:  1.22 on 3 and 26 DF,  p-value: 0.3223
```
