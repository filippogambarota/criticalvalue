
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
#> t = 0.722, df = 56.8, p-value = 0.47
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -0.31007  0.65968
#> sample estimates:
#> mean of x mean of y 
#>   0.49722   0.32242 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.18642 |dc| = 0.14281 |bc| = 0.36673 
#> g = 0.17636 |gc| = 0.1351

# cor.test
ctest <- cor.test(x, y)
critical(ctest)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 1.09, df = 28, p-value = 0.29
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.17095  0.52393
#> sample estimates:
#>     cor 
#> 0.20174 
#> 
#> |== Critical Value ==| 
#> |rc| = 0.36101

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
#>      0.2101       0.2346       0.0511       0.0408  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.45111     0.46959     0.44287     0.40206
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -2.126 -0.653  0.316  0.726  1.754 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)
#> (Intercept)   0.2101              0.4511     0.2195    0.96     0.35
#> x             0.2346              0.4696     0.2285    1.03     0.31
#> q             0.0511              0.4429     0.2155    0.24     0.81
#> z             0.0408              0.4021     0.1956    0.21     0.84
#> 
#> Residual standard error: 1.04 on 26 degrees of freedom
#> Multiple R-squared:  0.0477, Adjusted R-squared:  -0.0622 
#> F-statistic: 0.434 on 3 and 26 DF,  p-value: 0.73

# linear model (standardized)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit, standardize = TRUE)
summary(fit, digits = 5)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -2.116 -0.650  0.314  0.722  1.746 
#> 
#> Coefficients:
#>                          Estimate   |Critical Estimate|            Std. Error
#> (Intercept) -0.000000000000000173  0.386779862544724795  0.188165567115443216
#> x            0.202124205896398623  0.404628061505859005  0.196848585040459267
#> q            0.052045063843277305  0.451298916263581429  0.219553613672175685
#> z            0.045866154056785180  0.451720605985108326  0.219758762629714000
#>             t value Pr(>|t|)
#> (Intercept)    0.00     1.00
#> x              1.03     0.31
#> q              0.24     0.81
#> z              0.21     0.84
#> 
#> Residual standard error: 1.03 on 26 degrees of freedom
#> Multiple R-squared:  0.0477, Adjusted R-squared:  -0.0622 
#> F-statistic: 0.434 on 3 and 26 DF,  p-value: 0.73
```
