
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

## Installation (temporary)

The package is not production-ready thus to test the feature we suggest
to clone or download the repository, open the R Project and load the
functions with `devtools::load_all()`.

## Example from fitted objects

This is a basic example which shows you how to solve a common problem:

### T Test

``` r
# library(criticalvalue) # when the package will be ready
devtools::load_all() # loading all functions for testing
#> ℹ Loading criticalvalue

# t-test (welch)

x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)
critical(ttest)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  x and y
#> t = 3.49, df = 56.4, p-value = 0.00095
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  0.41389 1.52908
#> sample estimates:
#> mean of x mean of y 
#>   0.71569  -0.25579 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.90104 |dc| = 0.51716 |bc| = 0.55759 
#> g = 0.88899 |gc| = 0.51024

# t-test (standard)

ttest <- t.test(x, y, var.equal = TRUE)
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 3.49, df = 58, p-value = 0.00093
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  0.41423 1.52874
#> sample estimates:
#> mean of x mean of y 
#>   0.71569  -0.25579 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.90104 |dc| = 0.51684 |bc| = 0.55725 
#> g = 0.88933 |gc| = 0.51012

# t-test (standard) with monodirectional hyp

ttest <- t.test(x, y, var.equal = TRUE, alternative = "less")
critical(ttest)
#> 
#>  Two Sample t-test
#> 
#> data:  x and y
#> t = 3.49, df = 58, p-value = 1
#> alternative hypothesis: true difference in means is less than 0
#> 95 percent confidence interval:
#>    -Inf 1.4368
#> sample estimates:
#> mean of x mean of y 
#>   0.71569  -0.25579 
#> 
#> |== Effect Size and Critical Value ==| 
#> d = 0.90104 dc = -0.43159 bc = -0.46534 
#> g = 0.88933 gc = -0.42598

# within the t-test object saved from critical we have all the new values

ttest <- critical(ttest)
str(ttest)
#> List of 15
#>  $ statistic  : Named num 3.49
#>   ..- attr(*, "names")= chr "t"
#>  $ parameter  : Named num 58
#>   ..- attr(*, "names")= chr "df"
#>  $ p.value    : num 1
#>  $ conf.int   : num [1:2] -Inf 1.44
#>   ..- attr(*, "conf.level")= num 0.95
#>  $ estimate   : Named num [1:2] 0.716 -0.256
#>   ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "difference in means"
#>  $ stderr     : num 0.278
#>  $ alternative: chr "less"
#>  $ method     : chr " Two Sample t-test"
#>  $ data.name  : chr "x and y"
#>  $ g          : Named num 0.889
#>   ..- attr(*, "names")= chr "1"
#>  $ gc         : Named num 0.426
#>   ..- attr(*, "names")= chr "1"
#>  $ d          : num 0.901
#>  $ bc         : num 0.465
#>  $ dc         : num 0.432
#>  - attr(*, "class")= chr [1:3] "critvalue" "ttest" "htest"
```

We can check the results using:

``` r
ttest <- t.test(x, y)
ttest <- critical(ttest)

t <- ttest$bc/ttest$stderr # critical numerator / standard error

# should be 0.05 (or alpha)
(1 - pt(ttest$bc/ttest$stderr, ttest$parameter)) * 2
#> [1] 0.05
```

### Correlation Test

``` r
# cor.test
ctest <- cor.test(x, y)
critical(ctest)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  x and y
#> t = 1.17, df = 28, p-value = 0.25
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.15717  0.53413
#> sample estimates:
#>     cor 
#> 0.21529 
#> 
#> |== Critical Value ==| 
#> |rc| = 0.36101
```

### Linear Model

``` r
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
#>     -0.4797       0.2453      -0.0678      -0.2090  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.56408     0.46368     0.55556     0.52586
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = dat)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -1.969 -0.837  0.003  0.566  2.585 
#> 
#> Coefficients:
#>             Estimate |Critical Estimate| Std. Error t value Pr(>|t|)  
#> (Intercept)  -0.4797              0.5641     0.2744   -1.75    0.092 .
#> x             0.2453              0.4637     0.2256    1.09    0.287  
#> q            -0.0678              0.5556     0.2703   -0.25    0.804  
#> z            -0.2090              0.5259     0.2558   -0.82    0.421  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.19 on 26 degrees of freedom
#> Multiple R-squared:  0.0705, Adjusted R-squared:  -0.0368 
#> F-statistic: 0.657 on 3 and 26 DF,  p-value: 0.586

# linear model (standardized)
fit <- lm(y ~ x + q + z, data = dat)
fit <- critical(fit, standardize = TRUE)
fit
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Coefficients:
#>             (Intercept)                        x                        q  
#>  0.00000000000000000154   0.20677680028750763941  -0.04864933653239084110  
#>                       z  
#> -0.15829141095774629244  
#> 
#> 
#> Critical |Coefficients| 
#> 
#> (Intercept)           x           q           z 
#>     0.38212     0.39082     0.39840     0.39826
summary(fit)
#> 
#> Call:
#> lm(formula = y ~ x + q + z, data = D)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.6887 -0.7182  0.0025  0.4854  2.2169 
#> 
#> Coefficients:
#>                            Estimate     |Critical Estimate|
#> (Intercept)  0.00000000000000000154  0.38212473830528237428
#> x            0.20677680028750763941  0.39082305475924328242
#> q           -0.04864933653239084110  0.39840443267220376766
#> z           -0.15829141095774629244  0.39826425115168551105
#>                          Std. Error t value Pr(>|t|)
#> (Intercept)  0.18590088330604184752    0.00     1.00
#> x            0.19013255048163035799    1.09     0.29
#> q            0.19382083524682736098   -0.25     0.80
#> z            0.19375263796495784119   -0.82     0.42
#> 
#> Residual standard error: 1.02 on 26 degrees of freedom
#> Multiple R-squared:  0.0705, Adjusted R-squared:  -0.0368 
#> F-statistic: 0.657 on 3 and 26 DF,  p-value: 0.586
```

## Example from summary statistics

``` r
x <- rnorm(30, 0.5, 1)
y <- rnorm(30, 0, 1)

ttest <- t.test(x, y)

m1 <- mean(x)
m2 <- mean(y)
sd1 <- sd(x)
sd2 <- sd(y)
n1 <- n2 <- 30

critical_t2s(m1, m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2)
#> $d
#> [1] 0.06976
#> 
#> $dc
#> [1] 0.51695
#> 
#> $bc
#> [1] 0.49719
#> 
#> $df
#> [1] 57.434
#> 
#> $g
#> [1] 0.068844
#> 
#> $gc
#> [1] 0.51016

critical_t2s(t = ttest$statistic, se = ttest$stderr, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!
#> $d
#>       t 
#> 0.06976 
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] 0.49708
#> 
#> $df
#> [1] 58
#> 
#> $g
#>        t 
#> 0.068853 
#> 
#> $gc
#> [1] 0.51012

critical_t2s(t = ttest$statistic, n1 = n1, n2 = n2)
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!
#> Warning in crit_from_t_t2s(t, n1, n2, se, conf.level, hypothesis, var.equal):
#> When se = NULL bc cannot be computed, returning NA!
#> $d
#>       t 
#> 0.06976 
#> 
#> $dc
#> [1] 0.51684
#> 
#> $bc
#> [1] NA
#> 
#> $df
#> [1] 58
#> 
#> $g
#>        t 
#> 0.068853 
#> 
#> $gc
#> [1] 0.51012
```
