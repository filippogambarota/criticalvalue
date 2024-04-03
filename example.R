# devtools::load_all()
# library(criticalvalue)

# two sample t.test (var.equal = TRUE)

x <- rnorm(100, 0.5, 1)
y <- rnorm(100, 0, 1)

tt <- t.test(x, y)
fit <- lm(y ~ x)

critical(fit, standardize = TRUE) |> 
  summary()




