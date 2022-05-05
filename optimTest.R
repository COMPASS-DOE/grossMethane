
# Test example to make sure using optim() correctly
test <- tibble(x = 0:10, y = x + runif(11, -1, 1) * 2)

tst_opt <- optim(par = c("b" = -1, "m" = 0.5),
                 fn = function(p, test) {
                   pred <- test$x * p["m"] + p["b"]
                   sum((pred - test$y) ^ 2)
                 },
                 gr = NULL,
                 test)
m <- lm(y ~ x, data = test)
message("Test data actual fit:", paste(m$coefficients, collapse = ", "))
message("Test data optim found: ", paste(tst_opt$par, collapse = ", "))