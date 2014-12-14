library(rstan)

set.seed(2357911)

# number of observations 
N <- 250

# covariates
X1 <- rnorm(N)
X2 <- rpois(N, 2)
X3 <- rbinom(N, 1, 1/2)

# model matrix (with column of 1s for intercept)
X <- cbind(const = 1, X1, X2, X3)

# generate fake data y
b <- c(2, -1/4, 1/2, 5/2) # pick some coefficients
m <- X %*% b # calculate mean
s <- 3 # pick an sd
y <- rnorm(N, mean =  m, sd = s)

# run model in stan
stan_data <- list(y = y, X = X, N = N, K = ncol(X))
fit <- stan(file = "regression.stan", data = stan_data, chains = 4, iter = 200)



# y = observed data
# theta = parameters we want to estimate
# y_rep = simulated/replicated data using values of theta drawn from posterior
# y_hat = fitted values

posterior <- extract(fit)
beta <- posterior$beta
sigma <- posterior$sigma


nIter <- length(sigma)

# fitted values
y_hat <- sapply(1:nIter, function(i) X%*%beta[i,])
y_hat_avg <- rowMeans(y_hat)
plot(y, y_hat_avg, xlab = "observed y", ylab = "avg. fitted y")


# matrix of 100 simulated y values for each of the nIter posterior samples
y_rep <- sapply(1:nIter, function(i) rnorm(100, X%*%beta[i, ], sigma[i]))
y_rep_avg <- rowMeans(y_rep)

# plot density for a random sample of 20 of the replicated data sets along
# with that of the original data (in color)
y_rep_20 <- y_rep[sample(nrow(y_rep), 20) ,]
ymax <- max(c(density(y_rep_20)$y, density(y)$y))
plot(density(y_rep_20[1,]), ylim = c(0, ymax+0.05))
for (i in 2:20) lines(density(y_rep_20[i,]))
lines(density(y), col = "purple", lwd = 3)


# Avg residual vs avg fitted value 
resid_avg <- rowMeans(y_hat - y)
resid_rep_avg <- rowMeans(y_rep - y)
plot(y_hat_avg, resid_avg)
plot(y_rep_avg, resid_rep_avg)
