# In this example we sample directly from the posterior distribution of beta and
# sigma^2 in a simple Bayesian linear regression model. Then we launch the shiny
# app.

# if you don't have shiny installed do: 
# install.packages("shiny")


library(shiny)

# create fake data --------------------------------------------------------

# number of observations 
N <- 500

# covariates
X1 <- rnorm(N)
X2 <- rpois(N, 2)
X3 <- rbinom(N, 1, 1/2)

# model matrix (with column of 1s for intercept)
X <- cbind(const = 1, X1, X2, X3)
K <- ncol(X)

# generate fake outcome y
b <- c(2, -1/4, 1/2, 5/2) # pick some coefficients
m <- X %*% b # calculate mean
s <- 2 # pick an sd
y <- rnorm(N, mean =  m, sd = s) # generate y values

# run OLS and extract estimates
OLS <- lm(y ~ X1 + X2 + X3)
beta_hat <- coef(OLS) # coefficient estimates
sigma2_hat  <- sum( (y - X %*% beta_hat)^2 ) / (N - K) # variance estimate


# sample from posterior distribution under vague prior ------------------
v1 <- N - K
v2 <- v1*sigma2_hat
At <- t(chol(solve(t(X)%*%X))) # note: %*% = matrix multiplication, t() = transpose, solve() = inverse, chol() = cholesky factorization 

posterior_sample <- function(beta_hat, v1, v2, At) { 
  # draw sigma2 from inv-gamma distribution 
  sigma2 <- 1 / rgamma(1, shape =  v1/2, rate = v2/2)
  
  # draw beta from MVN distribution given value of sigma2 
  z <- rnorm(length(beta_hat))
  beta <- beta_hat + sqrt(sigma2)*(At %*% z)
  
  # return beta and sigma2
  c(beta, sigma2)
}
# take 1000 draws from posterior
posterior <- t(replicate(n = 1e3, posterior_sample(beta_hat, v1, v2, At)))

colnames(posterior) <- c(names(beta_hat), "sigma2")
beta <- posterior[, names(beta_hat)]
sigma <- sqrt(posterior[, "sigma2"])



# Launch shiny app for posterior predictive check -------------------------

# to use the app we need y, X, beta and sigma, all of which we have created
# above
shiny::runGitHub(repo = "jgabry/post_pred_shiny")
