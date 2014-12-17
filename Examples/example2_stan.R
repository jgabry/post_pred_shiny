# In this example we use we the rstan package (the R interface to Stan) to
# sample from the posterior distribution in a simple Bayesian linear regression
# model. Then we launch the shiny app. If you don't want to use Stan check out
# the other example. 

library(rstan) # see mc-stan.org if you need to install the rstan package
library(shiny)


# Create fake data --------------------------------------------------------

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
s <- 3 # pick an sd
y <- rnorm(N, mean =  m, sd = s) # generate y values



# Run model using Stan -------------------------------------------------------

# the data we'll need
stan_data <- list(y = y, X = X, N = N, K = K)

# the Stan code
stan_model <- "
data {
  # declare the data we're providing to Stan 
  int           N ; # integer, number of observations
  int           K ; # integer, number of columns in model matrix
  matrix[N,K]   X ; # NxK matrix, model matrix
  vector[N]     y ; # N-vector, outcome variable
}
parameters {
  # declare the parameters in our model 
  real<lower=0> sigma ; # real number > 0, standard deviation
  vector[K]     beta ;  # K-vector of real numbers, regression coefficients
}
model {
  # write our model 
  beta ~ normal(0, 10) ;      # prior for betas
  sigma ~ cauchy(0, 5) ;      # prior for sigma
  y ~ normal(X*beta, sigma) ; # likelihood (note: Stan knows X*beta is matrix times vector multiplication)
}
"

# run it
posterior <- stan(model_code = stan_model, data = stan_data, chains = 4)
print(posterior)

# extract the posterior samples
samples <- extract(posterior)
beta <- samples$beta
sigma <- samples$sigma


# Launch shiny app for posterior predictive check -------------------------

# to use the app we need y, X, beta and sigma, all of which we have created
# above
shiny::runGitHub(repo = "jgabry/post_pred_shiny")

