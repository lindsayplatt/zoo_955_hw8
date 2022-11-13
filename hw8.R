
##### Q1 #####

# Simulate some data for a linear regression using the simple linear 
# regression model: yi = beta_0 + beta_1*xi + error, where error~N(0,sigma).  

# Make sigma large enough that it resembles “typical” ecological data when you 
# plot it, but not so large that it completely obscures the relationship between 
# x and y. Record your true beta_0, beta_1, and sigma here:

# Generate some random values for the independent variable
?rnorm

sigma <- 12
set.seed(19)
error <- rnorm(50, mean = 0, sd = sigma)
xi <- 1:50

# Set "true" slope and intercept
beta_0 <- 4
beta_1 <- 1.3

# Generate dependent values using the "true" linear regression
yi <- beta_0 + beta_1*xi + error

plot(xi, yi)

# Fit a linear regression model using lm() and do your typical checks of 
# model assumptions that we went over in class. Paste your estimated model 
# coefficients and their 95% confidence intervals below:

mod_lm <- lm(yi ~ xi)
coefficients(mod_lm)
confint(mod_lm)

newvals <- seq(min(xi),  max(xi), length.out=100)
confint_vals <- predict(mod_lm, interval = 'confidence', 
                        newdata = data.frame(xi=newvals))

plot(xi, yi, cex = 1.75, pch = 21, bg = 'gray')
abline(mod_lm, col = 'black', lwd = 2)
lines(newvals, confint_vals[, 'lwr'], col = 'blue', lty = 2, lwd = 2)
lines(newvals, confint_vals[, 'upr'], col = 'blue', lty = 2, lwd = 2)

# Are the coefficient estimates close to the true values? Do the 95% 
# confidence intervals cover the true values?

# Coefficients are close, but the 95% confidence interval does not include
# all of the values.

##### Q2 #####

# Analyze the data generated in Q1 using the normal equation (see Word doc)
# Paste your estimated model coefficients below.
# Are the coefficient estimates close to the true values?  

n_obs <- length(xi)
xmat <- matrix(c(rep(1, n_obs), xi), nrow = n_obs)
ymat <- matrix(yi)

# Solve for beta_0 and beta_1 using matrix multiplication
analytical_coef <- solve(t(xmat)%*%xmat) %*% (t(xmat)%*%ymat)

analytical_coef

# Yes, they are pretty close!
  
##### Bonus Q1 #####

# Can you find an analytical solution for the se and 95% CI for the model 
# coefficients? (note: this is not in the lectures or reading, you'll have 
# to do some searching).  Write the equations and resulting answers below.
# Do the 95% confidence intervals cover the true values?
  
##### Q3 #####

# Analyze the data generated in Q1 using a grid search to minimize the sum of 
# squared errors (no need to iterate more than twice).
# Paste your estimated model coefficients below.

calc_sse <- function(x, y, betas) {
  yfit <- betas[1] + betas[2]*x
  sse <- sum((yfit - y)^2)
  return(sse)
}

# Create grid space
sse_grid_vals <- expand.grid(intercept = seq(0,8,by=0.5), slope = seq(0,3,by=0.25))
sse_grid_out  <- apply(sse_grid_vals, 1, calc_sse, x=xi, y=yi) # Calculate SSE for each combo
sse_grid_vals[which.min(sse_grid_out),] # Extract minimum

# Do that all once more but with finer precision
sse_grid_vals2 <- expand.grid(intercept = seq(4,7,by=0.10), slope = seq(1,1.5,by=0.05))
sse_grid_out2 <- apply(sse_grid_vals2, 1, calc_sse, x=xi, y=yi) # Calculate SSE for each combo
sse_grid_vals2[which.min(sse_grid_out2),] # Extract minimum

##### Q4 #####

# Analyze the data generated in Q1 using a grid search to minimize the negative 
# log likelihood (no need to iterate more than twice).  Note, there is a third 
# parameter that you will need to estimate here: sigma
# Paste your estimated model coefficients below.

calc_negll <- function(data, params) {
  beta0 <- params[1]
  beta1 <- params[2]
  sigma <- params[3]
  x <- data$x
  y <- data$y
  yfit <- beta0 + beta1*x
  -sum(dnorm(x=y, mean=yfit, sd=sigma, log=T))
}

# Create grid space
negll_grid_vals <- expand.grid(intercept = seq(0,8,by=0.5), slope = seq(0,3,by=0.25),
                               sigma = seq(5,15,by=1))
negll_grid_out <- apply(negll_grid_vals, 1, calc_negll, data=data.frame(x=xi, y=yi)) # Calculate -LogLikelihood for each combo
negll_grid_vals[which.min(negll_grid_out),] # Extract minimum

# Try once more with more precision
negll_grid_vals2 <- expand.grid(intercept = seq(6,7,by=0.05), slope = seq(1,1.5,by=0.05),
                                sigma = seq(10,15,by=0.5))
negll_grid_out2 <- apply(negll_grid_vals2, 1, calc_negll, data=data.frame(x=xi, y=yi)) # Calculate -LogLikelihood for each combo
negll_grid_vals2[which.min(negll_grid_out2),] # Extract minimum

# The intercept is at the low end, which means you should try once more because 
# the minimum could be below that!
negll_grid_vals3 <- expand.grid(intercept = seq(3,5,by=0.5), slope = seq(1,1.5,by=0.05),
                                sigma = seq(10,15,by=0.5))
negll_grid_out3 <- apply(negll_grid_vals3, 1, calc_negll, data=data.frame(x=xi, y=yi)) # Calculate -LogLikelihood for each combo
negll_grid_vals3[which.min(negll_grid_out3),] # Extract minimum

# Now finding intercept of 4 - much closer.

##### Q5 #####

# Analyze the data generated in Q1 using optim() to minimize the negative log 
# likelihood.  Note, there is a third parameter that you will need to estimate 
# here: sigma. Paste your estimated model coefficients below.

negll_optim_out <- optim(par = c(2, 0, 8), calc_negll, data = data.frame(x=xi, y=yi))
negll_optim_out

# Did the numerical optimization algorithm converge?  How do you know?

# I know because the value under the `convergence` item in the output of 
# `optim()` was 0, which indicates a successful completion.

# Did the numerical optimization algorithm find a global solution?  How do you know?

# Yes because all of the values are above my initial parameter inputs and 
# the optimization converged, so it successfully tried parameters above those
# initalizations, too.

##### Q6 #####

# Plot a likelihood profile for the slope parameter while estimating the 
# conditional MLEs of the intercept and sigma for each plotted value of the 
# slope parameter (see p. 173 of Hilborn and Mangel).

# Pick specific intercept & sigma values to consider from the previous effort, 
# but vary the slope:
min_beta_0 <- negll_grid_vals3[which.min(negll_grid_out3), 'intercept']
min_sigma <- negll_grid_vals3[which.min(negll_grid_out3), 'sigma']
slope_options <- seq(0,3,by=0.25)

negll_grid_vals_slope <- expand.grid(intercept = min_beta_0, slope = slope_options,
                                     sigma = min_sigma)
negll_grid_out_slope <- apply(negll_grid_vals_slope, 1, calc_negll, data=data.frame(x=xi, y=yi)) 

plot(slope_options, negll_grid_out_slope, type = 'l',
     ylab = 'Negative log likelihood', xlab = 'Values for slope',
     main = sprintf('Using intercept=%s and sigma=%s', 
                    round(min_beta_0, digits = 0),
                    round(min_sigma, digits = 2)))

##### Q7 #####

# Plot the joint likelihood surface for the intercept and slope parameters.  
# Is there evidence of confounding between these two parameters (i.e., a ridge 
# rather than a mountain top)?

# Vary intercept & slope but not sigma (use same sigma & slope options from Q6):
intercept_options <- seq(2,7,by=0.5)

negll_grid_vals_intslope <- expand.grid(intercept = intercept_options, slope = slope_options,
                                     sigma = min_sigma)
negll_grid_out_intslope <- apply(negll_grid_vals_intslope, 1, calc_negll, data=data.frame(x=xi, y=yi)) 
negll_grid_df_intslope <- cbind(negll_grid_vals_intslope, negll = negll_grid_out_intslope)

library(ggplot2)
ggplot(negll_grid_df_intslope, aes(x = slope, y = intercept, z = negll)) + 
  geom_contour_filled() + 
  ggtitle(sprintf('Using sigma=%s', round(min_sigma, digits = 2))) + 
  ylab('Values for intercept') + xlab('Values for slope')

# I have a ridge instead of a mountaintop (between slopes of 1-2, there is a
# minimum log likelihood for intercepts from 2-7).

##### Q8 #####

# How different are the estimated coefficients from Q1, Q2, and Q5 and how 
# do they compare to the true values?

# Q1 result
q1_int <- coefficients(mod_lm)[1]
q1_slope <- coefficients(mod_lm)[2]

# Q2 result
q2_int <- analytical_coef[1,1]
q2_slope <- analytical_coef[2,1]

# Q5 result
q5_int <- negll_optim_out$par[1]
q5_slope <- negll_optim_out$par[2]

data.frame(
  question = paste0('Q', c(1,2,5)),
  estim_intercept = c(q1_int, q2_int, q5_int),
  estim_slope = c(q1_slope, q2_slope, q5_slope)
)

# The estimates from Q1 and Q2 were closer than the estimates for Q3, but all
# are very close.

##### Bonus Q2 #####

# Calculate the standard errors of the intercept, slope, and sigma using the 
# Hessian matrix. Standard errors are the square roots of the diagonal of the 
# inverse Hessian matrix.  How do these standard errors compare to those from 
# lm() in Q1?


##### Bonus Q3 #####

# How does the computational speed compare between using lm(), the normal 
# equation, and optim() to estimate the coefficients? Note, you can get the 
# computation time placing the following code around your regression code:

# Record your system start time
start_time <- proc.time()

#your code goes here

# Subtract start time from current system time
proc.time() - start_time
