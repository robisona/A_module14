## A_Module14.R
## Assignment for Group A Module 14
## Andrew Robison, Connor Breton, Korik Moreno


## The data for this week include mean February temperature (in °C, Feb_temp), mean snowfall (in cm, Feb_snow) 
## and the day of year that plants leaf out (in days, leaf_DOY) around Durham, NH.  The goal of this analysis
## is to determine whether high February temperature and low February snowfall accelerate leaf out.  Remember
## that leaf out is a day of year, so lower values indicate earlier leaf out.

setwd("C:/Users/Connor/Documents/word_files/graduate_courses/r/module_14/A_module14")


## (1)	Read in the "Bayes_assn_day13.csv" file and check the data.  Check for collinearity in the predictors. 

# Read in data, and do a brief inspection
Pheno = read.table("Bayes_pheno.csv", sep=",", header = T)
head(Pheno)
summary(Pheno) # no missing data

# Check for collinearity
plot(Pheno$Feb_temp, Pheno$leaf_DOY) # Negative correlation here betwen leaf_DOY and Feb_temp. 
plot(Pheno$Feb_snow, Pheno$leaf_DOY) # No obvious correlation.
plot(Pheno$Feb_temp, Pheno$Feb_snow) # No obvious correlation.



########################################################################################################################
## (2)	Set up and run a three chain linear regression model for JAGS that includes the leafing date as the 
## response, additive predictors as appropriate based on the results from question 1, and non-informative priors.
## Don't forget the burn-in!  

# Load the rjags package and write the JAGS model
library(rjags)

model_string <- "model{

# Likelihood
  for (i in 1:Ntotal){
    y[i] ~ dnorm(mu[i], inv.var)
    mu[i] <- beta[1] + beta[2]*x1[i] + beta[3]*x2[i]
  }

# Priors for beta, one for each beta parameter
  for(j in 1:3){
    beta[j] ~ dnorm(0, 0.0001)
  }

# Prior for inverse variance
  inv.var ~ dgamma(0.01, 0.01)
  
}"

# Data setup
list = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_temp, x2 = Pheno$Feb_snow, Ntotal = length(Pheno$leaf_DOY)) 
# List with response variable, predictor variables, and number of samples


# Get model suited with 3 chains
model <- jags.model(textConnection(model_string), data = list, n.chains = 3)


# Update model with burn-in iterations (10000)
update(model, 10000)


# Sample the posterior with 20000 iterations
samp <- coda.samples(model, variable.names = c("beta"), n.iter = 20000) 



########################################################################################################################
## (3)	Check the convergence and autocorrelation of your chains post burn-in.  Briefly (1-2 sentences) describe 
## your interpretation of each diagnostic. 

# Plot summary parameters
plot(samp)
# The three chains run horizontally across each "Trace of beta[x]" plot and sufficiently mix across the length/number of iterations.
# As a note, however, the three chains are far better mixed for beta[2] than for beta[1] or beta[3]. 
# Each "Density of beta[x]" plot approximates a normal distribution and has a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp)
# For each chain, beta[1] and beta[3] appear to have some issues with autocorrelation, but beta[2] looks fine since autocorrelation 
# rapidly decreases from one to zero. Thinning will have to be done to reduce the autocorrelation seen in beta[1] and beta[3].


# Gelman-Brooks-Rubin plot
gelman.plot(samp)
# Each plot is ideal in that the two lines (97.5% and median) converge and rapidly decrease/remain at a value of one.



########################################################################################################################
## (4)	Try to solve any problems that you found in question 3.  Briefly describe what you tried and whether it
## solved the issue(s).  


# Re-compile model with 3 chains
model2 <- jags.model(textConnection(model_string), 
                     data = list, n.chains = 3)


# Update model with burn-in iterations (40000)
update(model2, 10000)


# Sample the posterior and add thinning
samp2 <- coda.samples(model2, variable.names = c("beta"), n.iter = 800000, thin = 80)


# Plot summary parameters
plot(samp2)
# The three chains run horizontally across each "Trace of beta[x]" plot and sufficiently mix across the length/number of iterations.
# Each "Density of beta[x]" plot approximates a normal distribution and has a relatively smooth curve.


# Plot autocorrelation
autocorr.plot(samp2)
# All three "betas" for each chain look fine since autocorrelation rapidly decreases from one to zero and stays that way. 
# Thinning was a great success here since and really helped reduce the autocorrelation seen in beta[1] and beta[3].


# Gelman-Brooks-Rubin plot
gelman.plot(samp2)
# Each plot is ideal in that the two lines (97.5% and median) converge and rapidly decrease/remain at a value of one.


# To reduce autocorrelation, we added a thinning section when running model2. With a thinning value of 80, therefore keeping 1 
# out of every 80 iterations, we were able to sufficiently reduce autocorrelation for all beta values in each chain of model2. 
# Since we thinned, however, we increased our total number of iterations from 20000 in our original model to 800000 in model2. 
# Though our total number of iterations decreased from 20000 in our original model to only 10000 (a.k.a. 800000/80) in model2, 
# all model diagnostics proved our model as adequate. 



########################################################################################################################
## (5)	Interpret your most well-behaved model from question 4.  Do high February temperature and/or low February
## snowfall accelerate leaf out?  By how much (including units)?  

summary(samp2)
# beta[1] - A median of 85.46 with a 95% credible interval of 79.54 to 91.37
# beta[2] - A median of -0.71 with a 95% credible interval of -0.88 to -0.54
# beta[3] - A median of 0.05 with a 95% credible interval of -0.17 to 0.28

# For a one degree C increase in mean February temperature, you would expect leaf out to occur 0.71 days quicker. "Significant" result
# since 95% credible interval does not overlap zero.


# Snowfall does not appear to influence leaf out since the 95% credible interval overlaps zero (a.k.a. the variable is "non-significant").
# Had snowfall been a significant variable we could interpret the results as leaf out taking 0.05 days longer for every centimeter
# increase in mean February snowfall.


# The resulting model formula for the Leaf Out Day of Year = 86.8 - (0.71 * mean Feb. temperature)



########################################################################################################################
## In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file to 
## submit your assignment in myCourses, one per group.

# Group members individually worked on assignment code but then worked together to compile the final version. Drew
# provided a solid base code, then Korik and Connor adjusted models and added comments/interpetations.




