## A_Module14.R
## Assignment for Group A Module 14
## Andrew Robison, Connor Breton, Korik Moreno


## The data for this week include mean February temperature (in ⁰C, Feb_temp), mean snowfall (in cm, Feb_snow) 
## and the day of year that plants leaf out (in days, leaf_DOY) around Durham, NH.  The goal of this analysis
## is to determine whether high February temperature and low February snowfall accelerate leaf out.  Remember
## that leaf out is a day of year, so lower values indicate earlier leaf out.


## (1)	Read in the “Bayes_assn_day13.csv” file and check the data.  Check for collinearity in the predictors. 

# Read in data, and do a brief inspection
Pheno = read.table("Bayes_pheno.csv", sep=",", header = T)
head(Pheno)
summary(Pheno) # no missing data

# Check for collinearity
plot(Pheno$leaf_DOY, Pheno$Feb_temp) # Some correlation here, betwen leaf_DOY and Feb_temp
plot(Pheno$leaf_DOY, Pheno$Feb_snow) # No obvious correlation
plot(Pheno$Feb_snow, Pheno$Feb_temp) # No obvious correlation



## (2)	Set up and run a three chain linear regression model for JAGS that includes the leafing date as the 
## response, additive predictors as appropriate based on the results from question 1, and non-informative priors.
## Don’t forget the burn-in!  

# Load the rjags package and write the JAGS model
library(rjags)
model_string <- "model{

# Likelihood
  for (i in 1:Ntotal){
    y[i] ~ dnorm(mu[i], inv.var)
    mu[i] <- beta[1] + beta[2]*x1[i]
  }

# Priors for beta, one for each beta parameter
  for(j in 1:2){
    beta[j] ~ dnorm(0, 0.0001)
  }

# Prior for inverse variance
  inv.var ~ dgamma(0.01, 0.01)
  
}"

# Data setup
dataList = list(y = Pheno$leaf_DOY, x1 = Pheno$Feb_temp, Ntotal = length(Pheno$leaf_DOY)) #x2 = Pheno$Feb_snow, 

# Compile Model
model <- jags.model(textConnection(model_string), 
                    data = dataList, n.chains = 3)

## draw samples for burn-in
## we do not save the burn-in iterations
update(model, 10000)

## sample the posterior
## coda.samples will pick up from where update() ended
samp <- coda.samples(model, variable.names = c("beta"), n.iter = 20000)



## (3)	Check the convergence and autocorrelation of your chains post burn-in.  Briefly (1-2 sentences) describe 
## your interpretation of each diagnostic. 

# Plot summary parameters
plot(samp)
# Traces look good, the distrubition of beta[1] is not completely smooth, but sufficiently so

# Plot autocorrelation
autocorr.plot(samp)
# Looks good

# Gelman-Brooks-Rubin plot
gelman.plot(samp)
# Looks great!



## (4)	Try to solve any problems that you found in question 3.  Briefly describe what you tried and whether it
## solved the issue(s).  

# Let's double the number of iterations
model1 <- jags.model(textConnection(model_string), data = dataList, n.chains = 3)

## draw samples for burn-in
update(model1, 10000)

## sample the posterior
## added thinning (based on diagnostics of samp) and additional samples based on first round of diagnostics of samp1
samp1 <- coda.samples(model1, variable.names = c("beta"), n.iter = 40000)

autocorr.plot(samp1)
plot(samp1)
gelman.plot(samp1)






# Instead, let's make it 5 chains
model2 <- jags.model(textConnection(model_string), data = dataList, n.chains = 5)

## draw samples for burn-in
update(model2, 10000)

## sample the posterior
## added thinning (based on diagnostics of samp) and additional samples based on first round of diagnostics of samp1
samp2 <- coda.samples(model2, variable.names = c("beta"), n.iter = 20000)

autocorr.plot(samp2)
plot(samp2)
gelman.plot(samp2)



## (5)	Interpret your most well-behaved model from question 4.  Do high February temperature and/or low February
## snowfall accelerate leaf out?  By how much (including units)?  
summary(samp2)
# The resulting formulat is that the Leaf Out Day of Year = 86.8 - 0.71*high Feb. temperature





## In 1-2 sentences, identify the contribution of each group member to the assignment.  Upload your .R file to 
## submit your assignment in myCourses, one per group.






