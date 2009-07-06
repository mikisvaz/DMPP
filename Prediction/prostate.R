library(ElemStatLearn)

data(prostate)


# This data is from prostate cancer, the response we wish
# to model is lpsa, the log expression of the cancer
# related gene PSA
names(prostate)

# The prostate data frame has an additional variable called train
# used to separate the data into train and test. To avoid
# having to subset the data frame all the time we separate the data
# into two variables
prostate.test = prostate[prostate$train == 0,]
prostate = prostate[prostate$train == 1,]

# This removes the train variable from both datasets
prostate.test$train = NULL
prostate$train = NULL


# This attaches the data to the workspace, the
# variables in the data frame are now accesible
# directly by there names, so lcavol will now by
# equal to prostate$lcavol
attach(prostate)

##############################
# Base line model

# We create a lineal model consisting only on the intercep.
# This model is used a a baseline model for comparison.
# Nota the we dont need to include the data frame in the 
# function as in lm(lpsa ~ 1, prostate), since we have it 
# attached. Note that variable names may include points '.',
# in other programming languages points have special meanings,
# not in R
g.base <- lm(lpsa ~ 1)
g.base


# Compute the predictions for the test and calculate
# the absolute error. We use absolute error here as a measure
# of error even though lm minimizes square error, this
# is for no particular reason
pred.base <- predict(g.base, prostate.test)
error.base <- sum(abs(pred.base - prostate.test$lpsa))/length(prostate.test$lpsa)




##############################
# Complete linear model


# This fits a lineal model with all the variables. A point in the formula
# get substituted by all possible variables. For lm to figure what these
# variables are we need to include the prostate data frame again, even though
# we had it attached...
g.lm <- lm(lpsa ~., prostate)
g.lm
pred.lm <- predict(g.lm, prostate.test)
error.lm <- sum(abs(pred.lm - prostate.test$lpsa))/length(prostate.test$lpsa)


##############################
# Sub-setted lineal model


# To be able to decide if a model offers an improvement
# over a simpler one we need a way to compare them. Here
# we define a method that fits two models with different
# sets of predictors and returns the p-value for the
# F-statistic.
compare_models <- function(data, response, current, new){
	# Fit both models. The paste function is used to construct the actual formula to
	# fit based on the predictors we wish to include. Do not worry about this!
	formula.current = as.formula(paste(response,' ~ ', paste(current,collapse = '+'),sep=" "))
	formula.new = as.formula(paste(response,' ~ ', paste(new,collapse = '+'),sep=" "))
	
	# Un comment this line to print the formula if you don't belive me! But
	# this will generate a lot of noise in the terminal output
	#
	#print("Formula:"); print(formula.new)
	
	g.current <- lm(formula.current,data)
	g.new <- lm(formula.new,data)
	
	
	# Anova is used to measure the improvements, it just gives us the F-statistic
	# we saw in class, its the same as the formula. The variable 'Pr(>F)' gives us
	# the p-value. Again, un comment the line for a trace, but that will be noisy
	#
	# print(anova(g.current, g.new))
	
	return(anova(g.current, g.new)$'Pr(>F)'[2])
}


# Start the variable selection

# These are all the candidate predictors
candidates = names(prostate)
candidates = candidates[candidates != 'lpsa']

# We start with only the intercept '1', usually you don't need
# to specify the intercept, it is assumed, but we need it for
# my little paste hack above to work :(

current = c('1')
response='lpsa'
best.p = 0
threshold = 0.1 # Threshold for the F p-value
while(best.p <= threshold){
	best = 'none'
	best.p = 1
	
	# Test all remaining candidates
	for(term in candidates){
		new = c(current,term)
		p = compare_models(prostate, response, current, new)
	    if (p < best.p){
		  best = term
		  best.p = p
		}
	}
	
	# If there is an improvement, add the term
	# to the current set, and remove from candidates
	if(best.p < threshold){
		current = c(current, best);
		candidates = candidates[candidates != best];
	}
}

# These where the selected predictors
print(current)

# Fit and evaluate the model
g.bs <- lm(as.formula(paste(response,' ~ ', paste(current,collapse = '+'),sep=" ")),prostate)
g.bs
pred.bs <- predict(g.bs, prostate.test)
error.bs <- sum(abs(pred.bs - prostate.test$lpsa))/length(prostate.test$lpsa)


##############################
# Ridge regression


# The ridge regression is implemented in simple.ridge (and probably in some other)
# package. simple.ridge does not accept formulas, so we need to indicate the
# predictor matrix and the response vector separatedly. This function also
# accepts the degrees of freedom that we wish to use.
g.ridge <- simple.ridge(prostate[,names(prostate) != 'lpsa'], prostate$lpsa,df=4)
g.ridge

# Prediction is also more complicated since ridge regression scaled internaly the
# inputs. 
pred.ridge <- as.matrix(
	cbind(1,scale(prostate.test,colMeans(prostate),TRUE)))[,1:9] %*% # Scale the inputs
	as.matrix(c(g.ridge$beta0,g.ridge$beta) # Append the intercept to the rest
)

error.ridge <- sum(abs(pred.ridge - prostate.test$lpsa))/length(prostate.test$lpsa)



# Finally, lets compare all the errors
print(error.base)
print(error.lm)
print(error.bs)
print(error.ridge)