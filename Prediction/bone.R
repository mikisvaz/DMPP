library(ElemStatLearn)
data(bone)


# Lets take a look
names(bone)
dim(bone)


attach(bone)

# This range of points will be used to draw the actual splines. 
# 200 points in the range of min(age) and max(age)
range=data.frame(age = seq(min(age),max(age), length.out = 200))


# Plot spine bone mineral density change as a function of age. Blue points are
# males and red points are females
plot(spnbmd ~ age, data=bone, main="Linear", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

#################################
# Linear Plots

# We do three fits, one for all points and two for males and females separately
g.total = lm(spnbmd ~ age)
abline(g.total)

# The with subset is a common way to select only part of the data frame. R
# is full of tricks like this, unfortunately sometimes they are hard to
# come across...
g.male = with(subset(bone,gender=="male"),lm(spnbmd ~ age))
abline(g.male, col='blue')

g.female <- with(subset(bone,gender=="female"),lm(spnbmd ~ age))
abline(g.female, col='red2')

##############################
# Fit a Polynomial basis expansion

# Repeat the plot
plot(spnbmd ~ age, data=bone,main="Polynomial", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

# Notice how we include a two and three degree polynomial. You need to use the I() to
# protect the ^ from  been evaluated, more R particularities...
g.total <- lm(spnbmd ~ age + I(age^2) + I(age^3), bone)
lines(cbind(range,predict(g.total, range)))

g.male <- with(subset(bone, gender=="male"), lm(spnbmd ~ age + I(age^2) + I(age^3)))
lines(cbind(range,predict(g.male, range)),col='blue')

g.female <- with(subset(bone, gender=="female"), lm(spnbmd ~ age + I(age^2) + I(age^3)))
lines(cbind(range,predict(g.female, range)),col='red2')


##############################
# Fit a Kernel Regression smoother

# Repeat the plot
plot(spnbmd ~ age, data=bone,main="Kernel (Loess)", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

# Loess fits a local weighted linear model
g.total <- loess(spnbmd ~ age)
lines(cbind(range,predict(g.total, range)))

g.male <- with(subset(bone, gender=="male"), loess(spnbmd ~ age))
lines(cbind(range,predict(g.male, range)),col='blue')

g.female <- with(subset(bone, gender=="female"), loess(spnbmd ~ age))
lines(cbind(range,predict(g.female, range)),col='red2')

##############################
# Fit Smoothing Spline

# Repeat the plot
plot(spnbmd ~ age, data=bone, main="Smoothing Spline", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

# Smooth splines are a fascinating scatterplot smoothing technique with its basis
# in regularization over Reproducing Kernel Hilbert Spaces that need not concern you
# now. Just know, they are easy to use and work very well. Internally they are 
# equivalent to cubic splines with knots at every point
g.total <- smooth.spline(spnbmd ~ age,df=12)
lines(g.total)

g.male <- with(subset(bone, gender=="male"), smooth.spline(spnbmd ~ age,df=12))
lines(g.male,col='blue')

g.female <- with(subset(bone, gender=="female"), smooth.spline(spnbmd ~ age,df=12))
lines(g.female,col='red2')



##############################
# Fit Natural Cubic Spline
library(splines)

# Repeat the plot
plot(spnbmd ~ age, data=bone, main="Natural Cubic Spline", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

# The ns function produces a basis expansion of the points with knots evenly spaced
# The degrees of freedom specify the number of knots to use
g.total <- lm(spnbmd ~ ns(age, df=12))
lines(cbind(range,predict(g.total, range)))

# Lest look at that basis expansion. Each element is now a point in a 12 dimensional
# space
tmp = ns(age, df=12)
dim(tmp)


g.male <- with(subset(bone, gender=="male"), lm(spnbmd ~ ns(age, df=12)))
lines(cbind(range,predict(g.male, range)),col='blue')

g.female <- with(subset(bone, gender=="female"), lm(spnbmd ~ ns(age, df=12)))
lines(cbind(range,predict(g.female, range)),col='red2')


##############################
# Fit B-Spline

# Repeat the plot
plot(spnbmd ~ age, data=bone, main="B-Spline", col = ifelse(gender=="male", "blue", "red2"), xlab="Age", ylab="Relative Change in Spinal BMD")

# These are actually very similar. B-splines have other useful properties, outside
# the scope of this lecture. Just notice the similarity 
df = 6
g.total <- lm(spnbmd ~ bs(age, df=df))
lines(cbind(range,predict(g.total, range)))

g.male <- with(subset(bone, gender=="male"), lm(spnbmd ~ bs(age, df=df)))
lines(cbind(range,predict(g.male, range)),col='blue')

g.female <- with(subset(bone, gender=="female"), lm(spnbmd ~ bs(age, df=df)))
lines(cbind(range,predict(g.female, range)),col='red2')

#############
# Lets study the effect of the degrees of freedom in B-splines using
# AIC

# sapply takes a list of elements and evaluates a function on each of them.
# This sort of method is common in functional programming and are one of my
# favorite things in R...
AIC.values =sapply(seq(5,20),function(df){
	g <- lm(spnbmd ~ bs(age, df=df))
	return(AIC(g))
})

# Plot the AIC values with a line
plot(cbind(seq(5,20),AIC.values),main="AIC values", type='l')


#############
# We do a similar analysis but using cross validation.

# Use 10-fold cross validation
k=10

# This assigns our points to k classes randomly. rep()
# produces a list of numbers from 1 to 10 repeated until
# we have 485. These assignment is possibly random, there is
# no reason to believe the elements are in no kind of order, but
# to be sure we use sample to shuffle them again
groups = sample(rep(1:k,length.out=dim(bone)[1]))

# Lets take a look at the result. We use length instead of dim
# because it has only one dimension, another annoyance of R
length(groups)
groups[1:20]

# We try degrees of freedom from 5 to 20
cv.values = sapply(seq(5,20),function(df){
	# We do the cross validation. Each group is used as test
	# for the model trained by the rest. Again, the cross validation
	# criteria is absolute error
	total = sum(sapply(1:k,function(group){
		g <- lm(spnbmd ~ bs(age, df=df),subset= groups != group)
		pred <- predict(g, bone[groups == group,])
		return(sum(abs(pred - bone[groups==group,'spnbmd'])))
	}))
	return(total)
})

# Plot the shape of the cross validation values
plot(cbind(seq(5,20),cv.values),main="Cross Validation", type='l',ylim=c(14.80,15.20))

# Repeat 10 times and add the lines to see the variability
# of cross validation. This might take a while...
for(i in 1:10){
	groups = sample(rep(1:k,length.out=dim(bone)[1]))
	cv.values = sapply(seq(5,20),function(df){
		total = sum(sapply(1:k,function(group){
			g <- lm(spnbmd ~ bs(age, df=df),subset= groups != group)
			pred <- predict(g, bone[groups == group,])
			return(sum(abs(pred - bone[groups==group,'spnbmd'])))
		}))
		return(total)
	})
	lines(cbind(seq(5,20),cv.values),type='l')
}

