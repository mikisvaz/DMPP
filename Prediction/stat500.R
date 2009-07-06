# You might need to do this the first time you include the
# faraway library. Un comment this line if the next one
# gives an error.

#install.packages('faraway')

# Include the library compiling Faraway book's examples
library(faraway)


# Load the data from the results in Statistics 500 course
data(stat500)


# Print the variables: final and midterm
names(stat500)


# Print the dimensions: 55 students, and the 2 variables we saw before
dim(stat500)


# Plot the data, final results against midterm. The ~ is used to define
# formulas, plotting the formula final as function of midterm just assigns
# the variables to the axis. This is equivalent to plot(stat500$final, stat500$midterm),
# I just like using formulas better for consistency.
plot(final ~ midterm, stat500, xlab='Midterm grades', ylab='Final grades')


# Produce a lineal model (method lm), again, we use the formula. This produces
# a fit model and saves it in g
g <- lm(final ~ midterm, stat500)



####################################
# Brief digression about R

# Produce a custom report for the fit. Some objects have a custom way of presenting
# their information, lm fits shows the coefficients
g


# Summary extracts a more verbose report for an object, again, objects define their own 
# way of been summarized, lm fits show some additional statistics
summary(g)


# The actual contents of an object can be examined using str() function, which
# examines the structure of the object. This helps us to figure out which information
# is accessible and can be used for other purposes. Note that R is a relatively
# complex programming language in terms of the object types. Do not worry about
# the details.
str(g)


# The names function can show just the names of the attributes of the object, which is 
# less verbose than str()
names(g)




####################################
# Back to our analysis

# Abline is used to add lines to a current plot. A plot() function must have been
# called previously. In this case abline is able to extract from the lm fit the
# information necessary to plot the line, the intercept and slope coefficients. R
# is smart that way...
abline(g,col='red')


# What we are going to do now is more involved, we would like to plot confidence
# interval, this requires evaluating the fit in a sequence of points evenly spaced.

# Define the new points. A data frame is the most common way to hold information
# in R, it represents elements with variables, where variables recieve names. The
# stat500 object is a data frame. We need a new data frame to hold new values. 
# Since we are using it to predict, we don't need to specify the final, which is
# the response we wish to predict. The midterms are defined as a sequence of points
# between 1 and 30 evenly spaced every 0.1.
new = data.frame(midterm = seq(1,30,0.1))

# Lets take a look at what we have done
names(new)
dim(new)

# This shows the first 10 elements. The second selector in between [] is empty,
# meaning that it will show all the variables, in our case just midterm, this
# is equivalent to new[1:10, 'midterm']
new[1:10,]


# We are going to produce the predictions. predict() is a polymorphic function, 
# meaning that it examines the first object, determines it's type, and calls the
# appropriate method for that object. Effectively the method predict.lm() is
# called, and we could have just used that and it would have made no difference.
# The rest of the arguments are predict to produce confidence interval for the
# linear fit at a 0.95 level of confidence. These intervals are for where the real
# function would be expected to be and have produce these data, we can be sure
# at a 95% confidence that the real function is not outside this interval, because
# it would not have produce data like this.
predictions = predict(g, new, interval='confidence',level=0.95)


# The predictions object hold the response for each input element plus an lwr
# and upr value, which are the upper and lower confidence values
str(predictions)


# Note that names() does not show the names of the variables, because predictions
# is not a data frame but an array. R is complex that way, you just have to
# get used to it... sorry
names(predictions)


# We draw them into the previous plot. Again, the plot must be allready be there.
# we use lines to draw a line across the points, and set the color and style of the
# line. We use cbind to bind columns, which makes each element a two variable element
# (midterm, prediction), these to variables are taken as points in the plot and
# joined by lines.
lines(cbind(new, predictions[,'lwr']), col='blue', lty='dashed')
lines(cbind(new, predictions[,'upr']), col='blue', lty='dashed')



# Lets examine the cbind function again. The results is a data frame, since
# the first was a data frame, again
tmp = cbind(new, predictions['lwr'])
names(tmp)
dim(tmp)




# Here we do something similar, but now for the prediction confidence interval
predictions = predict(g, new, interval='prediction',level=0.95)
lines(cbind(new, predictions[,'lwr']), col='green', lty='dotted')
lines(cbind(new, predictions[,'upr']), col='green', lty='dotted')


# Finally we are going to do some residual analysis. Just like objects have 
# custom ways of printing and summarizing themselves, they sometimes also
# have ways of plotting themselves. Plotting the lm fit ends up producing 
# a series of plots for the residuals. Use enter to cycle between them. Making
# new plots removes the previous ones.
plot(g)

