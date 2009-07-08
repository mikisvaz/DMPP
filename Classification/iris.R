library(e1071)
library(MASS)
library(mvnormtest)
library(randomForest)

data(iris)
attach(iris)


# cmdscale produces a two dimensional plot of higher dimensional data, this is
# called multidimensional scaling and it based on placing points in the plane
# relative to their distance. The function dist calculates these distances. We
# remove the fifth variable, which we know is the response. And we use it to
# define the colors of the points.
plot(cmdscale(dist(iris[,-5])), col = as.integer(iris[,5]))


# This fits a native Bayes model

model.nb <- naiveBayes(Species ~ ., iris)
pred.nb <- predict(model.nb, iris[-5])

# The table function will make a table counting each possible combination of
# values for the first and second vector. In our case this gives us the
# confusion matrix
table(pred.nb, iris[,5])

# This fits a linear discriminant model
# Linear Discriminan Analysis
model.lda <- lda(Species ~ ., iris)
pred.lda <- predict(model.lda, iris[-5])$class
table(pred.lda, iris[,5])


# This following code plots the projection of the points into the LDA space.
# Notice its similarity with cmdscale, however this one uses the class
# information, so it is better. 
plot(lda(Species ~ .,data=iris ),abbrev=T, col=as.numeric(iris$Species))


# The following are just normality test to see if LDA assumptions of normality 
# are actually true

mshapiro.test(t(as.matrix(iris[iris[5] == 'setosa',-5])))
mshapiro.test(t(as.matrix(iris[iris[5] == 'versicolor',-5])))
mshapiro.test(t(as.matrix(iris[iris[5] == 'virginica',-5])))

# The results actually reject the null hypothesis of normality, they are NOT
# normaly distrbuted, still this method outperforms the other we examined...

# This fits a SVM model
model.svm <- svm(Species ~ ., iris,cost=500)
pred.svm <- predict(model.svm, iris[-5])
table(pred.svm, iris[,5])


# Same for a RandomForest

model.rf <- svm(Species ~ ., iris)
pred.rf <- predict(model.rf, iris[-5])
table(pred.rf, iris[,5])


# This function here is a general function to perform cross validation, makes use of 
# metaprogramming tricks, you really don't need to understand
my.cv <- function(data, response, groups = sample(rep(1:10,length.out=dim(data)[1])),method.fit, method.predict = expression(predict(model, test)),  method.error=expression(sqrt((fitted - observed)^2))){
	return(mean(sapply(unique(groups), function(group){
		train <- data[groups != group,]
		test <- data[groups  == group, names(data) != response]
		observed <- data[groups == group, response]		
		
		model <- eval(method.fit)
		fitted <- eval(method.predict)
				
		return(mean(eval(method.error)))
		
	})))
}

# We will now compute cross validation scores for all models. Note that some
# methods like svm have a cross validation functionality incorporated.

k=10
# We use the same groupings for all methods, its the fair thing to do...
groups = sample(rep(1:k,length.out=dim(iris)[1]))



my.cv(iris,'Species',groups,expression(naiveBayes(Species ~ ., train)), method.error= expression(fitted != observed))
my.cv(iris,'Species',groups,expression(lda(Species ~ ., train)), method.predict=expression(predict(model,test)$class),method.error= expression(fitted != observed))
my.cv(iris,'Species',groups,expression(svm(Species ~ ., train)), method.error= expression(fitted != observed))
my.cv(iris,'Species',groups,expression(randomForest(Species ~ ., train)), method.error= expression(fitted != observed))
