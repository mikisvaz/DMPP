
# The following two functions will be used for cross validations and forward stepwise
# subset selection based on cross validation. Again, I'm using metaprogramming
# tricks you don't need to be concerned with
my.cv <- function(data, response, groups = NULL,method.fit, method.predict = expression(predict(model, test)),  method.error=expression(sqrt((fitted - observed)^2)), formula=NULL){

    if (is.null(groups)) groups = sample(rep(1:10,length.out=dim(data)[1]))	
	return(mean(sapply(unique(groups), function(group){
		train <- data[groups != group,]
		test <- data[groups  == group, names(data) != response]
		observed <- data[groups == group, response]		
		
		model <- eval(method.fit)
		fitted <- eval(method.predict)
				
		return(mean(eval(method.error)))
		
	})))
}

my.forward.cv <- function(data, response, method.fit, method.predict = expression(predict(model, test)),  method.error=expression(sqrt((fitted - observed)^2))){
	candidates = names(data)
	candidates = candidates[candidates != response]

	best.error = -1
	last.error = Inf
	current = c('1')
	groups = sample(rep(1:10,length.out=dim(data)[1]))	
	while(last.error > best.error && length(candidates) > 0){
		if(best.error == -1) last.error = Inf
		else last.error = best.error			
		best.error = Inf;
		for (term in candidates){
			test = c(current,term)
			formula = as.formula(paste( response , paste(test, collapse='+'), sep=" ~ "))
			cv.error = my.cv(data, response, groups, method.fit, method.predict, method.error, formula=formula)
			if  (cv.error < best.error){
				best.error = cv.error
				best.term = term
			}
		}
		if (best.error < last.error){
			current = c(current, best.term)
			candidates = candidates[candidates != best.term]
		}
	}
	return(list(terms = current, error = last.error, formula = as.formula(paste( response , paste(current, collapse='+'), sep=" ~ ")))
	);
}


library(ElemStatLearn)
library(splines)
library(randomForest)
library(e1071)
library(rpart)


data(SAheart)

# This variable was not used in the book example, and I do not know what it is
# so I'm removing it
SAheart$typea <- NULL

# Find the best subset using CV for all 4 methods
best.tree = my.forward.cv(SAheart,'chd', expression(rpart(formula, train)), expression(predict(model, test) > 0.5), expression(fitted != observed))
best.lm   = my.forward.cv(SAheart,'chd', expression(lm(formula, train)), expression(predict(model, test) > 0.5), expression(fitted != observed))
best.svm  = my.forward.cv(SAheart,'chd', expression(svm(formula, train)), expression(predict(model, test) > 0.5), expression(fitted != observed))
best.lr   = my.forward.cv(SAheart,'chd', expression(glm(formula, train,family=binomial(link="logit"))), expression(predict(model, test, type="response") > 0.5), expression(fitted != observed))

# Print the results
print(best.tree)
print(best.lm)
print(best.svm)
print(best.lr)


summary(lm(best.lm$formula, SAheart))
summary(glm(best.lm$formula, SAheart, family=binomial(link="logit")))


# This fits a simple tree. Note that chd needs a explicit conversion to factor. 
# A factor is a variable type specially fit to represent class labels.
model.tree = rpart(factor(chd) ~ .,SAheart)
print(model.tree)
plot(model.tree,uniform=T)
text(model.tree)

# This fits a randomForest. They perform exceptionally well in most cases and also
# provide with useful information
model.rf <- randomForest(factor(chd) ~., SAheart)
sort(model.rf$importance[,1],decreasing=T)

