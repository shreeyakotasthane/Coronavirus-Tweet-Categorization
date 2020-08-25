library(kernlab)
library(e1071)
library(neuralnet)
library(cluster)
library(factoextra)
#library(specc)
library(mclust)
library(dbscan)
library(cluster)
library(caret)
library(mlbench)

################################LIKES###################################
########################################################################
########################################################################
########################################################################
df <- read.csv(file.choose())

#creating testing sets
testidx <- which(1:nrow(df) %% 4 == 0 )

df_training <- df[-testidx,]
df_testing <- df[testidx,]

#Q1
#a
favorite_classifier <- svm(favorite_count~., data=df_training, type =  "eps-regression")

svr_prediction <- predict(favorite_classifier, df_testing)

cor(svr_prediction,	df_testing$favorite_count)	

#b with kernel 1
favorite_classifier <- svm(favorite_count~., data=df_training, type = "eps-regression", kernel = "sigmoid")

svr_prediction <- predict(favorite_classifier, df_testing)

cor(svr_prediction,	df_testing$favorite_count)	

#b with kernel 2
favorite_classifier <- svm(favorite_count~., data=df_training, type = "eps-regression", kernel = "poly")

svr_prediction <- predict(favorite_classifier, df_testing)

cor(svr_prediction,	df_testing$favorite_count)	

##############
### 2 NN #####
##############

normalize	<-	function(x)	{		return((x	-	min(x))	/	(max(x)	-	min(x)))	}	
#	apply	normaliza@on	to	en@re	data	frame	
fav_norm_train	<-	as.data.frame(lapply(df_training,	normalize))	
fav_norm_train <- subset(fav_norm_train, select = -c(quote_count, reply_count))

fav_norm_test <- as.data.frame(lapply(df_testing,	normalize))	
fav_norm_test <- subset(fav_norm_test, select = -c(quote_count, reply_count))

favorite_model	<-	neuralnet(favorite_count~., data=fav_norm_train)	
favorite_model$result.matrix
favorite_model$act.fct
favorite_model$weights
#plotting nn
plot(favorite_model)

#computing strength and nodes
summary(favorite_model)
fav_model_results = compute(favorite_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$favorite_count)	

######### Investigating the model with 5 hiddne layers #############
favorite_model	<-	neuralnet(favorite_count~., data=fav_norm_train, hidden = 5)	
favorite_model$result.matrix
favorite_model$act.fct
favorite_model$weights
#plotting nn
plot(favorite_model)

#computing strength and nodes
summary(favorite_model)
fav_model_results = compute(favorite_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$favorite_count)	

######### Investigating the model with 10 hiddne layers #############
favorite_model	<-	neuralnet(favorite_count~., data=fav_norm_train, hidden = 10)	
favorite_model$result.matrix
favorite_model$act.fct
favorite_model$weights
#plotting nn
plot(favorite_model)

#computing strength and nodes
summary(favorite_model)
fav_model_results = compute(favorite_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$favorite_count)	



######################
### 3 Clustering #####
######################

######### K MEANS
########
#######
######
#####

fviz_nbclust(fav_norm_train, kmeans, method = "wss") #elbow test to determine how many centers we should have
k <- kmeans(fav_norm_train, centers = 5, nstart = 25)

#amount of clusters and their sizes
k$size

#cluster plot for the k means
clusplot(fav_norm_train, k$cluster, main = "Cluster plot for K means")


######### SPECTRAL
########
#######
######
#####
db <- dbscan(fav_norm_train, eps=.5, MinPts = 8)
db
clusplot(fav_norm_train, db$cluster, main = "Cluster plot for Dbscan")

######### MCLUST
########
#######
######
#####

lust <- Mclust(fav_norm_train)
summary(lust)
clusplot(fav_norm_train, lust$classification, main = "Cluster plot for McLust")

######################
##### 4 Caret ########
######################

#CV Technique 1
control	<-	trainControl(method= "repeatedcv",	number=2, repeats=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(favorite_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(favorite_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(favorite_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	

#CV Technique 2
control	<-	trainControl(method= "cv",	number=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(favorite_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(favorite_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(favorite_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	

#CV Technique 3
control	<-	trainControl(method= "boot",	number=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(favorite_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(favorite_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(favorite_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	

############################RETWEETS####################################
########################################################################
########################################################################
########################################################################
df <- read.csv(file.choose())

#creating testing sets
testidx <- which(1:nrow(df) %% 4 == 0 )

df_training <- df[-testidx,]
df_testing <- df[testidx,]

#Q1
#a
retweet_classifier <- svm(retweet_count~., data=df_training, type =  "eps-regression")

svr_prediction <- predict(retweet_classifier, df_testing)

cor(svr_prediction,	df_testing$retweet_count)	

#b with kernel 1
retweet_classifier <- svm(retweet_count~., data=df_training, type = "eps-regression", kernel = "sigmoid")

svr_prediction <- predict(retweet_classifier, df_testing)

cor(svr_prediction,	df_testing$retweet_count)	

#b with kernel 2
retweet_classifier <- svm(retweet_count~., data=df_training, type = "eps-regression", kernel = "poly")

svr_prediction <- predict(retweet_classifier, df_testing)

cor(svr_prediction,	df_testing$retweet_count)	

##############
### 2 NN #####
##############

normalize	<-	function(x)	{		return((x	-	min(x))	/	(max(x)	-	min(x)))	}	
#	apply	normaliza@on	to	en@re	data	frame	
fav_norm_train	<-	as.data.frame(lapply(df_training,	normalize))	
fav_norm_train <- subset(fav_norm_train, select = -c(quote_count, reply_count))

fav_norm_test <- as.data.frame(lapply(df_testing,	normalize))	
fav_norm_test <- subset(fav_norm_test, select = -c(quote_count, reply_count))

retweet_model	<-	neuralnet(retweet_count~., data=fav_norm_train)	
retweet_model$result.matrix
retweet_model$act.fct
retweet_model$weights
#plotting nn
plot(retweet_model)

#computing strength and nodes
summary(retweet_model)
fav_model_results = compute(retweet_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$retweet_count)	

######### Investigating the model with 5 hiddne layers #############
retweet_model	<-	neuralnet(retweet_count~., data=fav_norm_train, hidden = 5)	
retweet_model$result.matrix
retweet_model$act.fct
retweet_model$weights
#plotting nn
plot(retweet_model)

#computing strength and nodes
summary(retweet_model)
fav_model_results = compute(retweet_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$retweet_count)	

######### Investigating the model with 10 hiddne layers #############
retweet_model	<-	neuralnet(retweet_count~., data=fav_norm_train, hidden = 10)	
retweet_model$result.matrix
retweet_model$act.fct
retweet_model$weights
#plotting nn
plot(retweet_model)

#computing strength and nodes
summary(retweet_model)
fav_model_results = compute(retweet_model, fav_norm_test)
summary(fav_model_results)
predicted_favs <- fav_model_results$net.result
predicted_favs

#examining correlation between predicted and actual values
cor(predicted_favs,	fav_norm_test$retweet_count)	



######################
### 3 Clustering #####
######################

######### K MEANS
########
#######
######
#####

fviz_nbclust(fav_norm_train, kmeans, method = "wss") #elbow test to determine how many centers we should have
k <- kmeans(fav_norm_train, centers = 5, nstart = 25)

#amount of clusters and their sizes
k$size

#cluster plot for the k means
clusplot(fav_norm_train, k$cluster, main = "Cluster plot for K means")


######### SPECTRAL
########
#######
######
#####
db <- dbscan(fav_norm_train, eps=.5, MinPts = 8)
db
clusplot(fav_norm_train, db$cluster, main = "Cluster plot for Dbscan")

######### MCLUST
########
#######
######
#####

lust <- Mclust(fav_norm_train)
summary(lust)
clusplot(fav_norm_train, lust$classification, main = "Cluster plot for McLust")

######################
##### 4 Caret ########
######################

#CV technique 1
control	<-	trainControl(method= "repeatedcv",	number=2, repeats=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(retweet_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(retweet_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(retweet_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	

#CV technique 2
control	<-	trainControl(method= "cv",	number=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(retweet_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(retweet_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(retweet_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	

#CV technique 3
control	<-	trainControl(method= "boot",	number=2)	

#Q1 Milestone 3
df_training <- df_training[complete.cases(df_training), ]
df_training <- subset(df_training, select = -c(quote_count, reply_count))
modelSVR	<-	train(retweet_count~., data=df_training,	method="svmLinear2", trControl=control, tunelength = 2)	
#Q2 Milestone 3
modelNN	<-	train(retweet_count~., data=fav_norm_train,	method="nnet", trControl=control,tunelength = 2)
#Q3 Milestone 2
modelRF <- train(retweet_count~., data=fav_norm_train,	method="rf", trControl=control,tunelength = 2)
warnings()
#comparisons
results	<-	resamples(list(SVR=modelSVR,	NN=modelNN,	RF=modelRF))

summary(results)	
#boxplots	of	results	
bwplot(results)	
#dot	plots	of	results	
dotplot(results)	


