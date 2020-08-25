mydata <- read.csv(file.choose())


################
#              #
#    PART1     #
#   Favorites  #
#              #
################

#prepping the data
interest <- mydata[c('text','user_verified','user_followers_count',
                     'is_quoted_tweet','quote_count','reply_count','retweet_count',
                     'favorite_count','word_count','hash_count','sentiment')]

#creating testing sets
testidx <- which(1:nrow(interest) %% 4 == 0 )

df_training <- interest[-testidx,]
df_testing <- interest[testidx,]

#1a var1 - user_verified
favorite_user_lm <- lm(favorite_count~user_verified, data=df_training)
summary(favorite_user_lm)

pc <- predict(favorite_user_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_user_lm,	int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$user_verified)	
matlines(df_testing$user_verified,pc)	
matlines(df_testing$user_verified,pp)	
rs <- residuals(favorite_user_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)

#1a var3 - is_quoted_tweet
favorite_quoted_lm <- lm(favorite_count~is_quoted_tweet, data=df_training)
summary(favorite_quoted_lm)

pc <- predict(favorite_quoted_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_quoted_lm,int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$is_quoted_tweet)	
matlines(df_testing$is_quoted_tweet,pc)	
matlines(df_testing$is_quoted_tweet,pp)	
rs <- residuals(favorite_quoted_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)

#1a var4 - quote_count
favorite_quotedcount_lm <- lm(favorite_count~quote_count, data=df_training)
summary(favorite_quotedcount_lm)

pc <- predict(favorite_quotedcount_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_quotedcount_lm,int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$quote_count)	
matlines(df_testing$quote_count,pc)	
matlines(df_testing$quote_count,pp)	
rs <- residuals(favorite_quotedcount_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)

#1a var5 - word_count
favorite_wordcount_lm <- lm(favorite_count~word_count, data=df_training)
summary(favorite_wordcount_lm)

pc <- predict(favorite_wordcount_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_wordcount_lm,int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$word_count)	
matlines(df_testing$word_count,pc)	
matlines(df_testing$word_count,pp)
rs <- residuals(favorite_wordcount_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)


#1a var6 - hash_count
favorite_hashcount_lm <- lm(favorite_count~hash_count, data=df_training)
summary(favorite_hashcount_lm)

pc <- predict(favorite_hashcount_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_hashcount_lm,int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$hash_count)	
matlines(df_testing$hash_count,pc)	
matlines(df_testing$hash_count,pp)
rs <- residuals(favorite_hashcount_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)

#1a var6 - sentiment
favorite_sentiment_lm <- lm(favorite_count~sentiment, data=df_training)
summary(favorite_sentiment_lm)

pc <- predict(favorite_sentiment_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_sentiment_lm,int="p",newdata=df_testing)	
plot(df_testing$favorite_count, df_testing$sentiment)	
matlines(df_testing$sentiment,pc)	
matlines(df_testing$sentiment,pp)	
rs <- residuals(favorite_sentiment_lm)		
qqnorm(rs)		
qqline(rs)
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)






#multivariate
favorite_lm <- lm(favorite_count ~ user_followers_count + user_verified + word_count, data = df_training)
summary(favorite_lm)

favorite.fit <- function(follower_count,user,word_count) favorite_lm$coefficients[1] +
  favorite_lm$coefficients[2]*follower_count + 
  favorite_lm$coefficients[3]*user + 
  favorite_lm$coefficients[4]*word_count

favorite.fit(df_testing$user_followers_count, df_testing$user_verified, df_testing$word_count)

pc <- predict(favorite_lm,int="c",newdata=df_testing)	
pp <- predict(favorite_lm,int="p",newdata=df_testing)	
cor(pc,	df_testing$favorite_count)	
cor(pp,	df_testing$favorite_count)


#with regularization
library(glmnet)	

cv.fit	<-	
  cv.glmnet(as.matrix(df_training[,c(-1, -7,-8)]),																									
            as.vector(df_training[,8]),	
            alpha=1)						
plot(cv.fit)		
coef(cv.fit)

prediction <- predict(cv.fit,																										
                       newx=as.matrix(df_testing[,c(-1,	-7,-8)]))		
#correlation	or	mean	square	error	to	check	result	
cor(prediction,	as.vector(df_testing[,8]))	

#Skipping 2
#Because of the nature of this problem, neither Logistical Regression and NB 
#would be models that we would be able to use for continuous number data. 
#If the data were categorical we might find ourselves using these models, 
#but in our case we are trying to predict a number (the number of likes that a 
#tweet will receive). 

#Answering 3a

interest_random <- interest[order(runif(20048)),]


df_training_random$favorite_count<-as.factor(df_training_random$favorite_count)

interest_random$favorite_count <- cut(interest_random$favorite_count, breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                      labels=paste("likes", 1:5, sep=""))
interest_random$sentiment <- cut(interest_random$sentiment, breaks=c(-1, -0.5, 0, .5,Inf), 
                                 labels=paste("sentiment", 1:4, sep=""))
interest_random$retweet_count <- cut(interest_random$retweet_count, breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                     labels=paste("retweet", 1:5, sep=""))
interest_random$user_followers_count <- cut(interest_random$user_followers_count, breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                            labels=paste("followers", 1:5, sep=""))
#making trainign and testing sets
testidx <- which(1:nrow(interest_random) %% 4 == 0 )
df_training_random <- interest_random[-testidx,]
df_training_random <- df_training_random[,-1]
df_testing_random <- interest_random[testidx,]
df_testing_random <- df_testing_random[,-1]

summary(interest)
summary(interest_random)


#3b
library(C50)
tree_model <- C5.0(df_training_random[c(-1,-6,-7)],df_training_random$favorite_count)
summary(tree_model)

tree_predict <- predict(tree_model, df_testing_random)
summary(tree_predict)
library(gmodels)
CrossTable(df_testing_random$favorite_count,tree_predict)


#3c - boosting
tree_model <- C5.0(df_training_random[c(-1,-6,-7)],df_training_random$favorite_count,trials = 10)
summary(tree_model)

tree_predict_10 <- predict(tree_model, df_testing_random)
summary(tree_predict_10)

CrossTable(df_testing_random$favorite_count,tree_predict_10)
#performs better

#3d - bagging
library(randomForest)

df_training_random.new <- droplevels(df_training_random)
bag.likes <- randomForest(favorite_count ~ . ,data=df_training_random.new, 
                          mtry = 9,importance = TRUE)	
pred <- predict(bag.likes, newdata=df_testing_random)	
CrossTable(df_testing_random$favorite_count, pred)

#changing number of trees - 10
bag.likes <- randomForest(favorite_count ~ . ,data=df_training_random.new, 
                          mtry =	9,importance = TRUE, ntree = 10)	
pred <- predict(bag.likes, newdata=df_testing_random)	
CrossTable(df_testing_random$favorite_count, pred)

#changing number of trees - 20
bag.likes <- randomForest(favorite_count ~ . ,data=df_training_random.new, 
                          mtry =	9,importance = TRUE, ntree = 20)	
pred <- predict(bag.likes, newdata=df_testing_random)	
CrossTable(df_testing_random$favorite_count, pred)

#changing number of trees - 30
bag.likes <- randomForest(favorite_count ~ . ,data=df_training_random.new, 
                          mtry =	9,importance = TRUE, ntree = 30)	
pred <- predict(bag.likes, newdata=df_testing_random)	
CrossTable(df_testing_random$favorite_count, pred)

#changing number of trees - 100
bag.likes <- randomForest(favorite_count ~ . ,data=df_training_random.new, 
                          mtry =	9,importance = TRUE, ntree = 100)	
pred <- predict(bag.likes, newdata=df_testing_random)	
CrossTable(df_testing_random$favorite_count, pred)

################
#              #
#    PART2     #
#   Retweets   #
#              #
################

#prepping the data
interest <- mydata[c('text','user_verified','user_followers_count',
                     'is_quoted_tweet','quote_count','reply_count','retweet_count',
                     'favorite_count','word_count','hash_count','sentiment')]

#creating testing sets
testidx <- which(1:nrow(interest) %% 4 == 0 )

df_training <- interest[-testidx,]
df_testing <- interest[testidx,]

#1a var1 - user_verified
retweet_user_lm <- lm(retweet_count~user_verified, data=df_training)
summary(retweet_user_lm)

pc <- predict(retweet_user_lm,int="c",newdata=df_testing)  
pp <- predict(retweet_user_lm, int="p",newdata=df_testing) 
plot(df_testing$retweet_count, df_testing$user_verified) 
matlines(df_testing$user_verified,pc) 
matlines(df_testing$user_verified,pp) 
rs <- residuals(retweet_user_lm)   
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)

#1a var3 - is_quoted_tweet
retweet_quoted_lm <- lm(retweet_count~is_quoted_tweet, data=df_training)
summary(retweet_quoted_lm)

pc <- predict(retweet_quoted_lm,int="c",newdata=df_testing)  
pp <- predict(retweet_quoted_lm,int="p",newdata=df_testing)  
plot(df_testing$retweet_count, df_testing$is_quoted_tweet) 
matlines(df_testing$is_quoted_tweet,pc) 
matlines(df_testing$is_quoted_tweet,pp) 
rs <- residuals(retweet_quoted_lm)   
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)

#1a var4 - quote_count
retweet_quotedcount_lm <- lm(retweet_count~quote_count, data=df_training)
summary(retweet_quotedcount_lm)

pc <- predict(retweet_quotedcount_lm,int="c",newdata=df_testing) 
pp <- predict(retweet_quotedcount_lm,int="p",newdata=df_testing) 
plot(df_testing$retweet_count, df_testing$quote_count) 
matlines(df_testing$quote_count,pc) 
matlines(df_testing$quote_count,pp) 
rs <- residuals(retweet_quotedcount_lm)    
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)

#1a var5 - word_count
retweet_wordcount_lm <- lm(retweet_count~word_count, data=df_training)
summary(retweet_wordcount_lm)

pc <- predict(retweet_wordcount_lm,int="c",newdata=df_testing) 
pp <- predict(retweet_wordcount_lm,int="p",newdata=df_testing) 
plot(df_testing$retweet_count, df_testing$word_count)  
matlines(df_testing$word_count,pc)  
matlines(df_testing$word_count,pp)
rs <- residuals(retweet_wordcount_lm)    
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)


#1a var6 - hash_count
retweet_hashcount_lm <- lm(retweet_count~hash_count, data=df_training)
summary(retweet_hashcount_lm)

pc <- predict(retweet_hashcount_lm,int="c",newdata=df_testing) 
pp <- predict(retweet_hashcount_lm,int="p",newdata=df_testing) 
plot(df_testing$retweet_count, df_testing$hash_count)  
matlines(df_testing$hash_count,pc)  
matlines(df_testing$hash_count,pp)
rs <- residuals(retweet_hashcount_lm)    
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)

#1a var6 - sentiment
retweet_sentiment_lm <- lm(retweet_count~sentiment, data=df_training)
summary(retweet_sentiment_lm)

pc <- predict(retweet_sentiment_lm,int="c",newdata=df_testing) 
pp <- predict(retweet_sentiment_lm,int="p",newdata=df_testing) 
plot(df_testing$retweet_count, df_testing$sentiment) 
matlines(df_testing$sentiment,pc) 
matlines(df_testing$sentiment,pp) 
rs <- residuals(retweet_sentiment_lm)    
qqnorm(rs)    
qqline(rs)
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)






#multivariate
retweet_lm <- lm(retweet_count ~ user_followers_count + user_verified + word_count, data = df_training)
summary(retweet_lm)

retweet.fit <- function(follower_count,user,word_count) retweet_lm$coefficients[1] +
  retweet_lm$coefficients[2]*follower_count + 
  retweet_lm$coefficients[3]*user + 
  retweet_lm$coefficients[4]*word_count

retweet.fit(df_testing$user_followers_count, df_testing$user_verified, df_testing$word_count)

pc <- predict(retweet_lm,int="c",newdata=df_testing) 
pp <- predict(retweet_lm,int="p",newdata=df_testing) 
cor(pc, df_testing$retweet_count)  
cor(pp, df_testing$retweet_count)


#with regularization
library(glmnet) 

cv.fit  <-  
  cv.glmnet(as.matrix(df_training[,c(-1, -7,-8)]),                                                  
            as.vector(df_training[,8]), 
            alpha=1)            
plot(cv.fit)    
coef(cv.fit)

prediction <- predict(cv.fit,                                                   
                      newx=as.matrix(df_testing[,c(-1, -7,-8)]))   
#correlation  or  mean  square  error to  check result  
cor(prediction, as.vector(df_testing[,8]))  

#Answering 3a
#randomizing I think
interest_random <- interest[order(runif(20048)),]

interest_random$retweet_count <- cut(interest_random$retweet_count, 
                                     breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                     labels=paste("retweets", 1:5, sep=""))
interest_random$sentiment <- cut(interest_random$sentiment, 
                                 breaks=c(-1, -0.5, 0, .5,Inf), 
                                 labels=paste("sentiment", 1:4, sep=""))
interest_random$retweet_count <- cut(interest_random$retweet_count, 
                                     breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                     labels=paste("retweet", 1:5, sep=""))
interest_random$user_followers_count <- cut(interest_random$user_followers_count, 
                                            breaks=c(-1, 10, 100, 1000,10000, Inf), 
                                            labels=paste("followers", 1:5, sep=""))


#making trainign and testing sets
testidx <- which(1:nrow(interest_random) %% 4 == 0 )
df_training_random <- interest_random[-testidx,]
df_training_random <- df_training_random[,-1]
df_testing_random <- interest_random[testidx,]
df_testing_random <- df_testing_random[,-1]

#showing that the non random set is the same as the random
summary(interest)
summary(interest_random)
df_training_random$retweet_count<-as.factor(df_training_random$retweet_count)

#3b
library(C50)
tree_model <- C5.0(df_training_random[c(-1,-6,-7)],df_training_random$retweet_count)
summary(tree_model)

tree_predict <- predict(tree_model, df_testing_random)
summary(tree_predict)
library(gmodels)
CrossTable(df_testing_random$retweet_count,tree_predict)


#3c - boosting
tree_model <- C5.0(df_training_random[c(-1,-6,-7)],df_training_random$retweet_count,trials = 10)
summary(tree_model)

tree_predict_10 <- predict(tree_model, df_testing_random)
summary(tree_predict_10)

CrossTable(df_testing_random$retweet_count,tree_predict_10)
#performs better

#3d - bagging
library(randomForest)

df_training_random.new <- droplevels(df_training_random)
bag.likes <- randomForest(retweet_count ~ . ,data=df_training_random.new, 
                          mtry = 9,importance = TRUE) 
pred <- predict(bag.likes, newdata=df_testing_random) 
summary(pred)
CrossTable(df_testing_random$retweet_count, pred)
#changing number of trees - 10
bag.likes <- randomForest(retweet_count ~ . ,data=df_training_random.new, 
                          mtry =  9,importance = TRUE, ntree = 10)  
pred <- predict(bag.likes, newdata=df_testing_random) 
summary(pred)
CrossTable(df_testing_random$retweet_count, pred)
#changing number of trees - 20
bag.likes <- randomForest(retweet_count ~ . ,data=df_training_random.new, 
                          mtry =  9,importance = TRUE, ntree = 20)  
pred <- predict(bag.likes, newdata=df_testing_random) 
summary(pred)
CrossTable(df_testing_random$retweet_count, pred)
#changing number of trees - 30
bag.likes <- randomForest(retweet_count ~ . ,data=df_training_random.new, 
                          mtry =  9,importance = TRUE, ntree = 30)  
pred <- predict(bag.likes, newdata=df_testing_random) 
summary(pred)
CrossTable(df_testing_random$retweet_count, pred)
#changing number of trees - 100
bag.likes <- randomForest(retweet_count ~ . ,data=df_training_random.new, 
                          mtry =  9,importance = TRUE, ntree = 100) 
pred <- predict(bag.likes, newdata=df_testing_random) 
summary(pred)
CrossTable(df_testing_random$retweet_count, pred)
