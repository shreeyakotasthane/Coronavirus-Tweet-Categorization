#Q1 SVM model

fear<-read.csv("bow2.csv")

head(fear)

str(fear)

fear_train <- fear[1:250,]
fear_test <- fear[251:502,]
library(kernlab)
fear_classifier <- ksvm(FCFR~.,data=fear_train,kernel="vanilladot")

fear_classifier

fear_predictions <-predict(fear_classifier,fear_test)
head(fear_predictions)
fear_predictions
#evaluating model performance
agreement <-fear_predictions==fear_test$FCFR
table(agreement)
prop.table(table(agreement))

fear_classifier_rbf <- ksvm(FCFR~.,data=fear_train,kernel="rbfdot")
fear_predictions_rbf <- predict(fear_classifier_rbf,fear_test)
agreement_rbf<-fear_predictions_rbf==fear_test$FCFR
table(agreement_rbf)
prop.table(table(agreement_rbf))

fear_classifier
fear_classifier_rbf

#Q2 Neural Networks

fear<-read.csv("bow_NN.csv")

head(fear)
str(fear)

normalize <- function(x){}

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

fear_norm <- as.data.frame(lapply(fear,normalize))

summary(fear_norm$W__wuhancoronavirus)

summary(fear$W__wuhancoronavirus)

fear_train <- fear_norm[1:375,]
fear_test <- fear_norm[376:501,]

library(neuralnet)

fear_model <- neuralnet(formula=FCFR~W__spread+W__spreadcoronavirus+W__wuhan+W__wuhancoronavirus+W__epidem+W__kill+W__virus+W__warn+W__prevent+W__protect,data=fear_train)

plot(fear_model)

model_results<-compute(fear_model,fear_test[1:10])

model_results

model_results$net.result

model_results$neurons

predicted_FCFR <-model_results$net.result
cor(predicted_FCFR,fear_test$FCFR)

#Q3_1- KMeans Clustering
fear<-read.csv("bow_NN.csv")
View(fear)
str(fear)

#Scatter Plot
plot(W__death~W__epidem,fear )
with(fear,text(W__death~W__epidem,labels=FCFR))

#Q3_2- KMeans Hierarchical
fear <- read.csv("bow.csv")
dataset <- read.csv("bow.csv")
dataset
df <- scale(dataset[-1])
d <- dist(df,method="euclidean")
hfit <- hclust(d,method="ward")
plot(hfit)
grps<-cutree(hfit,k=10)
grps
rect.hclust(hfit, k=10,border = "red")

#Q3_3- Density Based
fear<-read.csv("bow.csv")
str(fear)

library(fpc)
library(dbscan)
if(!require(devtools)) installed.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)

#obtaining optimal eps value
kNNdistplot(fear, k=1)
abline(h=1.41, lty=2)

#Density-based clustering with fpc & dbscan
set.seed(123)
f<- fpc::dbscan(fear,eps=.32, MinPts = 5 )
f

fviz_cluster(f,fear,geom="point")

#Caret-Comparative 
library(caret)
library(kernlab)

fear <- read.csv("bow2_no_RecNo.csv")
#fear.features
intrain <- createDataPartition(y=fear$FCFR,p=.7,list = FALSE )
training <- fear[intrain,]
testing <-fear[-intrain,]

dim(training);
dim(testing);

anyNA(fear)

summary(fear)

training[["FCFR"]]=factor(training[["FCFR"]])
trctrl <-trainControl(method="repeatedcv",number=10,repeats = 3)


svm_Linear <- train(FCFR~.,data=training,method="svmLinear",trControl=trctrl,preProcess=c("center","scale"),tunelength=1)

svm_Linear

