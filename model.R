library(caret)


data<-read.csv("data_input/data.csv")
data$diagnosis<-sapply(as.character(data$diagnosis), switch, "M"="Malignant","B"="Benign")
data$diagnosis<-as.factor(data$diagnosis)

prop.table(table(data$diagnosis))

#splitdata
set.seed(1234)
dataIndex<-createDataPartition(data$diagnosis,p=0.8,list = FALSE)
train_data <- data[dataIndex,-1]
test_data <- data[-dataIndex,]

data.submit<-read.csv("data_input/test.csv")
data.submit$diagnosis<-sapply(as.character(data.submit$diagnosis), switch, "M"="Malignant","B"="Benign")
data.submit$diagnosis<-as.factor(data.submit$diagnosis)


#3 Model 

#KNN
model_knn<-train(diagnosis~., data = train_data, method='knn')
pred_knn<-predict(model_knn,test_data[,-1])
confusionMatrix(test_data$diagnosis,pred_knn)
save(model_knn,file = "model_knn.rda")


dataBaru<-test_data[,c(1:2)]
dataBaru$Predict<-pred_knn
library(ROCR)


#test submit
pred_knn<-predict(model_knn,data.submit)
confusionMatrix(data.submit$diagnosis,pred_knn)




#Random forest
library(ranger)
model_rf<-train(diagnosis~., data = train_data, method='ranger')
pred_rf<-predict(model_rf,test_data[,-1])
#perbandingan<-confusionMatrix(test_data$diagnosis,pred_rf)
save(model_rf,file="model_rf.rda")


data_baru<-as.data.frame()
perbandingan<-table(pred_rf,test_data$diagnosis)
perbandingan

Recall<-round((perbandingan[2,2]/sum(perbandingan[,2]))*100,2)
Accuracy<-round((perbandingan[1,1]+perbandingan[2,2])/sum(perbandingan)*100,2)
Precision<-round((perbandingan[2,2]/sum(perbandingan[2,]))*100,2)
Specificity<-round((perbandingan[1,1]/sum(perbandingan[,1]))*100,2)

cat('\n Model Performance Modifikasi :\n\n','Accuracy \t:',Accuracy,'%\n','Recall \t:',Recall,'%\n','Precision \t:',Precision,'%\n','Specificity \t:',Specificity,"%\n")




#test submit
pred_rf<-predict(model_rf,data.submit[,-1])
confusionMatrix(data.submit$diagnosis,pred_rf)


#Logistic regresion
model_lr<-glm(diagnosis~., data = train_data, family = "binomial")
pred_lr<-predict(model_lr,test_data[,-1])
confusionMatrix(test_data$diagnosis,pred_lr)

library(partykit)
model_dt<-ctree(diagnosis~. , data = train_data)
save(model_dt,file="model_dt.rda")
plot(model_dt)
pred_dt<-predict(model_dt, test_data[,-1])
confusionMatrix(pred_dt,test_data$diagnosis)
