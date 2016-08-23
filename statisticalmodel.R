#set working directory
setwd("C:/Users/AMULGASPAR/Desktop/VijayAnnaProject/Linear")

#install xlsx package to read the datasets
install.packages("openxlsx")
library(openxlsx)

#add the tables here
Client_Bio <- read.xlsx("model.xlsx", sheet=2,startRow=1, colNames=TRUE)
Client_Account <- read.xlsx("model.xlsx",sheet=3,startRow=1,colNames=TRUE)
Client_Flow <- read.xlsx("model.xlsx",sheet=4,startRow=1,colNames=TRUE)
Client_Revenues <- read.xlsx("model.xlsx",sheet=5,startRow=1,colNames=TRUE)

#merging the tables together
Client_Bio_Account <- merge(Client_Bio,Client_Account)

#Right Join
Client_Flow_Revenues <- merge(Client_Flow,Client_Revenues,all.x=TRUE)
OverAll_Details <- merge(Client_Bio_Account,Client_Flow_Revenues,all.x=TRUE)

#Backup the file
a <- OverAll_Details

#Remove NA with Zero's
OverAll_Details[is.na(OverAll_Details)] <- 0

#convert character format into numeric 
a$ActBal_SA<-as.numeric(a$ActBal_SA)
a$ActBal_MF <- as.numeric(a$ActBal_MF)
a$ActBal_OVD <- as.numeric(a$ActBal_OVD)
a$ActBal_CC <- as.numeric(a$ActBal_CC)
a$ActBal_CL <- as.numeric(a$ActBal_CL)
a$Count_CL <- as.numeric(a$Count_CL)

#Roundup the numbers 
a$ActBal_CA<- round(a$ActBal_CA,0)
a$ActBal_SA <- round(a$ActBal_SA,0)
a$ActBal_MF <- round(a$ActBal_MF,0)
a$ActBal_OVD <- round(a$ActBal_OVD,0)
a$ActBal_CC <- round(a$ActBal_CC,0)
a$ActBal_CL <- round(a$ActBal_CL,0)


#Copy the output in newfile 
write.csv(OverAll_Details,file="a.csv",sep=",")

#Linear models
fit1 <- lm( Count_CL ~ ., data=a)
summary(fit1)

fit2 <- lm( Sale_CL ~ ., data=a)
summary(fit2)

fit3 <- lm(Sale_CL ~ Age + Tenure +  Count_CC + ActBal_SA + ActBal_OVD + ActBal_CC
           + TransactionsCred_CA + Revenue_MF + Revenue_CL, data=a)
summary(fit3)

qqnorm(fit2$residuals)
qqline(fit2$residuals)

#Split the data's into train and test
train <- a[1:1110,]
test <- a[1110:1615,]


#make the decision tree based on the predictions
library(rpart)
tree<-rpart(Sale_CL ~ .,
            data=train,method='class' )
plot(tree)

#Display the results
printcp(tree)

#Visualize cross-validation results
plotcp(tree)

#detailed summary of spilts
summary(tree)

# tree package
library(tree)
tr= tree(Sale_CL~.,data=a)


#Tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Fancied tree
fancyRpartPlot(tree)

#random forest 
install.packages("randomForest")
library(randomForest)

#train and test data 
str(train)
str(test)

#set for reproduceability
set.seed(111)

#apply the random forest algorithm here
prediction <- randomForest(Sale_CL~ Tenure+ActBal_CA+ActBal_SA+ActBal_CL+VolumeDeb_CA , 
                           data=train, importance=TRUE, ntree=10)

#make the prediction using test data
pred <- predict(prediction,test)

#create the data frame
data <- data.frame(Clientid = test$Client, solution= pred)

# to find the important categories in the data
varImpPlot(prediction)

