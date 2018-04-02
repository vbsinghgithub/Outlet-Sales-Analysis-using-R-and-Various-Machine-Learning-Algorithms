Train_market<- read.csv("E:/vit pdf/master thesis/Train_market.csv")

View(Train_market)

Test_market<- read.csv("E:/vit pdf/master thesis/Test_market.csv")

View(Test_market)

dim(Train_market)
 
dim(Test_market)

str(Train_market)

#After run, we can see that the data Train_market has integer,numeric and string(factor) variables.

summary(Train_market) 

# or

table(is.na(Train_market))

colSums(is.na(Train_market))


#After running this command, We can easily observe that the Item_Weight ,Outlet_Size has NA values.  
#install library ggplot2

library(ggplot2)
ggplot(Train_market,aes(x=Item_Visibility,y=Item_Outlet_Sales))+geom_point(size=1,color="maroon")+xlab("Item visibility")+ylab("Item outlet sales")+ggtitle("Item visibility vs Item outlet sales")+theme_bw()

#As we can see item visibility less than 0.2 have majority of sales and visibility more than 0.2 have minority of sales. 

ggplot(Train_market,aes(Outlet_Identifier,Item_Outlet_Sales))+geom_histogram(stat="identity",color="blue")+ggtitle("outlet identifier vs outlet sales")+theme_bw()

#As from the above figure we can analyze that the OUT027 contributes most to the total sales followed by OUT35 and so on.

ggplot(Train_market, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity",col="violet")+theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "orange")) +xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

#From this graph, we can infer that Fruits and Vegetables contribute to the highest amount of outlet sales followed by
#snack foods and household products.

ggplot(Train_market, aes(Item_MRP, Item_Outlet_Sales)) + geom_bar( stat = "identity",col="maroon")+theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "orange")) +xlab("Item MRP") + ylab("Item Outlet Sales")+ggtitle("Item MRP vs Sales")

# we can see that if mrp increases sales also increases.

ggplot(Train_market,aes(Outlet_Identifier,Item_Weight))+geom_boxplot(color="maroon")+theme(axis.text.x = element_text(angle=70,vjust=0.5,color="maroon"))+xlab("Outlet_Identifier")+ylab("Item_Weight")+ggtitle("item weight vs outlet identifier")


#As it is clear from above diagram that outlet out019 and out0027 does not have any item weight,so we have to impute those missing values of item weight for the two outlets 0027 and 0019.

ggplot(Train_market,aes(Item_Type,Item_MRP))+geom_boxplot(color="maroon")+theme(axis.text.x = element_text(angle=70,vjust=0.5,color="maroon"))+xlab("Item_Type")+ylab("Item_MRP")+ggtitle("item Type vs item mrp")

#If we go through the train and test data we get that the test data has 11 columns whereas train data has 12 columns.

#So,we have to add one extra column with value 0ne(1) in test dataframe that is missing.

Test_market$Item_Outlet_Sales<-1
View(Test_market)

#Inorder to simplify I am going to combine the Train_market and Test_marketdataframes.

combine<-rbind(Train_market,Test_market)
View(combine)

#So,we can see that only Item_Weight has NA values,after imputing in column Outlet_Size.


combine$Item_Weight[is.na(combine$Item_Weight)]<-median(combine$Item_Weight[!is.na(combine$Item_Weight)])

#So,here I imputed the missing values in column item weight by median of non missing values.
table(is.na(combine$Item_Weight))

#FALSE 
#14204

table(is.na(combine))


levels(combine$Item_Fat_Content)
#[1] "LF"      "low fat" "Low Fat" "reg"     "Regular"

library("plyr", lib.loc="~/R/win-library/3.4")

combine$Item_Fat_Content<-revalue(combine$Item_Fat_Content,c("LF"="Low Fat" ,"low fat"="Low Fat","reg"="Regular"))
table(combine$Item_Fat_Content)

#If,we go through the data again,we will see  that the item visibility column has value "0",which is not
#Feasible.so,we have to impute median value for all the zeroes in item visibility.

combine$Item_Visibility<-ifelse(combine$Item_Visibility==0,median(combine$Item_Visibility),combine$Item_Visibility)

#Now, if we look into the combine data we can see that the column outlet size has NA(empty) values.so,for now i am going to impute "any" for the empty cells.

levels(combine$Outlet_Size)[1]<-"any"

#Here I am going to create a dataframe fatcount which will show the fat level for each item type.

fatcount<-as.data.frame(setNames(aggregate(combine$Item_Fat_Content,by=list(Category=combine$Item_Type,Category=combine$Item_Fat_Content),FUN=length),c("Item_Type","Item_Fat_Content","total")))
View(fatcount)

#After running fatcount we will able to analyze that the item type like "household", "others","health & hygiene" also has fat level like low fat,regular etc, which makes no meaning to them.so, I am going to introduce a new fat level category "none" for these item types.

levels(combine$Item_Fat_Content) <- c(levels(combine$Item_Fat_Content), "None")

combine[ which(combine$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content<- "None"

combine[ which(combine$Item_Type == "Household") ,]$Item_Fat_Content<- "None"

combine[ which(combine$Item_Type == "Others") ,]$Item_Fat_Content<- "None"

combine$Item_Fat_Content<-factor(combine$Item_Fat_Content)

 
fatcount<-as.data.frame(setNames(aggregate(combine$Item_Fat_Content,by=list(Cateogry=combine$Item_Type,Cateogry=combine$Item_Fat_Content),FUN=length),c("Item_Type","Item_Fat_Content","total")))

View(fatcount)

#Or,we can apply Knn algorithm to fill the missing cells in the column outlet size.


#library("VIM", lib.loc="~/R/win-library/3.4")

#kNN(Train_market,variable=c("Outlet_Size"),k=6)

#kNN(combine,variable=c("Outlet_Size"),k=6)->KNN

#summary(kNN)

#colSums(is.na(combine))

#colSums(is.na(combine))


library(plyr)

library(dplyr)

#dplyr package makes data manipulation quite effortless

#count of outlet identifiers There are 10 unique outlets in this data. This variable will give us information on
#count of outlets in the data set. More the number of counts of an outlet, chances are more will be the sales
#contributed by it.


a <-combine%>%group_by(Outlet_Identifier)%>%tally()

names(a)[2] <-"Outlet_Count"
head(a)

combine <- full_join(a, combine, by ="Outlet_Identifier")

summary(combine$Outlet_Identifier)

#count of item identifier This will help us to understand, which outlet has
#maximum frequency

cnt<-combine%>%group_by(Item_Identifier)%>%tally()

names(cnt)[2]<-"item_count"

head(cnt)

combine<-merge(cnt,combine,by="Item_Identifier")

  #Below code will count the age in year of every outlet 

combine$year<-as.factor(2017-combine$Outlet_Establishment_Year)

#we'll discover, items corresponding to "DR", are mostly eatables. Items corresponding to
#"FD", are drinks. And, item corresponding to "NC", are products which can't be consumed,

#Here I'll use substr(), gsub() function to extract and rename the variables respectively. 

newtype<-substr(combine$Item_Identifier,1,2)
newtype<-gsub("FD","Food",newtype)
newtype<-gsub("DR","Drinks",newtype)
newtype<- gsub("NC","NonConsumable",newtype)
table(newtype)

#Let's now add this information in our data set with a variable name 'Item_Type_New.

combine$Item_Type_New <-newtype

#drop variables not required in modeling >
library(dplyr) 
combine <- select(combine, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year)) 

#divide data set >
new_train_market <- combine[1:nrow(Train_market),] 
new_test_market <- combine[- (1:nrow(Train_market)),] 
#linear regression > 
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train_market) 
summary(linear_model) 

library(rpart) 
library(e1071) 
library(rpart.plot) 
library(caret)
install.packages("Metrics") 
library(Metrics)

rmse(new_train_market$Item_Outlet_Sales, exp(linear_model$fitted.values)) 

linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train_market) 
summary(linear_model) 


rmse(new_train_market$Item_Outlet_Sales, exp(linear_model$fitted.values)) 


#kmeans algorithm

newframe<-data.frame(combine$item_count,combine$Outlet_Count,combine$Item_Weight,combine$Item_Visibility,combine$Outlet_Type)
copy<-newframe
copy$combine.Outlet_Type<-NULL
pred<-kmeans(copy,3)
View(pred)
pred

table(newframe$combine.Outlet_Type,pred$cluster)

#                     1    2    3
#Grocery Store        0 1805    0
#Supermarket Type1 1553    0 7741
#Supermarket Type2    0    0 1546
#Supermarket Type3 1559    0    0

#linear regression

ggplot(new_train_market,aes(Item_MRP,Item_Outlet_Sales))+geom_point(col="maroon")+ylim(0,5000)+xlim(30,100)

linear_model1<-lm(new_train_market$Item_Outlet_Sales~new_train_market$Item_MRP)
summary(linear_model1)


#ggplot(combine,aes(Item_Visibility,Item_Outlet_Sales))+geom_point(col="maroon")+ylim(0,1500)+xlim(0,0.4)

#model3<-lm(combine$Item_Outlet_Sales~combine$Item_Visibility)
#summary(model3)

#prediction1<-predict(model3)
#prediction1

#multiple regression

mult_reg<-lm(combine$Item_Outlet_Sales~combine$Item_Weight+combine$Item_MRP,data=combine)
summary(mult_reg)

#loading required libraries > 
 
#setting the tree control parameters > 
fit <- trainControl(method = "cv", number = 5) 
cart_Grid <- expand.grid(.cp= (1:50)*0.01) 
#decision tree >
tree_model <- train(Item_Outlet_Sales ~ ., data = new_train_market, method = "rpart", trControl = fit, tuneGrid = cart_Grid) 
print(tree_model) 

main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train_market, control = rpart.control(cp=0.01)) 
prp(main_tree)

pre_score <- predict(main_tree, type = "vector") 
rmse(new_train_market$Item_Outlet_Sales, pre_score) 


library(ggplot2)
library(gridExtra)
library(dplyr)
library(corrplot)
library(pROC)
library(C50)
library(caret)
library(rpart)

naive<-data.frame(new_train_market$item_count,new_train_market$Outlet_Count,new_train_market$Item_Weight,new_train_market$Item_Visibility,new_train_market$Item_MRP,new_train_market$Item_Outlet_Sales,new_train_market$year,new_train_market$Item_Fat_Content)
View(naive)
str(naive)
naive$new_train_market.Item_Fat_Content<-ifelse(naive$new_train_market.Item_Fat_Content=="Regular",1,0)
library("psych", lib.loc="~/R/win-library/3.4")

pairs.panels(naive[-1])
pairs.panels(naive[-5])
set.seed(1234)
vid<-sample(2,nrow(naive),replace=T,prob = c(0.8,0.2))
train<-naive[vid==1,]
test<-naive[vid==2,]
naive$new_train_market.Item_Fat_Content<-as.factor(naive$new_train_market.Item_Fat_Content)
model<-naive_bayes(new_train_market.Item_Fat_Content~.,data=naive)
model
predic<-predict(model,train,type='prob')
head(cbind(predic,train))
predic1<-predict(model,train)
(tab1<-table(predic1,train$new_train_market.Item_Fat_Content))

1-sum(diag(tab1))/sum(tab1)

predic2<-predict(model,test)

 (tab2<-table(predic2,test$new_train_market.Item_Fat_Content))

#predic2   0   1
#       0 997 686
#       1   4   4
1-sum(diag(tab2))/sum(tab2)
#[1] 0.4080426
