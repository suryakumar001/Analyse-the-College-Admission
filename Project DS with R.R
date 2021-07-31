library(boot) 
library(car)
install.packages("car")
install.packages("QuantPsyc")
library(QuantPsyc)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
install.packages("vars")
library(vars)
install.packages("nortest")
library(nortest)
install.packages("MASS")
library(MASS)

setwd("C:\\Users\\Mohit\\Downloads")

data<- read.csv("college_admission.csv")
data2<-data
str(data) #check the structure of the data
summary(data) #check the total summary of the data

head(data) # Heading with Top 6 Rows of Data

nrow(data) #Count the number's of row
# Q.1 Find the missing values. (if any, perform missing value treatment)

sapply(data, function(x) sum(is.na(x))) #Check the NA and NULL Values in Data

data <- na.omit(data) #Remove NA or NULL Value Rows 
data


#Q.2 Find outliers (if any, then perform outlier treatment)

# 2.1 Outlier of gre column
Outlier_gre <- IQR(data$gre)
Outlier_gre

Quarters1 <- quantile(data$gre , na.rm = TRUE) # View the dataset in 4 equal quaters/parts, and if there are any NA/NULL values present then remove those values
Quarters1

Outlier_gre_top <- 660+(1.5*Outlier_gre) #To find out the Maximum value of outlier [Q3+(1.5*IQR)]
Outlier_gre_top

Outlier_gre_bottom <- 520-(1.5*Outlier_gre) #To find out the Minimum value of outlier  [Q1-(1.5*IQR)]
Outlier_gre_bottom


print(which(data$gre > Outlier_gre_top)) #To find the number of values above Maximum outlier value 

print(which(data$gre < Outlier_gre_bottom)) #TO find the number of values below Minimum outlier value

# Treatment of Outlier // Removing the Outlier

boxplot(data$gre) # GRE Box plot before removing outlier
data <- data[-c(72,180,305,316),] #We are removing the row , which contain outlier value
nrow(data)
boxplot(data$gre) # GRE Box plot after removing outlier

# 2.2 Outlier of gpa column

Outlier_gpa <- IQR(data$gpa)
Outlier_gpa

Quarters2 <- quantile(data$gpa , na.rm = TRUE) # To View the dataset in 4 equal quaters/parts, and if there are any NA/NULL values present then remove those values
Quarters2

Outlier_gpa_top <- 3.67+(1.5*Outlier_gpa) #To find out the Maximum value of outlier [Q3+(1.5*IQR)]
Outlier_gpa_top

Outlier_gpa_bottom <- 3.13-(1.5*Outlier_gpa) #To find out the Minimum value of outlier  [Q1-(1.5*IQR)]
Outlier_gpa_bottom


print(which(data$gpa > Outlier_gpa_top)) #To find the number of values above Maximum outlier value 

print(which(data$gpa < Outlier_gpa_bottom)) #TO find the number of values below Minimum outlier value

#Treatment of Outlier // Remove outlier 

boxplot(data$gpa) # GPA Box plot before removing outlier
data <- data[-c(286),] # Removing the row , which contain outlier value
nrow(data)
boxplot(data$gpa) # GPA Box plot after removing outlier

# Q.3 Find the structure of the data set and if required, transform the numeric data type to factor and vice-versa.

# Convert Class to Factor from Integer  

str(data) #to check the stracture and leveles of dataset

sapply(data, class) #Check the Class of the Overall Data

data_Columns <- c("admit", "ses", "Gender_Male", "Race", "rank") #Define the heading in a new vector

data[data_Columns] <- lapply(data[data_Columns], factor) #Convert the Integer into facor # Categorical variables

sapply(data, class) #Check the class again

str(data) #to check the stracture and levels of dataset


# Q.4 Find whether the data is normally distributed or not. Use the plot to determine the same. 

summary(data$gre) #Data is Not normally distributed : mean (591.2) > median (580.0), right skewness
hist(data$gre, breaks=20, col="yellow", density = 75 , xlab= 'GRE Score', ylab = "Density",  main =  "Distribution of GRE Score") # Visual inspection
?hist

summary(data$gpa) #Data is Not normally distributed : median (3.4) > mean (3.398), left skewness
hist(data$gpa, breaks=20, col="yellow", density = 75 , xlab= 'GPA ', ylab = "Density", main =  "Distribution of GPA ")

# Q.5 Normalize the data if not normally distributed.

# Normalization using  scale function

Scale_GRE <- scale(data$gre)


#Creating histo graph for visualization

hist(Scale_GRE, breaks=20, col="green", density = 75 , xlab= 'GRE Score', ylab = "Density",  main =  "Distribution of GRE Score after Scalling") 

Scale_GPA <- scale(data$gpa)

#Creating histo graph for visualization

hist(Scale_GPA, breaks=20, col="blue", density = 75 , xlab= 'GPA ', ylab = "Density",  main =  "Distribution of GPA after Scalling")


# Q.6 Use variable reduction techniques to identify significant variables.

#Splitting the data into Training and Testing part
set.seed(250)
library(caTools) # for splitting the data
data[,2:3] <- scale(data[,2:3]) #Scalling #Selecting the gre and gpa column

split <- sample.split(data$admit, SplitRatio = .70) #splitting
training <- subset(data, split== TRUE) #Training Part
testing <- subset(data, split== FALSE) #Testing Part


# Q.7 Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables) 

#Logistics Regression Model

LR_model <- glm(admit ~ ., data = training,family = "binomial")
summary(LR_model)


#Used logistic regression model to check the important variables based on the P-value. <.05
# Found gre, and rank as important variables.

# Using Factor that Influence admission process, removed all the variables except gre, and rank

LR_model = glm(admit ~ I(rank == 4) + I(rank == 3) +gre , data = training,family = "binomial")
summary(LR_model)

# Q.8 Calculate the accuracy of the model and run validation techniques.

#In LR Residual deviance Increased from : 311.01 to 320.42, So now I have applied prediction model

pred = predict(LR_model,testing, type="response")
pred = ifelse(pred>0.5,1,0)
pred = factor(pred)

# Validation of the prediction model using Confusion Matrix

act <- testing$admit
# Accuracy of Logistics Model
install.packages("caret")
library(caret) # For calculation 
table(pred, act)
C_lr=confusionMatrix(pred,act)
C_lr # Accuracy is 64.96% for logistic regression

# Q.9 Try other modelling techniques like decision tree and SVM and select a champion model 

#Analysis using Support Vector machine // SVM Model


library(caret)
install.packages("e1071")
library(e1071)

svmfit =svm(admit ~ gre+ I(rank == 4) + I(rank == 3), data = training, kernel="linear",
            scale = T)
print(svmfit)
pred_svm = predict(svmfit,testing, type="response")
table(pred_svm, act)
a_svm=confusionMatrix(pred_svm,act)
a_svm # Accuracy is 67.52% for SVM



# Q.10 Determine the accuracy rates for each kind of model 

#K- NEAREST NEIGHBOR ALGORITHM (KNN Model) 

library(class) 
classifier_knn <- knn(train = training, 
                      test = testing, 
                      cl = training$admit, 
                      k = 1) 
classifier_knn 
misClassError <- mean(classifier_knn != testing$admit) 
print(paste('Accuracy =', 1-misClassError)) 

# Confusiin Matrix 
cm <- table(testing$admit, classifier_knn) 
cm 

# K = 5 
classifier_knn <- knn(train = training, 
                      test = testing, 
                      cl = training$admit, 
                      k = 5) 
misClassError <- mean(classifier_knn != testing$admit) 
print(paste('Accuracy =', 1-misClassError)) 
## Accuracy for KNN is 93.16%

# Q.11 Select the most accurate model 

#Model	Accuracy 
#KNN Model	93.16%
#Logistics Regression Model	64.96%
#Support Vector Machine	67.52%


# Q.12 Identify other Machine learning or statistical techniques

# Information Gain and Entropy

library(rpart)
library(caret)
install.packages("dplyr")
library(dplyr)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("data.tree")
library(data.tree)
library(caTools)


Admission_tree <- rpart(admit~.,data = training ) 

tree_view <- predict(Admission_tree, testing, type = "class")

#confusionMatrix

tc <- confusionMatrix(tree_view, testing$admit)
tc # Accuracy is 66.67%
prp(Admission_tree)
?prp



#Descriptive: 
# Q.1 Categorize the average of grade point into High, Medium, and Low (with admission probability percentages) and plot it on a point chart.  



#1.1 - Categorize the grade point average into High or Medium and plot it on point chart.

Aptitute_Descriptive = transform(data2,GreLevels=ifelse(gre<440,"Low",ifelse(gre<580,"Medium","High")))

str(Aptitute_Descriptive)

Sum_Apt=aggregate(admit~GreLevels,Aptitute_Descriptive,FUN=sum)
?aggregate
length_Apt=aggregate(admit~GreLevels,Aptitute_Descriptive,FUN=length)
Probability_Table = cbind(Sum_Apt,Recs=length_Apt[,2])
Probability_Table_final = transform(Probability_Table,Probability_Admission = admit/Recs)
Probability_Table_final

install.packages("ggplot2")
library(ggplot2)
Plot1 = ggplot(Probability_Table_final,aes(x=GreLevels,y=Probability_Admission))+geom_point()
Plot1

# 1.2: Cross grid for admission variable with GRE categorized

table(Aptitute_Descriptive$admit,Aptitute_Descriptive$GreLevels)
