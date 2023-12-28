# Project Title: Admitted Vs Enrolled students for three North American Colleges - Analysis
# The purpose of this script is to analyze and model the college admissions process.
# We aim to understand the factors influencing admission decisions in this model, using logistic regression and other statistical techniques.

#Install and load necessary packages
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")


# Load the necessary libraries
library(caTools)
library(dplyr)
library("readxl")
library("ROSE")
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)

#Import the data from the College_Admissions worksheet and label it as mydata
mydata<-read.csv("College_Admissions.csv")

#Variables in dataset - Applicant,Edu_Parent1,	Edu_Parent2,	Sex,	White,	Asian	
#                       HSGPA,	SAT_ACT,	College,	Admitted,	Enrolled,	College_GPA

#  performing Summary statistics on the data file
summary(mydata) 
str(mydata)
View(mydata)
# the summary statistics and the structure type tells us that the categorical variables in the data file are marked as numeric and character type



# Defining the columns and groups to compute means
columns <- c("Sex", "White", "Asian", "College", "Admitted", "Enrolled")
variables <- c("Edu_Parent1", "Edu_Parent2", "Sex", "White", "Asian", "College", "Admitted", "Enrolled")

# generating the covaraince matrix between HSGPA V/S SAT Scores
cov_matrix <- cov(mydata[, 7:8]);cov_matrix

# Standard deviation HSGPA and SAT
sd_HSGPA <- sd(mydata$HSGPA);sd_HSGPA
sd_SAT <- sd(mydata$SAT_ACT); sd_SAT

# Correlation coefficient between HSGPA V/S SAT Scores
correlation_coeff <- cor(mydata[,7:8]);correlation_coeff
#the correlation coefficient for both variables are less than 0.8, so they are not strongly positively related and can be further used in our analysis

# Performing HSGPA mean for all categories 
for (col in columns) {
  result <- tapply(mydata$HSGPA, mydata[, col], mean)
  cat("HSGPA category mean for",col,":\n")
  print(result)
}
#This provides us the mean HSGPA based on the different categories among the data set
# The HSGPA mean of students who have applied to the the math & science college is higher than the other colleges
# The HSGPA mean of students who were offered admission have a higher mean and thus, can be infered that students with
# a higher HSGPA, have a higher chance to get an admission
#Performing SAT scores mean for all categories
for (col in columns) {
  result <- tapply(mydata$SAT_ACT, mydata[, col], mean)
  cat("SAT_ACT category mean for",col,":\n")
  print(result)
}
#This provides us the mean SAT_ACT based on the different categories among the data set
# The SAT_ACT mean of students who have applied to the the math & science college is higher than the other colleges
# The SAT_ACT mean of students who were offered admission have a higher mean and thus, can be infered that students with
# a higher SAT_ACT, have a higher chance to get an admission
#Performing SAT scores mean for all categories

# frequency and relative frequency table for all categories
for (var in variables) {
  freq <- table(mydata[, var])
  relfreq <- prop.table(freq)
  cat("Frequency Table for", var, ":\n")
  print(freq)
  cat("Relative Frequencies for", var, ":\n")
  print(relfreq)
  barplot(freq, main = paste("Bar chart for", var), ylab = "Number of Students", col = "blue")
  abline(h = 0)
}
# Majority of parents of students have a education score of 6,7 referring to 4 years of college and postgraduation
# This tells us that the students come from a high educational background
# Also, the proportion of female students is way higher than that of male students 

#contingency table for college category and sex,admission and enrollment
sex_collegetable <- table(mydata$Sex,mydata$College);sex_collegetable
mytable<-table(mydata$White,mydata$College);mytable
adm_collegetable<-table(mydata$Admitted,mydata$College);adm_collegetable
enr_collegetable<-table(mydata$Enrolled,mydata$College);enr_collegetable
# A majority of female students prefer arts & letters where as a majority of male students prefer Math and science as 
# their graduate option
# out of the total number of applications for math & science around 50% were provided with an admission 
# where as the acceptance rate for arts and letter was mere 25% and for business and economics it was 35%
# this makes the arts and letters college even competitive 

#plots
#box plots
boxplot(mydata$HSGPA ~ mydata$Sex, xlab = "Sex", ylab = "HSGPA",main= "Box plot of HSGPA vs Sex")
boxplot(mydata$SAT_ACT ~ mydata$Sex, xlab = "Sex", ylab = "SAT_ACT",main= "Box plot of SAT_ACT vs Sex")
boxplot(mydata$HSGPA ~ mydata$White, xlab = "Non-White/White", ylab = "HSGPA",main= "Box plot of HSGPA vs White")
boxplot(mydata$SAT_ACT ~ mydata$White, xlab = "Non-White/White", ylab = "SAT_ACT",main= "Box plot of SAT_ACT vs White")

#Now, we plot the box plots to a more detailed estimation in terms of the middle 50 quantiles
# Majority of the students have HSGPA scores between 3-4 with only a few outliers
# Majority of the students have SAT scores between 1000-1400 with only a few outliers

#stacked barplots
barplot(sex_collegetable, col=c('blue','red'), legend=rownames(sex_collegetable), xlab="College",ylab="Sex",main= "Stacked Bar plot of College and Sex")
barplot(adm_collegetable, col=c('blue','red'), legend=rownames(adm_collegetable), xlab="College",ylab="Admitted",ylim=c(0,7000),main= "Stacked Bar plot of College and Admitted")
barplot(enr_collegetable, col=c('blue','red'), legend=rownames(enr_collegetable), xlab="College",ylab="Enrolled",ylim=c(0,7000),main= "Stacked Bar plot of College and Enrolled")
# stacked plots can help us understand the categories even better 

#histogram 
hist(mydata$HSGPA, right=TRUE, main="Histogram for the HSGPA", xlab="HSGPA", col="blue")
hist(mydata$SAT_ACT, right=TRUE, main="Histogram for the SAT", xlab="HSGPA", col="red")
#both the histogram are negatively skewed showing that the majoirty of our dataset lies on the right side of the graph

#scatterplot
plot(mydata$HSGPA ~ mydata$SAT_ACT, main ="Scatterplot of HSGPA vs. SAT_ACT", xlab = "SAT_ACT", ylab = "HSGPA", pch=1)
plot(mydata$HSGPA ~ mydata$SAT_ACT, main="Scatterplot of HSGPA vs. SAT_ACT (Asian)", xlab = "SAT_ACT", ylab = "HSGPA", pch=1,col=ifelse(mydata$Asian == 0, 20, 26));legend("bottomleft", legend=c("Non-Asian", "Asian"), pch=1, col=c(20, 26))
plot(mydata$HSGPA ~ mydata$SAT_ACT, main="Scatterplot of HSGPA vs. SAT_ACT (White)", xlab = "SAT_ACT", ylab = "HSGPA", pch=1,col=ifelse(mydata$White == 0, 20, 26));legend("bottomleft", legend=c("Non-White", "White"), pch=1, col=c(20, 26))

# After getting some basic insights on the dataset through tables and graphs, we will try to understand more
# about the dataset using logistic regression and decision tree in detain.
# The following models are prepared:
# 1. Logistic regression model for the classification of a student granted admission or not
# 2. Logistic regression model for the classification of a student granted admission, chooses to enroll or not 
# 3. CART model for the classification of a student granted admission or not
# 4. CART model for the classification of a student granted admission, chooses to enroll or not 


#Model development 

# 1.Model for admission classification using logistic regression  
    # Creating a numeric variable for Sex (1 for Male, 0 for Female)
    mydata$sexNumeric <- ifelse(mydata$Sex == "M", 1, 0)
    
    # Creating a numeric variable for Admitted (1 for Yes, 0 for No)
    mydata$AdmittedNumeric <- ifelse(mydata$Admitted == "Yes", 1, 0)
    
    #View(mydata)
    # Setting options to avoid scientific notation
    options(scipen=999)
    
    
    # Model 1 using all the attributes
    Logistic_Model1 <- glm(AdmittedNumeric ~ Edu_Parent1 + Edu_Parent2 + sexNumeric + White + Asian + HSGPA + SAT_ACT, family = binomial, data = mydata)
    summary(Logistic_Model1)
    
    pHat1 <- predict(Logistic_Model1, mydata, type = "response")
    yHat1 <- ifelse(pHat1 >= 0.5, 1,0)
    
    yHat1_factor <- factor(yHat1, levels = c(0, 1))
    AdmittedNumeric_factor1 <- factor(mydata$AdmittedNumeric, levels = c(0, 1))
    
    # Computing the confusion matrix
    confMatrix1 <- confusionMatrix(yHat1_factor, AdmittedNumeric_factor1)
    confMatrix1
    # Extract sensitivity and specificity
    accuracy1 <- 100*mean(mydata$AdmittedNumeric == yHat1)
    sensitivity1 <- confMatrix1$byClass["Sensitivity"]*100
    specificity1 <- confMatrix1$byClass["Specificity"]*100
    sprintf("Accuracy measure for model1 = %f",100*mean(mydata$AdmittedNumeric == yHat1))
    sprintf("Sensitivity for Model1 = %f",sensitivity1)
    sprintf("Specificity for Model1 = %f",specificity1)
    
    
    #In the above model, the p value of sexNumeric and Asian is too large than 0.05, so they are not significant to the model and we 
    #exclude these and develop a new model
    #sexNumeric: This has a p-value of 0.966109, which is much higher than the usual significance level of 0.05. This suggests that sexNumeric is not statistically significant in predicting whether a student will be admitted or not.
    #Asian: The p-value for Asian is 0.311399, which is also higher than 0.05. This indicates that the Asian variable is not providing a statistically significant contribution to the model.
    
    # Model 2 excluding the above attributes
    Logistic_Model2 <- glm(AdmittedNumeric ~ Edu_Parent1 + Edu_Parent2 + White + HSGPA + SAT_ACT, family = binomial, data = mydata)
    summary(Logistic_Model2)
    
    pHat2 <- predict(Logistic_Model2, mydata, type = "response")
    yHat2 <- ifelse(pHat2 >= 0.5, 1,0)
    
    yHat2_factor <- factor(yHat2, levels = c(0, 1))
    AdmittedNumeric_factor2 <- factor(mydata$AdmittedNumeric, levels = c(0, 1))
    
    # Computing the confusion matrix
    confMatrix2 <- confusionMatrix(yHat2_factor, AdmittedNumeric_factor2)
    confMatrix2
    # Extract sensitivity and specificity
    accuracy2 <- 100*mean(mydata$AdmittedNumeric == yHat2)
    sensitivity2 <- confMatrix2$byClass["Sensitivity"]*100
    specificity2 <- confMatrix2$byClass["Specificity"]*100
    
    sprintf("Accuracy measure for model3 = %f",100*mean(mydata$AdmittedNumeric == yHat2))
    sprintf("Sensitivity for Model2 = %f",sensitivity2)
    sprintf("Specificity for Model2 = %f",specificity2)
    
    # Since white is also binary variable and p-value of white is 0.000175 is higher here compared to other variables, building a model excluding white and testing the model.
    # Model 3
    Logistic_Model3 <- glm(AdmittedNumeric ~ Edu_Parent1 + Edu_Parent2 + HSGPA + SAT_ACT, family = binomial, data = mydata)
    summary(Logistic_Model3)
    
    pHat3 <- predict(Logistic_Model3, mydata, type = "response")
    yHat3 <- ifelse(pHat3 >= 0.5, 1,0)
    
    yHat3_factor <- factor(yHat3, levels = c(0, 1))
    AdmittedNumeric_factor3 <- factor(mydata$AdmittedNumeric, levels = c(0, 1))
    
    # Computing the confusion matrix
    confMatrix3 <- confusionMatrix(yHat3_factor, AdmittedNumeric_factor3)
    confMatrix3
    # Extract sensitivity and specificity
    accuracy3 <- 100*mean(mydata$AdmittedNumeric == yHat3)
    sensitivity3 <- confMatrix3$byClass["Sensitivity"]*100
    specificity3 <- confMatrix3$byClass["Specificity"]*100
    
    sprintf("Accuracy measure for model3 = %f",100*mean(mydata$AdmittedNumeric == yHat3))
    sprintf("Sensitivity for Model3 = %f",sensitivity3)
    sprintf("Specificity for Model3 = %f",specificity3)
    # The accuracy for this model is better than previous one ie 81.51% whereas previous model had accuracy of 81.48%
    # So we will be going ahead with variables - Edu_Parent1, Edu_Parent2, HSGPA, SAT_ACT
    # Logistic_Model3 is our final model
    # Creating a data partition to split the data into training (70%) and validation (30%) sets
    # This split is crucial for training the model and then validating its performance
    
    # Model 4
    set.seed(123) # For reproducibility
    split <- createDataPartition(mydata$AdmittedNumeric, p = .7, list = FALSE)
    train_data <- mydata[split, ]
    test_data <- mydata[-split, ]
    
    Logistic_Model4 <- glm(AdmittedNumeric ~ Edu_Parent1 + Edu_Parent2 + HSGPA + SAT_ACT, family = binomial, data = train_data)
    summary(Logistic_Model4)
    model_logLik4 <- logLik(Logistic_Model4); model_logLik4
    
    pHat4 <- predict(Logistic_Model4, test_data, type = "response")
    yHat4 <- ifelse(pHat4 >= 0.5, 1,0)
    
    yHat4_factor <- factor(yHat4, levels = c(0, 1))
    AdmittedNumeric_factor4 <- factor(test_data$AdmittedNumeric, levels = c(0, 1))
    
    # Computing the confusion matrix
    confMatrix4 <- confusionMatrix(yHat4_factor, AdmittedNumeric_factor4)
    confMatrix4
    # Extract sensitivity and specificity
    accuracy4 <- 100*mean(test_data$AdmittedNumeric == yHat4)
    sensitivity4 <- confMatrix4$byClass["Sensitivity"]*100
    specificity4 <- confMatrix4$byClass["Specificity"]*100
    
    sprintf("Accuracy measure for model4 = %f",100*mean(test_data$AdmittedNumeric == yHat4))
    sprintf("Sensitivity for Model4 = %f",sensitivity4)
    sprintf("Specificity for Model4 = %f",specificity4)

    # Validating our model with 10-fold cross-validation method
    folds <- createFolds(mydata$AdmittedNumeric, k = 10)
    results <- list()
    folds
    specificities <- numeric(length(folds))  # Array to store specificity values
    sensitivities <- numeric(length(folds))  # Array to store sensitivity values
    
    for(i in 1:length(folds)) {
      # The rows for the training set are all the rows that are not in the current fold
      trainingRows <- unlist(folds[-i])
      # The rows for the test set are just the rows in the current fold
      testRows <- folds[[i]]
      
      # Extracting the training and test sets
      trainingSet <- mydata[trainingRows, ]
      testSet <- mydata[testRows, ]
      
      # Fitting the model on the training set
      Logistic_Model5 <- train(AdmittedNumeric ~ Edu_Parent1 + Edu_Parent2 + HSGPA + SAT_ACT,
                               data = trainingSet,
                               method = "glm",
                               family = "binomial",
                               trControl = trainControl(method = "none"))  
      Logistic_Model5
      finalModel <- Logistic_Model5$finalModel
      summary(finalModel)
      # Checking log-likelihood of the model
      modelLogLike <- logLik(finalModel); modelLogLike
      
      pHat5 <- predict(finalModel, testSet, type = "response")
      yHat5 <- ifelse(pHat5 >= 0.5, 1, 0)
      #yHat5
      yHat5 <- factor(yHat5, levels = c(0, 1))
      testSet$AdmittedNumeric <- factor(testSet$AdmittedNumeric, levels = c(0, 1))
      
      # Storing the results
      confMatrix <- confusionMatrix(yHat5, testSet$AdmittedNumeric)
      results[[i]] <- confMatrix
      specificities[i] <- confMatrix$byClass["Specificity"]
      sensitivities[i] <- confMatrix$byClass["Sensitivity"]
    }
    results
    # Calculate the average performance across all folds sensitivity1 <- confMatrix1$byClass["Sensitivity"]
    
    averageAccuracy <- mean(sapply(results, function(x) x$overall['Accuracy']))*100; averageAccuracy
    averageSpecificity <- mean(specificities)*100; averageSpecificity
    averageSensitivity <- mean(sensitivities)*100; averageSensitivity
    
    sprintf("Accuracy measure for model5 = %f",averageAccuracy)
    sprintf("Sensitivity for Model5 = %f",averageSensitivity)
    sprintf("Specificity for Model5 = %f",averageSpecificity)
    
    accuracy_values <- c(81.54,81.48,81.51,81.75,81.48)
    specificity_values<-c(64.12,63.97,64.1,65.42,89.21)
    sensitivity_values<-c(89.26,89.24,89.24,89.04,64.01)
    model_names <- c("Model 1", "Model 2", "Model 3","Model 4", "Model 5")
    
    # Creating a data frame
    accuracy_data <- data.frame(Model = model_names, Accuracy = accuracy_values)
    sensitivity_data<-data.frame(Model = model_names, Sensitivity = sensitivity_values)
    specificity_data<-data.frame(Model = model_names, Specificity = specificity_values)
    
    # creating a data list
    data_list <- list(accuracy_data, sensitivity_data, specificity_data)
    titles <- c("Logistic Model Accuracy Comparison-Admission", "Logistic Model Sensitivity Comparison-Admission", "Logistic Model Specificity Comparison-Admission")
    ylabs <- c("Accuracy", "Sensitivity", "Specificity")
    
    # Generating bar plots for accuracy, sensitivity and specificity
    for (i in seq_along(data_list)) {
      # Plot the bar chart
      barplot(data_list[[i]][, 2], names.arg = data_list[[i]]$Model,
              col = "skyblue", main = titles[i],
              ylab = ylabs[i])}
    
    
# 2. Model for enrollment classification using logistic Regression 
      
      # Creating a subset of the dataset that contains only students who were offered admission 
      myData<-mydata[mydata$Admitted=='Yes',]
      summary(myData)
      str(myData)
      
      #data transformation from numeric/ character to factor
      myData$Edu_Parent1<-as.factor(myData$Edu_Parent1)
      myData$Edu_Parent2<-as.factor(myData$Edu_Parent2)
      myData$Sex<-as.factor(myData$Sex)
      myData$White<-as.factor(myData$White)
      myData$Asian<-as.factor(myData$Asian)
      myData$College<-as.factor(myData$College)
      myData$Admitted<-as.factor(myData$Admitted)
      myData$Enrolled<-as.factor(myData$Enrolled)
      
      options(scipen=999)
      # creating dummy variables for the enrollment category
      myData$EnrolledNumeric <- ifelse(myData$Enrolled == "Yes", 1, 0)
      
      #creating the  first model for enrollment
      Logistic_Model_enrollment_1 <- glm(EnrolledNumeric ~ Edu_Parent1+Edu_Parent2+ Sex + White + Asian + HSGPA + SAT_ACT+College, family = binomial, data = myData)
      
      #generating the model summary
      summary(Logistic_Model_enrollment_1)
      
      #prediciting using the validation datasets
      pHat1 <- predict(Logistic_Model_enrollment_1, myData, type = "response")
      yHat1 <- ifelse(pHat1 >= 0.5, 1,0)
      yTP1 <- ifelse(yHat1 == 1 & myData$EnrolledNumeric == 1, 1, 0)
      yTN1 <- ifelse(yHat1 == 0 & myData$EnrolledNumeric == 0, 1, 0)
      #finding the accuracy, sensitivity and specificity measures for the model 
      sprintf("Accuracy measure for model1 = %f",100*mean(myData$EnrolledNumeric == yHat1))
      sprintf("Sensitivity for Model1 = %f",100*(sum(yTP1)/sum(myData$EnrolledNumeric==1)))
      sprintf("Specificity for Model1 = %f",100*(sum(yTN1)/sum(myData$EnrolledNumeric==0)))
      # This model gives an accuracy of 75.97%, sensitivity of 13.89% and specificity of 96.01% when all the categories of the dataset were obtained
      # Thus, this model can classify the sutdents who are unlike;y to enroll with a very high specificity
      # infering from the model summary, the following parameters can be neglected from the next model as they have a significance value of more than 0.05
      #Education of parent 1 with their education level being 1,2,3,4,6
      #Education of parent 2 with their education level being 2,3,5,6
      # Sex and Asian categories
      
      #Creating the 2nd model excluding the above mentioned categories
      Logistic_Model_enrollment_2 <- glm(EnrolledNumeric ~ Edu_Parent1+Edu_Parent2 + White + HSGPA + SAT_ACT+College, family = binomial, data = myData)
      #parent15+parent21+parent24
      #generating the model summary
      summary(Logistic_Model_enrollment_2)
      
      #prediciting using the validation data sets
      pHat2 <- predict(Logistic_Model_enrollment_2, myData, type = "response")
      yHat2 <- ifelse(pHat2 >= 0.5, 1,0)
      yTP2 <- ifelse(yHat2 == 1 & myData$EnrolledNumeric == 1, 1, 0)
      yTN2 <- ifelse(yHat2 == 0 & myData$EnrolledNumeric == 0, 1, 0)
      #finding the accuracy, sensitivity and specificity measures for the model
      sprintf("Accuracy measure for model2 = %f",100*mean(myData$EnrolledNumeric == yHat2))
      sprintf("Sensitivity for Model2 = %f",100*(sum(yTP2)/sum(myData$EnrolledNumeric==1)))
      sprintf("Specificity for Model2 = %f",100*(sum(yTN2)/sum(myData$EnrolledNumeric==0)))
      
      # this model gives an accuracy of 76.01%, sensitivity of 13.81% and specificity of 96.16%
      # The sensitivity slighty goes down and the specificity and accuracy of the model slightly goes up 
      
      #Creating a third model now, but randomly splitting the data into training and validation data sets
      set.seed(123)
      #splitting the data set
      split <- createDataPartition(myData$EnrolledNumeric, p = .7, list = FALSE)
      train_data <- myData[split, ]
      val_data <- myData[-split, ]
      
      #performing regression
      Logistic_Model_enrollment_3 <- glm(EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + SAT_ACT+College, family = binomial, data = train_data)
      summary(Logistic_Model_enrollment_3)
      
      #predicting using validation data sets
      pHat3 <- predict(Logistic_Model_enrollment_3, val_data, type = "response")
      yHat3 <- ifelse(pHat3 >= 0.5, 1,0)
      yTP3 <- ifelse(yHat3 == 1 & val_data$EnrolledNumeric == 1, 1, 0)
      yTN3 <- ifelse(yHat3 == 0 & val_data$EnrolledNumeric == 0, 1, 0)
      # performance measure calculations
      sprintf("Accuracy measure for model3 = %f",100*mean(val_data$EnrolledNumeric == yHat3))
      sprintf("Sensitivity for Model3 = %f",100*(sum(yTP3)/sum(val_data$EnrolledNumeric==1)))
      sprintf("Specificity for Model3 = %f",100*(sum(yTN3)/sum(val_data$EnrolledNumeric==0)))
      # this model gives an accuracy of 75.87%, sensitivity of 16.00% and specificity of 94.26%
      #This gives a higher sensitivity than the other models
      
      #Creating a 4th model but splitting data as the first 70% of raows as training data and the remaining as validation dataset 
      #splitting the data set into training and validation sets
      train_data <- myData[0:3726, ]
      val_data <- myData[3727:5323,]
      
      #performing logisitc regression
      Logistic_Model_enrollment_4 <- glm(EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + SAT_ACT+College, family = binomial, data = train_data)
      summary(Logistic_Model_enrollment_4)
      
      #prediciting using validations set
      pHat4 <- predict(Logistic_Model_enrollment_4, val_data, type = "response")
      yHat4 <- ifelse(pHat4 >= 0.5, 1,0)
      yTP4 <- ifelse(yHat4 == 1 & val_data$EnrolledNumeric == 1, 1, 0)
      yTN4 <- ifelse(yHat4 == 0 & val_data$EnrolledNumeric == 0, 1, 0)
      # performance measure calculations
      sprintf("Accuracy measure for model4 = %f",100*mean(val_data$EnrolledNumeric == yHat4))
      sprintf("Sensitivity for Model4 = %f",100*(sum(yTP4)/sum(val_data$EnrolledNumeric==1)))
      sprintf("Specificity for Model4 = %f",100*(sum(yTN4)/sum(val_data$EnrolledNumeric==0)))
      # this model gives an accuracy of 81.40%, sensitivity of 21.62% and specificity of 95.00%
      #This gives a higher accuracy and sensitivity than the other models
      
      # creating a 5th model by taking the middle 70% of rows as training data and the remaining as validation datasets
      #splitting the dataset
      train_data <- myData[799:4524, ]
      val_data <- myData[c(1:798,4525:5323),]
      
      #performing regression
      Logistic_Model_enrollment_5 <- glm(EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + SAT_ACT+College, family = binomial, data = train_data)
      summary(Logistic_Model_enrollment_5)
      
      #prediciting using validation set
      pHat5 <- predict(Logistic_Model_enrollment_5, val_data, type = "response")
      yHat5 <- ifelse(pHat5 >= 0.5, 1,0)
      yTP5 <- ifelse(yHat5 == 1 & val_data$EnrolledNumeric == 1, 1, 0)
      yTN5 <- ifelse(yHat5 == 0 & val_data$EnrolledNumeric == 0, 1, 0)
      # performance measure calculations
      sprintf("Accuracy measure for model5 = %f",100*mean(val_data$EnrolledNumeric == yHat5))
      sprintf("Sensitivity for Model5 = %f",100*(sum(yTP5)/sum(val_data$EnrolledNumeric==1)))
      sprintf("Specificity for Model5 = %f",100*(sum(yTN5)/sum(val_data$EnrolledNumeric==0)))
      # this model gives an accuracy of 73.70%, sensitivity of 11.33% and specificity of 95.6%
      #This gives a lower accuracy and sensitivity than the other models
      
      # creating a 6th model that has the last 70% of rows as training data and the first 30% of rows as validation data
      #splitting the data set into training and validation
      train_data <- myData[1597:5323, ]
      val_data <- myData[1:1596,]
      
      # performing logistic regression
      Logistic_Model_enrollment_6 <- glm(EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + SAT_ACT+College, family = binomial, data = train_data)
      summary(Logistic_Model_enrollment_6)
      
      #predicting using the validation data set
      pHat6 <- predict(Logistic_Model_enrollment_6, val_data, type = "response")
      yHat6 <- ifelse(pHat6 >= 0.5, 1,0)
      yTP6 <- ifelse(yHat6 == 1 & val_data$EnrolledNumeric == 1, 1, 0)
      yTN6 <- ifelse(yHat6 == 0 & val_data$EnrolledNumeric == 0, 1, 0)
      # performance measure calculations
      sprintf("Accuracy measure for model6 = %f",100*mean(val_data$EnrolledNumeric == yHat6))
      sprintf("Sensitivity for Model6 = %f",100*(sum(yTP6)/sum(val_data$EnrolledNumeric==1)))
      sprintf("Specificity for Model6 = %f",100*(sum(yTN6)/sum(val_data$EnrolledNumeric==0)))
      # this model gives an accuracy of 71.24%, sensitivity of 12.03% and specificity of 96.86%
      #This gives a lower accuracy and sensitivity than the other models
      
      #Creating a 7th  model, to oversample the students who have enrolled 
      
      # Set seed for reproducibility
      set.seed(123)
      
      #  Oversample data with students who have enrolled
      #  class 0 is "Not Enrolled" and class 1 is "Enrolled"
      class_0_indices <- which(myData$EnrolledNumeric == 0)
      class_1_indices <- which(myData$EnrolledNumeric == 1)
      oversample_factor <- length(class_0_indices) / length(class_1_indices)
      oversampled_indices <- sample(class_1_indices, replace = TRUE, size = round(oversample_factor * length(class_1_indices)))
      oversampled_data <- rbind(myData[class_0_indices, ], myData[oversampled_indices, ])
      
      
      # Splitting the dataset into training and validation sets
      split <- sample.split(oversampled_data$EnrolledNumeric, SplitRatio = 0.7)
      train_data <- subset(oversampled_data, split == TRUE)
      val_data <- subset(oversampled_data, split == FALSE)
      
      # Performing logistic regression
      Logistic_Model_enrollment_7 <- glm(EnrolledNumeric ~ Edu_Parent1 + Edu_Parent2 +White+ HSGPA + SAT_ACT, data = train_data, family = "binomial")
      
      # Prediction on the validation set
      pHat7 <- predict(Logistic_Model_enrollment_7, val_data, type = "response")
      yHat7 <- ifelse(pHat7 >= 0.5, 1,0)
      yTP7 <- ifelse(yHat7 == 1 & val_data$EnrolledNumeric == 1, 1, 0)
      yTN7 <- ifelse(yHat7 == 0 & val_data$EnrolledNumeric == 0, 1, 0)
      #performance measure calculations
      sprintf("Accuracy measure for model7 = %f",100*mean(val_data$EnrolledNumeric == yHat7))
      sprintf("Sensitivity for Model7 = %f",100*(sum(yTP7)/sum(val_data$EnrolledNumeric==1)))
      sprintf("Specificity for Model7 = %f",100*(sum(yTN7)/sum(val_data$EnrolledNumeric==0)))
      # This model gives a higher sensitivity compared to other models as this is overfitted.
      # A trade off is to be done to increase sensitivity as the sensitivity increased to 67.91% but accuracy decreaded to 67.2%
      
      # thus, the model that performs the best among all the 6 models is model 4 
      # this model gives an accuracy of 80.90%, sensitivity of 20.27% and specificity of 94.69%, performing the best in comparison to other models
      
      # plotting the values in graphs for all comparisons
      model_names <- c("Model 1", "Model 2", "Model 3","Model 4", "Model 5", "Model 6","Model 7")
      accuracy_values <- c(75.97, 76.01, 75.88,81.40,73.70,71.24,67.2)
      specificity_values<-c(96.01,96.17,94.26,95.00,95.6,96.86,66.5)
      sensitivity_values<-c(13.89,13.81,16.00,21.62,11.32,12.033,67.91)
      
      # Creating a data frame
      accuracy_data <- data.frame(Model = model_names, Accuracy = accuracy_values)
      sensitivity_data<-data.frame(Model = model_names, Sensitivity = sensitivity_values)
      specificity_data<-data.frame(Model = model_names, Specificity = specificity_values)
      
      # creating a data list
      data_list <- list(accuracy_data, sensitivity_data, specificity_data)
      titles <- c("Logistic Model Accuracy Comparison-Enrolled", "Logistic Model Sensitivity Comparison-Enrolled", "Logistic Model Specificity Comparison-Enrolled")
      ylabs <- c("Accuracy", "Sensitivity", "Specificity")
      
      # Generating bar plots for accuracy, sensitivity and specificity
      for (i in seq_along(data_list)) {
        # Plot the bar chart
        barplot(data_list[[i]][, 2], names.arg = data_list[[i]]$Model,
                col = "skyblue", main = titles[i],
                ylab = ylabs[i])}

      

# 3. Model for Admission classification using Classification Tree

      #Decision tree for classification
      options(scipen=999)  #to avoid scientific notation
      
      #For Classification tree model, R requires target variable as factor. so we will use as.factor command to convert Admitted variable into a categorical type. 
      
      mydata$Admitted <- as.factor(mydata$Admitted)
      
      #We use the set.seed command to set the random seed to 1, thus generating the partitions i.e. Training (70%) $ validation (30%) data.
      set.seed(1)
      
      #We use the createDataPartition function to partition the data into training (70%)
      #and validation (30%) data sets. 
      myIndex <- createDataPartition(mydata$Admitted, p=0.7, list=FALSE)
      trainSet <- mydata[myIndex,]
      dim(trainSet)
      validationSet <- mydata[-myIndex,]
      dim(validationSet)
      
      #We use the rpart function to generate the default classification tree,
      #default_tree. Within the rpart function, we specify the model structure,
      #data source, and method. The method option is set to "class" for
      #developing a classification tree.
      
      set.seed(1)
      default_tree <- rpart(Admitted ~Edu_Parent1+Edu_Parent2+Sex+White+Asian+HSGPA+SAT_ACT+College, data = trainSet, method = "class")
      summary(default_tree)
      
      #To view the classification tree visually, use the prp function. The type
      #option is set equal to 1 so that all nodes except the leaf nodes are
      #labeled in the tree diagram. The extra option is set equal to 1 so that
      #the number of observations that fall into each node is displayed. The
      #under option is set equal to TRUE to put the number of cases under
      #each decision node in the diagram. 
      prp(default_tree, type = 1, extra = 1, under = TRUE)
      
      # By identifying the value of cp associated with the smallest cross-validated classification error, 
      # we can create the minimum-error tree.
      #To plot Full grown tree
      
      set.seed(1)
      full_tree <- rpart(Admitted ~ Edu_Parent1+Edu_Parent2+Sex+White+Asian+HSGPA+SAT_ACT+College, data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
      prp(full_tree, type = 1, extra = 1, under = TRUE)
      
      #To identify the value of cp that is associated with the smallest crossvalidated classification error, we use the printcp function to display the
      #complexity parameter table.
      printcp(full_tree)
      
      # Use the "prune" function to create the pruned tree by using the cp value associated with the 
      # best pruned or minimum error tree (or middle)
      pruned_tree <- prune(full_tree, cp = 0.003219748 )
      prp(pruned_tree, type = 1, extra = 1, under = TRUE)
      
      # The figure displays the pruned tree.
      
      #We predict the class memberships of the observations in the validation
      #data set using the predict function.
      predicted_class <- predict(pruned_tree, validationSet, type = "class")
      
      #The confusion matrix can be created by comparing the predicted class
      #memberships and actual class memberships of the validation data set.
      
      confusionMatrix(predicted_class, validationSet$Admitted, positive = "Yes")
      
      # Review the resulting confusion matrix. 
      # The model produces an accuracy of 81.4%, sensitivity of 59.52% and specificity of 91.09%
      # but, results  are obtained using the default cutoff rate of 0.5
      
      # In this particular example, the default cutoff value is much higher than the proportion of target
      # cases in the data set, which is 0.307
      # By lowering the cutoff value to be close to the actual class distribution, we will be able to
      # classify more cases into the target class and improve the sensitivity measure.
      
      #To evaluate the predictive performance of the classification tree model
      #using a different cutoff value in R, we first compute the probability of
      #each validation case belonging to the target class instead of its class
      #membership. In the predict function, we set the type option equal to
      #"prob" to predict the probability values. 
      predicted_prob <- predict(pruned_tree, validationSet, type= "prob")
      head(predicted_prob)
      
      # the first column lists the probabilities belonging 
      # to the Class 0 and the second column lists probabilities of cases belonging to Class 1
      # To determine the class memberships of cases using a cutoff value other than the default value of 0.5,
      # we compare the values in the second column to the new cutoff value.
      
      #To construct a confusion matrix using the new cutoff value of 0.307, we
      #use the ifelse function to determine the class memberships and convert
      #them into text labels of 1s and 0s. We use the as.factor function to
      #convert the class membership to factor, which is the same data type as
      #the target variable, Admitted. Enter:
      
      # Create a factor variable based on a threshold (0.307 in this case)
      predicted_class <- as.factor(ifelse(predicted_prob[, 2] > 0.307, 'Yes', 'No'))
      
      # Ensure levels are in the correct order
      levels(predicted_class) <- c('No', 'Yes')
      
      # Create confusion matrix
      conf_matrix <- confusionMatrix(predicted_class, validationSet$Admitted, positive = 'Yes')
      
      # Print confusion matrix
      print(conf_matrix)
      
      #         Accuracy : 76.77%                        
      #         Sensitivity : 0.7788              
      #         Specificity : 0.7628          
      
      #         With an accuracy of 76.77% we can say that the model performance is good
      #         Sensitivity of 77.88% indicates that the model is good enough in catching the positive cases                               
      #         Specificity 76.28 % indicates that the model is able to capture negative cases effectively
      
      #We will use pruned tree as it is more sensitive to our data set (sensitivity value = 0.7788) 
    
      #We first convert the target variable (Admitted) from a factor variable to
      #a numerical data type as required by the gains package using the
      #as.character and as.numeric functions. We generate the cumulative lift
      #table using the gains function. The gains function requires two inputs:
      #Admitted students and predicted Admissions probabilities.
      
      validationSet$Admitted <- as.numeric(validationSet$Admitted)
      
      gains_table <- gains(validationSet$Admitted, predicted_prob[,2])
      gains_table
      
      # Note that gains function generates few groups instead of the default 10 groups,
      # one for each unique probability value
      
      # Lift Chart
      
      plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Admitted)) ~ c(0, gains_table$cume.obs), xlab = 'No. of Admitted students', ylab = "Cumulative", type = "l",main="CART Lift Chart-Admitted")
      lines(c(0, sum(validationSet$Admitted))~c(0, dim(validationSet)[1]), col="red", lty=2)
      
      #the lift curve of the model lies above the diagonal line , indicating that the classification model performs
      #better than the baseline model in terms of predicting whether an applicant will get admitted or not
      
      # Decile Chart 
      
      barplot(gains_table$mean.resp/mean(validationSet$Admitted), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="CART Decile-Wise Lift Chart- Admitted")
      #the chart shows us that if we focus on the top 23% of the applicants with highest probability of enrolling
      
      #we would be able to capture approx. 1.35 times as many applicants who will belong to the target class than the base model
      
      
      #ROC Chart 
      roc_obj <- roc(validationSet$Admitted, predicted_prob[,2])
      plot(roc_obj, print.auc=TRUE,main="CART ROC Chart- Admitted")
      auc(roc_obj)
      #area under roc curve is approx ~ 82.56%, indicating that the model performs better in predicting 
      #the applicants who will get admitted to the colleges than the baseline model .
      

# 4. Model for enrollment classification using Classification Tree 
     
      View(myData)
      
      #converting Enrolled
      myData<-mydata[mydata$Admitted=='Yes',]
      myData$Enrolled <- ifelse(myData$Enrolled == "Yes",1,0)
      str(myData)
      
      #data transformation
      myData$Enrolled<-as.factor(myData$Enrolled)
      
      # Oversample data with students who have enrolled
      # class 0 is "Not Enrolled" and class 1 is "Enrolled"
      
      set.seed(123)
      class_0_indices <- which(myData$Enrolled == 0)
      class_1_indices <- which(myData$Enrolled == 1)
      oversample_factor <- length(class_0_indices) / length(class_1_indices)
      oversampled_indices <- sample(class_1_indices, replace = TRUE, size = round(oversample_factor * length(class_1_indices)))
      oversampled_data <- rbind(myData[class_0_indices, ], myData[oversampled_indices, ])
      
      #since Enrolled= Yes or 1 which is too less compared to the Enrolled = No or 0 . The default #decision tree is not able classify them properly .Hence we are oversampling our target class to get the intial tree
      # Splitting the dataset into training and validation sets
      split <- sample.split(oversampled_data$Enrolled, SplitRatio = 0.7)
      trainSet <- subset(oversampled_data, split == TRUE)
      validationSet <- subset(oversampled_data, split == FALSE)
      
      #Generating the default classification tree ,default_tree
      set.seed(123)
      default_tree <- rpart(Enrolled ~ Edu_Parent1 + Edu_Parent2 + Sex +White+ Asian +HSGPA+ SAT_ACT +College,data = trainSet, method = "class")
      
      # see the summary of the deafult tree
      summary(default_tree)
      
      #To view the classification tree visually
      prp(default_tree, type = 1, extra = 1, under = TRUE)
      
      # to plot the full tree using the prp function
      set.seed(123)
      full_tree <- rpart(Enrolled ~ Edu_Parent1 + Edu_Parent2 + Sex +White+ Asian +HSGPA+ SAT_ACT +College, data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
      prp(full_tree, type = 1, extra = 1, under = TRUE)
      
      #To identify the value of cp that is associated with the smallest crossvalidated classification error, we use the printcp function to display the
      #complexity parameter table.
      printcp(full_tree)
      
      # Creating the pruned tree by using the cp value associated with the
      # best pruned or minimum error tree (or middle)
      pruned_tree <- prune(full_tree, cp = 0.004027482 )
      prp(pruned_tree, type = 1, extra = 1, under = TRUE)
      
      # The figure displays the pruned tree
      #We predict the class memberships of the observations in the validation
      #data set using
      predicted_class <- predict(pruned_tree, validationSet, type = "class")
      
      #The confusion matrix can be created by comparing the predicted class
      #memberships and actual class memberships of the validation data set.
      
      myData$Enrolled<-as.factor(myData$Enrolled)
      confusionMatrix(predicted_class, validationSet$Enrolled, positive = "1")
      
      # The resulting confusion matrix. It shows results using the default cutoff rate of 0.5
      predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
      
      # the first column lists the probabilities belonging
      # to the Class 0 and the second column lists probabilities of cases belonging to Class 1
      #To construct a confusion matrix using cutoff value of 0.5 which is to be used here
      confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.50, '1', '0')), validationSet$Enrolled, positive = '1')
      
      # Accuracy : 0.6675
      # 95% CI : (0.6483, 0.6863)
      # Sensitivity : 0.6418
      # Specificity : 0.6932
      # With an accuracy of 66.75% we can say that the model performance is okay
      # Sensitivity of 64.18 % indicates that the model is able to capture the positivecase to an extent
      # Specificity 69.32% indicates that the model is able to capture negative cases to an extent
      
      validationSet$Enrolled <- as.numeric(as.character(validationSet$Enrolled))
      gains_table <- gains(validationSet$Enrolled, predicted_prob[,2])
      gains_table
      
      # one  group for each unique probability value
      
      # Lift Chart
      plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Enrolled)) ~ c(0, gains_table$cume.obs), xlab = 'No of Students', ylab = "Cumulative", type = "l",main = "CART LIft Chart-Enrolled")
      lines(c(0, sum(validationSet$Enrolled))~c(0, dim(validationSet)[1]), col="red", lty=2)
      
      #the lift curve of the model lies above the diagonal line , indicating that the classification model #performs
      #better than the baseline model in terms of predicting whether an applicant will enroll or not
      
      # Decile Chart
      
      barplot(gains_table$mean.resp/mean(validationSet$Enrolled), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="CART Decile-Wise Lift Chart- Enrolled")
      
      #the chart shows us that if we focus on the top 43% of the applicants with highest probability of #enrolling
      #we would be able to capture approx. 1.35 times as many applicants who will belong to the target #class than the basemodel
      
      # ROC chart
      roc_object <- roc(validationSet$Enrolled, predicted_prob[,2])
      plot.roc(roc_object,main="CART ROC curve- Enrolled")
      auc(roc_object)
      
      #area under roc curve is approx ~ 67.64%, indicating that the model performs better in predicting 
      #the applicants who will enroll to the colleges than the baseline model ..
      
      
      
# Final Model Comparisons
      
      #levels for the factors
      models <- c("Logistic Regression-Admission", "Logistic Regression-Enrolled", "CART-Admission", "CART-Enrolled")
      measures <- c("Accuracy", "Sensitivity", "Specificity")
      
      # combinations of models and measures
      model_metrics <- expand.grid(Model = models, Measure = measures)
      
      # Input for the Value column
      model_metrics$Value <- c(81.75, 81.4, 76.77,66.75,  # Dummy values for Accuracy
                               89.04, 21.62, 77.88,64.18,  # Dummy values for Sensitivity
                               65.42, 95.00, 76.28,69.32)  # Dummy values for Specificity
      
      # Convert Model and Measure columns to factors
      model_metrics$Model <- as.factor(model_metrics$Model)
      model_metrics$Measure <- as.factor(model_metrics$Measure)e
      print(model_metrics)
      
      #Plotting the comparison graph
      ggplot(data = model_metrics, aes(x = Measure, y = Value, fill = Model)) +
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
        ylim(0,100) +geom_text(aes(label = Value), vjust = 1.5,
                               position = position_dodge(.9), size = 3) +
        labs(x = "\n Performance Measures", y = "Percentage\n", title = "\n Model Comparision \n") +
        theme(plot.title = element_text(hjust = 0.5), 
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
              legend.title = element_text(face="bold", size = 10))
      
      #Inferences:
        # The logistic model for admission performs better in comparison to the CART model for both accuracy and sensitivity
        # The logistic model for enrollment performs better in comparison to the CART model in terms of accuracy and specificity
        # Sex and Asian categories are not significant categories for both admission and enrollment 
        # Students who's parents have a college undergraduation/ postgraduation degree are very likely to enroll
        # Thus, parent's educational background can be a facotr than influences a students descision to enroll for a college or not 
        # Students who were provided admission into Math & Science and Business & Economics are more likely to enroll when compared to Arts & Letters college
      