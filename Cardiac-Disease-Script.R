if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign")
if(!require(GGally))install.packages("GGally")

#Downloading and checking dataset

data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", sep=",")

str(data) #look at the dataframe

#Age: age in years
#
#Sex: (1 = male; 0 = female)
#
#Chest.Pain: chest pain type -- Value 1: typical angina -- Value 2: atypical angina -- Value 3: non-anginal pain Value 4: Asymptomatic
#
#BP.Rest: resting blood pressure (in mm Hg on admission to the hospital)
#
#Chol.Liv: serum cholestoral in mg/dl
#
#Fast.Blood.Sugar: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
#
#ECG.Rest: resting electrocardiographic results: 
#                 Value 0: normal  
#                 Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) 
#                 Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria
#
#HR.Max: maximum heart rate achieved
#
#Angina.post.Exercise: exercise induced angina (1 = yes; 0 = no)
#
#OldPeak.ST: ST depression induced by exercise relative to rest 
#
#Slope.ST: the slope of the peak exercise ST segment
#
#Vessels.Fluo:number of major vessels (0-3) colored by flourosopy
#
#Defect:3 = normal; 6 = fixed defect; 7 = reversable defect
#
#Disease: disease in the patient. It is integer valued from 0 (no presence) to 4. 
#         Experiments with the Cleveland database have concentrated on simply attempting to distinguish 
#         presence (values 1,2,3,4) from absence (value 0).




                              #DATASET CLEANING AND VARIABLES SETTING UP



#adding column names
colnames(data) <- c("Age", "Sex", "Chest.Pain", "BP.Rest", "Chol.Liv", "Fast.Blood.Sugar", "ECG.Rest", "HR.Max",
                  "Angina.post.Exercise", "OldPeak.ST", "Slope.ST", "Vessels.Fluo", "Defect", "Disease")

#Check for changes after renaming columns, and check for NAs
str(data)

#Check for NAs
apply(is.na(data), 2,which) 

#After the check, despite there wasn't any NAs, we have seen that there was some missing variables 
#indicated by the question mark "?", so we procede to remove them all by filtering dataset.
data <- data %>% filter_all(all_vars(.!="?")) 

#Now we have removed 6 entries, but due to the pregress "?", despite his numeric nature, "Vessels.Flu" variable
#was loaded as Factor. We have to convert it in a numerical variable and remove two values in accord with the dataset legend.

data$Vessels.Fluo<-as.numeric(data$Vessels.Fluo) -2

#For the same reason ("?") "Defect" variable present four levels instead three. We have to correct it, and also convert in factor
#other categorical/binary variables that was mistaken read as numeric from the file.

data$Defect<-factor(data$Defect)
data$Sex<-factor(data$Sex)
data$Chest.Pain<-factor(data$Chest.Pain)
data$Fast.Blood.Sugar<-as.factor(data$Fast.Blood.Sugar)
data$ECG.Rest<-as.factor(data$ECG.Rest)
data$Angina.post.Exercise<-as.factor(data$Angina.post.Exercise)
data$Slope.ST<-as.factor(data$Slope.ST)

#For further analyses we also convert Disease variable in binary s reason, where 0 means "No disease", 1 "Disease present"

data<-data%>%mutate(Disease=as.factor(ifelse(Disease==0,0,1)))

#Checking after setting up the dataset
str(data)



                                        #STARTING EXPLORATORY DATA ANALYSIS
#Make summary of the 14 variables
summary(data[,1:6])%>%knitr::kable()
summary(data[,7:14])%>%knitr::kable()

#Make a preview of the first 10 entries

head(data,10)

   
#Heart Disease Males VS Females
data %>%
    ggplot(aes(x=Disease, fill=Disease, color=Disease)) +
    geom_bar() +
    theme(legend.position="bottom") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_wrap(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Disease Males VS Females")
    
#Heart Diseases versus Age for Males and Females

data%>%
    ggplot(aes(x=Age, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Age") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versus Age for Males and Females")

#Heart Diseases versus Systolic BP for Males and Females

data%>%
    ggplot(aes(x=BP.Rest, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="BP") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versus Systolic BP for Males and Females")

#Heart Diseases versus Cholesterol Levels for Males and Females

data%>%
    ggplot(aes(x=Chol.Liv, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Chol.Liv.") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versus Cholesterol Levels for Males and Females")

#Heart Diseases versus Fast blood Sugar >120 mg/dl exercises for Males and Females

data%>%
    ggplot(aes(x=Disease, fill=Disease, color=Disease))+
    geom_bar() +
    theme(legend.position="bottom") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~Fast.Blood.Sugar, labeller = labeller(Sex=c("0"="Female", "1"="Male"), Fast.Blood.Sugar=c("0"="<=120mg/dl", "1"=">120mg/dl")))+
    ggtitle("Heart Disease VS FBS > 120 mg/dl Males and Females")

#Heart Diseases versus HR max for Males and Females

data%>%
    ggplot(aes(x=HR.Max, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="HR Max") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versus HR max for Males and Females")

#Heart Diseases versus ECG for Males and Females

data%>%
    ggplot(aes(x=Disease, fill=Disease, color=Disease))+
    geom_bar() +
    theme(legend.position="bottom") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~ECG.Rest, labeller = labeller(Sex=c("0"="Female", "1"="Male"), ECG.Rest=c("0"="ECG: Normal", "1"="ECG: ST.Abn.", "2"="ECG: Vent.Hyper.")))+
    ggtitle("Heart Disease VS ECG Males and Females")

#Heart Diseases versus Vessels colored in Fluo. for Males and Females

data%>%
    ggplot(aes(x=Vessels.Fluo, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Vessels") +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versus Vessels colored in Fluo for Males and Females")

#Heart Diseases versus ST depression post exercises for Males and Females

data%>%
    ggplot(aes(x=OldPeak.ST, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="ST.Dept") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~., labeller = labeller(Sex=c("0"="Female", "1"="Male")))+
    ggtitle("Heart Diseases versusST depression post exercises for Males and Females")

#Heart Diseases versus Angina post exercises for Males and Females

data%>%
    ggplot(aes(x=Disease, fill=Disease, color=Disease))+
    geom_bar() +
    theme(legend.position="bottom") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~Angina.post.Exercise, labeller = labeller(Sex=c("0"="Female", "1"="Male"), Angina.post.Exercise=c("0"="No Angina", "1"="Angina")))+
    ggtitle("Heart Disease VS Angina Males and Females")

#Heart Diseases versus Chest Pain for Males and Females

data%>%
    ggplot(aes(x=Disease, fill=Disease, color=Disease))+
    geom_bar() +
    theme(legend.position="bottom") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_grid(Sex~Chest.Pain, labeller = labeller(Sex=c("0"="Female", "1"="Male"), Chest.Pain=c("1"="Typical Angina", "2"="Atypical Angina", "3"="No Anginal Pain", "4"="Asymptomatic")))+
    ggtitle("Heart Disease VS Chest Pain Males and Females")



#Checking for the variables correlation
ggcorr(cor(matrix(as.numeric(unlist(data)),nrow=nrow(data))),label=TRUE,nbreaks = 5)+
    ggtitle("Correlation Plot")


                                                            #TESTING DATASET


#Now we'll split the dataset in Test and Training set, with a proportion 8:2. For this task we'll use caret package.
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
test_index<-createDataPartition(data$Disease,times = 1, p=0.2, list=F) #Indexing for test set
test_set<-data[test_index,] #creating test set
train_set<-data[-test_index,] #crating training set


#Checking for datasets distributions
summary(train_set)
summary(test_set)




                                                                #MODELING

# Define train control for k-fold (10-fold here) cross validation
fitControl <- trainControl(method = "cv",number = 10, savePredictions = TRUE)

#starting with a KNN Model
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
fit_KNN<-train(Disease~., data= train_set, method="knn", tuneGrid=data.frame(k=seq(1,30,1)),trControl=fitControl) #fit model and try some k's
pre_KNN<-predict(fit_KNN,test_set) #predict results
confusionMatrix(pre_KNN,test_set$Disease) #make a confusion matrix to calculate Accuracy of the model
#saving parameters and print the results
Accuracy_KNN <- confusionMatrix(pre_KNN,test_set$Disease)$overall["Accuracy"]
Sensitivity_KNN <- confusionMatrix(pre_KNN,test_set$Disease)$byClass["Sensitivity"]
Specificity_KNN <- confusionMatrix(pre_KNN,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_KNN <- confusionMatrix(pre_KNN,test_set$Disease)$byClass["Balanced Accuracy"]

results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN)
results

#Adaboost Classification trees model
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
fit_ada<-train(Disease~.,data=train_set, method="adaboost",trControl=fitControl) #fit model
pre_ada<-predict(fit_ada,test_set) #predict results
confusionMatrix(pre_ada,test_set$Disease) #make a confusion matrix to calculate Accuracy of the model
#saving parameters and print the results
Accuracy_ada <- confusionMatrix(pre_ada,test_set$Disease)$overall["Accuracy"]
Sensitivity_ada <- confusionMatrix(pre_ada,test_set$Disease)$byClass["Sensitivity"]
Specificity_ada <- confusionMatrix(pre_ada,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_ada <- confusionMatrix(pre_ada,test_set$Disease)$byClass["Balanced Accuracy"]

results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN,
    "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,Bal.Accuracy_ada)
results

#Logistic Regression Model
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
fit_GLM<-train(Disease~., data= train_set, method="glm", family="binomial", trControl=fitControl) #fit model
pre_GLM<-predict(fit_GLM,test_set) #predict results
confusionMatrix(pre_GLM,test_set$Disease) #make a confusion matrix to calculate Accuracy of the model
#saving parameters and print the results
Accuracy_GLM <- confusionMatrix(pre_GLM,test_set$Disease)$overall["Accuracy"]
Sensitivity_GLM <- confusionMatrix(pre_GLM,test_set$Disease)$byClass["Sensitivity"]
Specificity_GLM <- confusionMatrix(pre_GLM,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_GLM <- confusionMatrix(pre_GLM,test_set$Disease)$byClass["Balanced Accuracy"]

results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN,
    "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,Bal.Accuracy_ada,
    "General Logistic Regression Model", Accuracy_GLM,  Sensitivity_GLM, Specificity_GLM,Bal.Accuracy_GLM)
results

#Decision tree Model
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
fit_rpart<-train(Disease~., data= train_set, method="rpart",trControl=fitControl) #fit model
pre_rpart<-predict(fit_rpart,test_set) #predict results
confusionMatrix(pre_rpart,test_set$Disease) #make a confusion matrix to calculate Accuracy of the model
#saving parameters and print the results
Accuracy_rpart <- confusionMatrix(pre_rpart,test_set$Disease)$overall["Accuracy"]
Sensitivity_rpart <- confusionMatrix(pre_rpart,test_set$Disease)$byClass["Sensitivity"]
Specificity_rpart <- confusionMatrix(pre_rpart,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_rpart <- confusionMatrix(pre_rpart,test_set$Disease)$byClass["Balanced Accuracy"]

results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN,
    "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,Bal.Accuracy_ada,
    "General Logistic Regression Model", Accuracy_GLM,  Sensitivity_GLM, Specificity_GLM,Bal.Accuracy_GLM,
    "Decision Tree Model", Accuracy_rpart,Sensitivity_rpart,Specificity_rpart,Bal.Accuracy_rpart)
results

#Random Forest model
set.seed(1, sample.kind = "Rounding") #use set.seed(1) if use R version < 3.6
fit_rf<-train(Disease~., data= train_set, method="rf",trControl=fitControl) #fit model
pre_rf<-predict(fit_rf,test_set) #predict results
confusionMatrix(pre_rf,test_set$Disease) #make a confusion matrix to calculate Accuracy of the model
#saving parameters and print the results
Accuracy_rf <- confusionMatrix(pre_rf,test_set$Disease)$overall["Accuracy"]
Sensitivity_rf <- confusionMatrix(pre_rf,test_set$Disease)$byClass["Sensitivity"]
Specificity_rf <- confusionMatrix(pre_rf,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_rf <- confusionMatrix(pre_rf,test_set$Disease)$byClass["Balanced Accuracy"]

results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN,
    "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,Bal.Accuracy_ada,
    "General Logistic Regression Model", Accuracy_GLM,  Sensitivity_GLM, Specificity_GLM,Bal.Accuracy_GLM,
    "Decision Tree Model", Accuracy_rpart,Sensitivity_rpart,Specificity_rpart,Bal.Accuracy_rpart,
    "Random Forest", Accuracy_rf,  Sensitivity_rf, Specificity_rf,Bal.Accuracy_rf)
results

#Try to improve our models with ensemble strategy
ensemble<-data.frame(pre_GLM,pre_rf,pre_ada)
pre_ensemble<-as.factor(ifelse(rowMeans(ensemble==1)>0.5,1,0))
confusionMatrix(pre_ensemble,test_set$Disease)
#saving parameters
Accuracy_ens <- confusionMatrix(pre_ensemble,test_set$Disease)$overall["Accuracy"]
Sensitivity_ens <- confusionMatrix(pre_ensemble,test_set$Disease)$byClass["Sensitivity"]
Specificity_ens<- confusionMatrix(pre_ensemble,test_set$Disease)$byClass["Specificity"]
Bal.Accuracy_ens <- confusionMatrix(pre_ensemble,test_set$Disease)$byClass["Balanced Accuracy"]


#Print final table of results.
results <- tribble(
    ~Method, ~Accuracy, ~Sensitivity,  ~Specificity, ~Bal.Accuracy,
    "KNN", Accuracy_KNN,  Sensitivity_KNN, Specificity_KNN,Bal.Accuracy_KNN,
    "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,Bal.Accuracy_ada,
    "General Logistic Regression Model", Accuracy_GLM,  Sensitivity_GLM, Specificity_GLM,Bal.Accuracy_GLM,
    "Decision Tree Model", Accuracy_rpart,Sensitivity_rpart,Specificity_rpart,Bal.Accuracy_rpart,
    "Random Forest", Accuracy_rf,  Sensitivity_rf, Specificity_rf,Bal.Accuracy_rf,
    "Ensamble Model",Accuracy_ens,Sensitivity_ens,Specificity_ens,Bal.Accuracy_ens
)
results



##########################  end  #########################




