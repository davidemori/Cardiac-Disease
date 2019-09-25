if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign")

#Downloading and checking dataset

data<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", sep=",")

str(data) #look at the dataframe

#Age: age in years
#
#Sex: (1 = male; 0 = female)
#
#Chest.Pain: chest pain type -- Value 1: typical angina -- Value 2: atypical angina -- Value 3: non-anginal pain
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




colnames(data) <- c("Age", "Sex", "Chest.Pain", "BP.Rest", "Chol.Liv", "Fast.Blood.Sugar", "ECG.Rest", "HR.Max",
                  "Angina.post.Exercise", "OldPeak.ST", "Slope.ST", "Vessels.Fluo", "Defect", "Disease")

#Check for changes after renaming columns, and check for NAs
str(data)

#Check for NAs
apply(is.na(data), 2,which) 

#After the check, despite there wasn't any NAs, we have seen that there was some missing variables 
#indicated by the question mark "?", so we procede to remove them all by filtering dataset.
data <- data %>% filter_all(all_vars(.!="?")) 

#Now we have removed 6 entries, but due to the pregress "?", despite his numeric nature, Vessels.Flu variable
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


data%>%ggplot(aes(x=Age, fill=Disease, color=Disease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name="Age") +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    facet_wrap(vars(Sex), labeller = labeller(Sex=c("0"="Male", "1"="Female")))