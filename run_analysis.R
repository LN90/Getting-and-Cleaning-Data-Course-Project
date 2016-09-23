library(dplyr)
library(tidyr)
library(gdata)
library(data.table)

run_analysis <- function(no_idea){
  
  ################################################################################################################################################
  # STEP ONE
  #set directory good (because this is work pc I cannot show you the actual code)
  #I change the directory several times to go to the different files
 
  #set directory good
  setwd("~test")
  y_test<-read.table("y_test.txt")
  x_test<-read.table("x_test.txt")
  subject_test<-read.table("subject_test.txt")
 
  #set directory good 
  setwd("~train")
  subject_train<-read.table("subject_train.txt")
  y_train<-read.table("y_train.txt")
  x_train<-read.table("x_train.txt")
 
  #set directory good
  setwd("~")
  features<-read.table("features.txt")
  activity_labels<-read.table("activity_labels.txt")
 
  ##################################################################################################################################
  # STEP TWO
  #for x_test
  #dim(x_test)=2947x561
  #the rows are connected to subject_test (2947x1) and y_test (2947x1)
  #respectivily is y_test connected to activity_labels
  #the collums of x_test are features (561x2)
 
  #start with adding colnames to x_test
  #the names are row var in features
  #plan: make vector of features, plug that vector in colnames X_test
  features_vector<-as.vector(features$V2)
  x_test_features<-setNames(x_test,features_vector)
  features_vector_unique<-make.names(names=names(x_test_features),unique=TRUE, allow_=TRUE)
  names(x_test_features)<-features_vector_unique
 
  #replace y_test numbers by words from activity label
  colnames(y_test)[1]="activitynumber"
  colnames(activity_labels)[1]="activitynumber"
  colnames(activity_labels)[2]="Activity"
 
  #merge
  #the problem I ran into was that the original roder of y_test was not remained when I merged the two datasets
  #to solve this I add an id row to y_test
  #then I merge the two datasets and arrange on id so the original order is remained
  #code:
  y_test$id<-1:nrow(y_test)
  y_test_f<-merge(y_test,activity_labels,all.x = TRUE)
  y_test_f<-arrange(y_test_f,id)
 
  #now add the subjects to this dataframe
  colnames(subject_test)<-"Subject"
  y_test_f_s<-cbind(subject_test,y_test_f)
  y_test_final<-select(y_test_f_s,-id,-activitynumber)
 
  #now I want x_test merged with y_test_f
  #this gives an error, the dataframe is to big
  #so first I select the wanted colums for this assignment
  #this are all the collums with the name "mean" or "std" in it
  #this gives the column numbers of x_test where "mean" or "std" occur
  mean_colnumtest<-grep("mean",names(x_test_features))
  std_colnumtest<-grep("std",names(x_test_features))
 
  #now subset only x_test_features with "mean" and "std" in it
  #problem occured of colums having same name
  #fix this by:
  x_test_f_ms<-select(x_test_features,c(mean_colnumtest,std_colnumtest))
 
  #I wanted to merge the two datasets, however I got memory isseus so I use cbind
  x_test_fmsa<-cbind( y_test_final,x_test_f_ms)
 
  #####################################################################################################################################################
  # STEP THREE
  #for x_train
  #dim(x_test)=2947x561
  #the rows are connected to subject_train (2947x1) and y_train (2947x1)
  #respectivily is y_train connected to activity_labels
  #the collums of x_train are features (561x2)
 
  #start with adding colnames to x_train
  #the names are row var in features
  #plan: make vector of features, plug that vector in colnames X_test
  #make all column names unique else you get trouble later with selecting mean and std columns
  features_vector<-as.vector(features$V2)
  x_train_features<-setNames(x_train,features_vector)
  features_vector_unique <- make.names(names=names(x_train_features), unique=TRUE, allow_ = TRUE)
  names(x_train_features)<-features_vector_unique
 
  #replace y_test numbers by words from activity label
  colnames(y_train)[1]="activitynumber"
  colnames(activity_labels)[1]="activitynumber"
  colnames(activity_labels)[2]="Activity"
 
  #merge
  #the problem I ran into was that the original roder of y_train was not remained when I merged the two datasets
  #to solve this I add an id row to y_train
  #then I merge the two datasets and arrange on id so the original order is remained
  #code:
  y_train$id<-1:nrow(y_train)
  y_train_f<-merge(y_train,activity_labels,all.x = TRUE)
  y_train_f<-arrange(y_train_f,id)
 
  #now add the subjects to this dataframe
  colnames(subject_train)<-"Subject"
  y_train_f_s<-cbind(subject_train,y_train_f)
  y_train_final<-select(y_train_f_s,-id,-activitynumber)
 
  #now I want x_train merged with y_train_f
  #this gives an error, the dataframe is to big
  #so first I select the wanted colums for this assignment
  #this are all the collums with the name "mean" or "std" in it
  #this gives the column numbers of x_train where "mean" or "std" occur
  mean_colnumtrain<-grep("mean",names(x_train_features))
  std_colnumtrain<-grep("std",names(x_train_features))
 
  #now subset only x_test_features with "mean" and "std" in it
  x_train_f_ms<-select(x_train_features,c(mean_colnumtrain,std_colnumtrain))
 
  #I wanted to merge the two datasets, however I got memory isseus so I use cbind
  x_train_fmsa<-cbind(y_train_final,x_train_f_ms)
 
  #####################################################################################################################################################
  # STEP 4
  #now we have desired columns in x_test and x_train: x_test_fmsa and x_train_fmsa
  #the colums have the correct names
  #both start with id which has to leave
  testfinal<-x_test_fmsa
  trainfinal<-x_train_fmsa
  
  #to keep overview, remove the rest:
  keep(list="testfinal","trainfinal","features_vector",sure=TRUE)
 
  final<-rbind(testfinal,trainfinal)
 
  #####################################################################################################################################################
  # STEP 5
 
  #now create new dataframe with the means of each subject, each acitivity
  # and mean of feature
  #use the ddply function to split up dataframe,apply function and return result in a dataframe
  means<-ddply(final, .(Subject, Activity),function(x) colMeans(x[,3:81]))
 
  
}





