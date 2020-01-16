
#Removing RAM 
rm(list=ls())
 
#Setting working directory
 setwd("C://Users//akhil//Desktop//Kriti Data//EDWISOR_invoice//data science study material//Project")

#Checking the working directory
 getwd()
 
#Loading csv data into Credit_Card_Data object
 Credit_Card_Data=read.csv("credit-card-data.csv",header = TRUE)
 
#Opening the loaded data 
 Credit_Card_Data

# Getting structure of dataset with its datatype 
  str(Credit_Card_Data)

#Getting column names of the dataset 
 colnames(Credit_Card_Data)

#loading libraries for visualization and preprocessing of data
  x=c("ggplot2","corrgram","caret","DMwR","unbalanced","C50","dummies","MASS","rpart","gbm","ROSE")
  install.packages(x)
  lapply(x,require,character.only=TRUE)
  rm(x)

-------------------------------------------------------------------
############## MISSING VALUE ANALYSIS #############################
--------------------------------------------------------------------
  
#Missing value detection in each variable of the dataset by using apply function that uses Sum(is.na()) function as an argument and will return total count of the missing values present in each variable of the dataset. 
Variable_with_Missing_VAlues=data.frame(apply(Credit_Card_Data,2, function(x) {sum(is.na(x))}))
Variable_with_Missing_VAlues

#Only two variables in the dataset contain missing values they are CREDIT_LIMIT and MINIMUM_PAYMENTS
#Computing missing values of the variable

#Calculating the missing value percentage
MINIMUM_PAYMENTS_Missing_Per=(sum(is.na(Credit_Card_Data$MINIMUM_PAYMENTS))/nrow(Credit_Card_Data))*100
MINIMUM_PAYMENTS_Missing_Per

CREDIT_LIMIT_Missing_Per=(sum(is.na(Credit_Card_Data$CREDIT_LIMIT))/nrow(Credit_Card_Data))*100
CREDIT_LIMIT_Missing_Per

#Since the mising value percentage of both the variable is less than 30% so we cannot ignore the missing values.

#Deleting some values manually and finding missing values to find best method for missing value analysis

#Finding missing value using mean:
#Credit_Card_Data[3,16]=NA
#Credit_Card_Data$MINIMUM_PAYMENTS
#Credit_Card_Data$MINIMUM_PAYMENTS[is.na(Credit_Card_Data$MINIMUM_PAYMENTS)]=mean(Credit_Card_Data$MINIMUM_PAYMENTS,na.rm=T)
#Credit_Card_Data
# original value= 627.28479
# missing value computed by mean method= 824.23398

#finding missing value using KNN imputation method:
#Credit_Card_Data[3,16]=NA
#Credit_Card_Data$MINIMUM_PAYMENTS
#Credit_Card_Data=knnImputation(Credit_Card_Data,k=5)
#Credit_Card_Data$MINIMUM_PAYMENTS
#Original value= 627.28479
#missing value computed by KNN imputation= 689.7210

#On comparing missing values computed by the KNN imputation is more closer to the actual value so in this case KNN imputation is the best method for computing missing values 
#Computing missing values for the variables CREDIT_LIMIT and MINIMUM_PAYMENTS using KNN imputation method
Credit_Card_Data=knnImputation(Credit_Card_Data,k=5)

#ReCheck for missing values
sum(is.na(Credit_Card_Data))


-------------------------------------------------------------------------------
######################## OUTLIER ANALYSIS ###################################
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------  
############### USING OUTLIERS REPLACE BY NA  METHOD ################
-------------------------------------------------------------------------------
  
#Selecting only numeric variables
Numeric_index= sapply(Credit_Card_Data,is.numeric)
Numeric_data=Credit_Card_Data[,Numeric_index]
#Variables names containing numeric data
Cnames=colnames(Numeric_data)
Cnames

# Outlier check
for(i in 1:length(Cnames))
{ assign(paste0("gn",i),ggplot(aes_string(y=(Cnames[i])),data=subset(Credit_Card_Data))
  +stat_boxplot(geom="errorbar",width=0.5)
  +geom_boxplot(outlier.colour = "red",fill="grey",outlier.shape = 18,outlier.size = 1,notch =FALSE)
  +theme(legend.position = "bottom")+labs(y=Cnames[i])
  +ggtitle(paste("BOX PLOT FOR",Cnames[i])))
}

#Displaying boxplot for outlier check
  gridExtra::grid.arrange(gn1)
  gridExtra::grid.arrange(gn2)
  gridExtra::grid.arrange(gn3)
  gridExtra::grid.arrange(gn4)
  gridExtra::grid.arrange(gn5)
  gridExtra::grid.arrange(gn6)
  gridExtra::grid.arrange(gn7)
  gridExtra::grid.arrange(gn8)
  gridExtra::grid.arrange(gn9)
  gridExtra::grid.arrange(gn10)
  gridExtra::grid.arrange(gn11)
  gridExtra::grid.arrange(gn12)
  gridExtra::grid.arrange(gn13)
  gridExtra::grid.arrange(gn14)
  gridExtra::grid.arrange(gn15)
  gridExtra::grid.arrange(gn16)
  gridExtra::grid.arrange(gn17)
  
#Replacing outliers with NA present in the dataset as this is the best method to deal with the NA as taught in lecture because on deleting outliers we could loose some of the important data.

    for(i in Cnames)
  {  
    Credit_Card_Data[,i]=as.data.frame(Credit_Card_Data[,i])
    Val=Credit_Card_Data[,i][Credit_Card_Data[,i] %in% boxplot.stats(as.list(Credit_Card_Data[,i])$out)]
    
    Credit_Card_Data[,i][Credit_Card_Data[,i] %in% Val]=NA
  }
  
#NA values computation
Credit_Card_Data=knnImputation(Credit_Card_Data,k=3)
  
#ReCheck for missing values
  sum(is.na(Credit_Card_Data))
  
#Check for Dataset dimensions
  dim(Credit_Card_Data)
  
----------------------------------------------------------------------------------
################ ADVANCE DATA PREPARATION (CREATING NEW VARIABLES) ##############
----------------------------------------------------------------------------------
    
  Credit_Card_Data$MONTHLY_AVERAGE_PURCHASE = Credit_Card_Data$PURCHASES/(Credit_Card_Data$PURCHASES_FREQUENCY*Credit_Card_Data$TENURE)
  Credit_Card_Data$LIMIT_USAGE=Credit_Card_Data$BALANCE/Credit_Card_Data$CREDIT_LIMIT
  Credit_Card_Data$PAYMENTS_TO_MINIMUM_PAYMENT_RATIO = Credit_Card_Data$PAYMENTS/Credit_Card_Data$MINIMUM_PAYMENTS
  Credit_Card_Data$AVERAGE_AMOUNT_PER_PURCHASE=Credit_Card_Data$PURCHASES/Credit_Card_Data$PURCHASES_TRX
  Credit_Card_Data$MONTHLY_CASH_ADVANCE = Credit_Card_Data$CASH_ADVANCE/(Credit_Card_Data$CASH_ADVANCE_FREQUENCY*Credit_Card_Data$TENURE)
  Credit_Card_Data$MONTHLY_INSTALLMENTS_PURCHASES = Credit_Card_Data$INSTALLMENTS_PURCHASES/(Credit_Card_Data$PURCHASES_INSTALLMENTS_FREQUENCY*Credit_Card_Data$TENURE)
  Credit_Card_Data$MONTHLY_ONEOFF_PURCHASES = Credit_Card_Data$ONEOFF_PURCHASES/(Credit_Card_Data$ONEOFF_PURCHASES_FREQUENCY*Credit_Card_Data$TENURE)
  Credit_Card_Data$MONTHLY_CASH_ADVANCE_TRANSACTION = Credit_Card_Data$CASH_ADVANCE/Credit_Card_Data$CASH_ADVANCE_TRX
  
#Checking no of observations and variables present in the object Credit_Card_Data
  dim(Credit_Card_Data)
  
#writing the data into file after preparing Advance data
  write.csv(Credit_Card_Data,"Advance_KPI_DataSet.csv",row.names=T)
  
#Copying data and storing KPI in Advance_KPI_Dataset
  Advance_KPI_Dataset=Credit_Card_Data

#Dropping derived KPI from the data set to use actual data for futher computation purpose  
Credit_Card_Data = subset(Credit_Card_Data, select = -c(PAYMENTS_TO_MINIMUM_PAYMENT_RATIO,AVERAGE_AMOUNT_PER_PURCHASE,MONTHLY_INSTALLMENTS_PURCHASES,MONTHLY_CASH_ADVANCE_TRANSACTION,MONTHLY_CASH_ADVANCE,MONTHLY_ONEOFF_PURCHASES,LIMIT_USAGE,MONTHLY_AVERAGE_PURCHASE))
dim(Credit_Card_Data)

-----------------------------------------------------------------------------------------
########################## FEATURE SELECTION ######################################
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
############################ CORRELATION PLOT ######################################
-----------------------------------------------------------------------------------------
  
#Selecting only numeric variables   
  Numeric_index= sapply(Credit_Card_Data,is.numeric)
  Numeric_data=Credit_Card_Data[,Numeric_index]
  #Variables names containing numeric data
  Cnames=colnames(Numeric_data)
  Cnames

#Correlation plot 
#Variables are reduced by Factor Analysis technique.
#So, not dropping variables that are highly correlated with more than one variables
corrgram(Credit_Card_Data[,Cnames],order=F,upper.panel=panel.pie,text.panel=panel.txt,main="CORRELATION PLOT")

#Checking no of observations and variables present in the object Credit_Card_Data
dim(Credit_Card_Data)

-----------------------------------------------------------------------------------------
########################## FACTOR ANALYSIS ######################################
-----------------------------------------------------------------------------------------
    
  install.packages('psych')
  install.packages('GPArotation')
  library(psych)
  library(GPArotation)

  
#We'll be using "Psych" package's "fa.parallel" function to execute parallel analysis.It will also find acceptable number of factors and generate the "scree plot"
  Parallel_Analysis=fa.parallel(Credit_Card_Data[-1], fm = 'ml', fa = 'fa')
  
#The blue line shows eigenvalues of actual data and the two red lines (placed on top of each other) show simulated and resampled data.
#Here we look at the large drops in the actual data and spot the point where it levels off to the right.Also we locate the point of inflection - the point where the gap between simulated data and actual data tends to be minimum.
#Looking at scree plot and parallel analysis, anywhere between 3 to 6 factors.Factors would be good choice.
#Parallel analysis suggests that the maximum  number of factors =  6
  
#Create the correlation matrix 
  Credit_Card_Cor = cor(Credit_Card_Data[-1])
  write.csv(Credit_Card_Cor,"Customer_Correlation_Matrix.csv",row.names = T)
  
#display correlation matrix
  Credit_Card_Cor
  
#In order to perform factor analysis, we'll use `psych` package's  `fa()function. Given below are the arguments we'll supply:
#r- Raw data or correlation or covariance matrix
#nfactors - Number of factors to extract
#rotate - Although there are various types rotations, `Varimax` and `Oblimin` are most popular
#fm - One of the factor extraction techniques like `Minimum Residual (OLS)`, `Maximum Liklihood`, `Principal Axis` etc.  
#In this case, we will select oblique rotation (rotate = "oblimin") as we believe that there is correlation in the factors.
  
  Factor_Analysis=fa(r=Credit_Card_Data[-1],nfactors =6,rotate = "verimax",fm="ml")
  print(Factor_Analysis)
  
#we need to consider the loadings more than 0.3 and not loading on more than one factor
  print(Factor_Analysis$loadings,cutoff = 0.5)
  print(Factor_Analysis)

 
#Writing loadings value in the csv file
  Loadings=data.frame(unclass(Factor_Analysis$loadings))
  Loadings=cbind(rownames(Loadings),Loadings)
  rownames(Loadings)=NULL
  colnames(Loadings)[1]=" Features "
  write.csv(Loadings,"Factor_Analysis_Loadings.csv",row.names = T)
  
#Diagramatical view of factor analysis
  fa.diagram(Factor_Analysis)
  
------------------------------------------------------------------------------------------  
##################### CLUSTERING ALGORITHM #################################
-------------------------------------------------------------------------------------------  
  install.packages("NbClust")
  library(NbClust)
  
#Dropping variables as per Factor Analysis 
  Credit_Card_Data = subset(Credit_Card_Data, select = -c(BALANCE_FREQUENCY,PURCHASES_TRX,CREDIT_LIMIT,MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,TENURE))
  
# Standarizing data 
  Credit_Card_New_Data=data.frame(scale(Credit_Card_Data[-1]))
  
#Displaying Standardized data
  Credit_Card_New_Data
  
#Extracting no of clusters to build using Factor loadings because using dataset directly is giving me error of not enough RAM space
  NBclust_Creditcard=NbClust(Factor_Analysis$loadings,min.nc=2,max.nc=6,method="kmeans")
  
#Barplot to analyse the optimum clusters
 barplot(table(NBclust_Creditcard$Best.n[1,]),xlab="No of Cluster",ylab="No of Criteria",main="No of clusters")
  
#K-means clustering
#According to the majority rule, the best number of clusters suggested by NbClust method = 4 
  Kmeans_model=kmeans( Credit_Card_New_Data,4, nstart=25)
  
#Writing cluster_data into csv file which contains clustering information of the data
  Cluster_data =data.frame( Credit_Card_New_Data,Kmeans_model$cluster)
  Cluster_data
  Cluster_data=cbind(rownames(Cluster_data),Cluster_data)
  rownames(Cluster_data)=NULL
  colnames(Cluster_data)[1]=" Features "
  write.csv(Cluster_data,"Cluster_Data_Of_Creditcard_Holders.csv",row.names = T)
 
#summarizing model
  Kmeans_model
  table(Kmeans_model$cluster)
  dim(Credit_Card_Data)
  
#Cluster VisualiZation 
  install.packages("factoextra")
  library(factoextra)
  library(cluster)
  fviz_cluster(Kmeans_model, data = Credit_Card_New_Data, geom = "point",stand = FALSE, frame.type = "norm") + theme_bw()      
 
 
#######################################################################################

  