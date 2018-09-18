#SYS 6018: Applied Data Mining
#Competition 2: House Prices

#Importing the required packages
library(readr)
library(dplyr)
library(BBmisc)
library(caret)

#Setting working directory to source file location
setwd("E:/Fall Term/SYS 6018 Applied Data Mining/Competitions/sys6018-competition-house-prices/Code")
#Moving up one folder
setwd('..')

#Importing the training and the testing datasets
traindata = read_csv("Datasets/train.csv")
testdata = read_csv("Datasets/test.csv")

#Combining the training and testing data for cleaning and pre-processing and dropping the sale price column from the train data
data <- rbind(traindata[,-c(ncol(traindata))], testdata)

#Filling in the missing values for the data

#Are there any missing values
any(is.na(data))

#Number of missing values
sum(is.na(data))

#Columns that have missing values
na.cols = as.data.frame(which(colSums(is.na(data)) > 0))
na.cols$Name = rownames(na.cols)
names(na.cols) = c("ColumnNum", "Name")
rownames(na.cols) = NULL

#Setting the missing values in MSZoning to 'OTH'
data$MSZoning[is.na(data$MSZoning)] = 'OTH'

#Getting the index of missing LotFrontage values
index <- which(is.na(data$LotFrontage))
#Grouping by neighborhood and calculating the median value for each neighborhood
frontage_by_hood <- data %>% 
  dplyr::select(Neighborhood, LotFrontage) %>% 
  group_by(Neighborhood) %>% 
  summarise(median_frontage = median(LotFrontage, na.rm = TRUE))
#Imputing the median value for the missing values
for (i in index){
  med_frontage = frontage_by_hood[frontage_by_hood == data$Neighborhood[i], 'median_frontage']
  data[i, 'LotFrontage'] = med_frontage[[1]]
}

#Setting the missing values in Alley to 'None'
data$Alley[is.na(data$Alley)] = 'None'

#Setting the missing values in Utilitis to the most common value
#Defining mode function to get the most common value
mode = function(x) {
  t = table(x) 
  return(names(t)[t==max(t)])
}
data$Utilities[is.na(data$Utilities)] = mode(data$Utilities)

#Setting the missing values in Exterior1st to Other
data$Exterior1st[is.na(data$Exterior1st)] = 'Other'

#Setting the missing values in Exterior2nd to Other
data$Exterior2nd[is.na(data$Exterior2nd)] = 'Other'

#Setting the missing values in MasVnrType to 'None'
data$MasVnrType[is.na(data$MasVnrType)] = 'None'

#Setting the area equal to zero if MasVnrType is None
data[data$MasVnrType %in% 'None',]$MasVnrArea = 0

#Setting the NA values in BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1,  BsmtFinType2 to 'None'
data$BsmtQual[is.na(data$BsmtQual)] = 'None'
data$BsmtCond[is.na(data$BsmtCond)] = 'None'
data$BsmtExposure[is.na(data$BsmtExposure)] = 'None'
data$BsmtFinType1[is.na(data$BsmtFinType1)] = 'None'
data$BsmtFinType2[is.na(data$BsmtFinType2)] = 'None'

#Setting the NA values in TotalBsmtSF to zero
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] = 0

#Setting the other basement variables to 0 if area is 0
data$BsmtFinSF1[data$TotalBsmtSF == 0] = 0
data$BsmtFinSF2[data$TotalBsmtSF == 0] = 0
data$BsmtUnfSF[data$TotalBsmtSF == 0] = 0
data$BsmtFullBath[data$TotalBsmtSF == 0] = 0
data$BsmtHalfBath[data$TotalBsmtSF == 0] = 0

#Setting the most common value of electrical to the missing values
data$Electrical[is.na(data$Electrical)] = mode(data$Electrical)

#Setting the KitchenQual NA values with the most common value
data$KitchenQual[is.na(data$KitchenQual)] = mode(data$KitchenQual)

#Setting the Functional NA values as 'Oth'
data$Functional[is.na(data$Functional)] = 'Oth'

#Setting the FireplaceQu NA values as 'None'
data$FireplaceQu[is.na(data$FireplaceQu)] = 'None'

#Setting the GarageType NA values as 'None'
data$GarageType[is.na(data$GarageType)] = 'None'

#Setting the GarageYrBlt
data$GarageYrBlt[is.na(data$GarageYrBlt)] = 0

#Setting the GarageFinish NA values as 'None'
data$GarageFinish[is.na(data$GarageFinish)] = 'None'

#Setting the GarageCars NA values as 0
data$GarageCars[is.na(data$GarageCars)] = 0

#Setting the GarageArea NA values as 0
data$GarageArea[is.na(data$GarageArea)] = 0

#Setting the GarageQual NA values as 'None'
data$GarageQual[is.na(data$GarageQual)] = 'None'

#Setting the GarageCond NA values as 'None'
data$GarageCond[is.na(data$GarageCond)] = 'None'

#Setting the PoolQC NA values as 'None'
data$PoolQC[is.na(data$PoolQC)] = 'None'

#Setting the Fence NA values as 'None'
data$Fence[is.na(data$Fence)] = 'None'

#Setting the MiscFeature NA values as 'None'
data$MiscFeature[is.na(data$MiscFeature)] = 'None'

#Setting the SaleType NA values as 'Oth'
data$SaleType[is.na(data$SaleType)] = 'Oth'

#Feature Engineering
#Creating a variable called TotalBath
data$TotalBath = data$FullBath + (data$HalfBath*0.5) + data$BsmtFullBath + (data$BsmtHalfBath*0.5)
#Creating a variable called PorchSqFt
data$PorchSF = data$OpenPorchSF + data$EnclosedPorch + data$`3SsnPorch` + data$ScreenPorch

#Dropping the individual variables
data$FullBath = NULL
data$HalfBath = NULL
data$BsmtFullBath = NULL
data$BsmtHalfBath = NULL
data$OpenPorchSF = NULL
data$EnclosedPorch = NULL
data$`3SsnPorch` = NULL
data$ScreenPorch = NULL

#Dropping ID since it is not useful
data$Id = NULL

#Converting all character variables to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

#Creating dummy variables for the categorical variables
dmy <- dummyVars(" ~ .", data = data, sep = "_", fullRank = TRUE)
data_new <- data.frame(predict(dmy, newdata = data))

#Normalising numerical variables
ndata = normalize(data_new, method = "standardize")

#Splitting into train and test data
train = ndata[1:1460,]
test = ndata[1461:2919,]

#Computing the euclidean distance
euclidean_distance = function(x, y){
  sum = 0   #Inititalising sum
  for(i in 1:length(test)){  #Length gives the no of columns
    sum = sum + (x[i] - y[i])^2   #Computing pair-wise distance across all the columns
    distance = sqrt(sum)    #Taking the square root
  }
  return(distance)
}

#KNN function
knn = function(test, train, k){ 
  preds = c() #Initialising a prediction vector
  for(i in 1:nrow(test)){
    ed = as.data.frame(matrix(ncol = 2, nrow = nrow(train)))   #Defining a data frame to store the row number and distance across the training data
    names(ed) = c("obs", "distance")    #Changing the names
    for(j in 1:nrow(train)){
      ed$obs[j] = j   #Row number
      ed$distance[j] = euclidean_distance(test[i,], train[j,])  #Euclidean distance
    }
    ed <- ed[order(unlist(ed$distance)),]   #Sorting the data frame
    ed <- ed[1:k,]  #Taking the top k neighbors
    top_k = traindata[ed$obs,]   #Subsetting the original training data with the top k neighbors to get the label of the output
    preds[i] = mean(top_k$SalePrice)  #Taking the mean value among the top k neighbors
  }
  return(preds)   
}

#Predictions on the test data
predictions = c()   #Initialising the prediction vector
predictions <- knn(test, train, 10) 

#Creating a dataframe predictions_knn
predictions_knn = data.frame(ID = testdata$Id, SalePrice = predictions)

#Writing to a csv file
write.csv(predictions_knn, file = "predictions_knn.csv", row.names = FALSE)
