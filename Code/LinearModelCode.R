#************************************************
#
#Kaggle Housing Price Project
#SYS 6018
#
#************************************************


library(readr) 
library(dplyr)

#Read in dataset, combine test and train data into one for data pre-processing
p1.train1 = read_csv("train.csv")
p1.pred = read_csv("test.csv")
p1.pred["SalePrice"] <- NA
p1.train1 = rbind(p1.train1,p1.pred)

#Find out about missing values in the dataset
miss = sapply(p1.train1, function(x) length(which(is.na(x))))
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

#Tackle the missing data in LotFrontage based on prediction from LotArea
#Find the relationship between non-missing values of LotFrontage and LotArea first with plots and model summary, 
#then use the linear model to predict the missing values in LotFrontage
all_data_LF_noNA <- p1.train1[!(is.na(p1.train1$LotFrontage)),]
all_data_LF_NA <- p1.train1[is.na(p1.train1$LotFrontage),]

plot(LotFrontage ~ LotArea, data=all_data_LF_noNA)
LF_LotArea.lm1 <- lm(LotFrontage ~ LotArea, data=all_data_LF_noNA)
summary(LF_LotArea.lm1)

LF_LotArea.lm2 <- lm(LotFrontage ~ I(sqrt(LotArea)), data=all_data_LF_noNA)
summary(LF_LotArea.lm2)

all_data_LF_NA$LotFrontage <- predict(LF_LotArea.lm2,newdata=data.frame(LotArea=all_data_LF_NA$LotArea))
plot(LotFrontage ~ LotArea, data=all_data_LF_NA)

all_data <- rbind(all_data_LF_noNA,all_data_LF_NA)
#Now the relationship 
plot(LotFrontage ~ LotArea, data=all_data)
plot(SalePrice ~ LotFrontage, data=all_data_LF_NA)

# We also impute missing MasVnrArea as 0 as for all such cases MasVnrType is NA, similarly 
#we treat missing MasVnrType values as "None"; this is applied to the remaining dataset for the other predictors as well 
for (i in 1:length(all_data$Id)) {
  if (is.na(all_data$MasVnrArea[i])) {
    all_data$MasVnrArea[i] <- 0
  } 
  if (is.na(all_data$Alley[i])) {
    all_data$Alley[i] <- 'None'
  } 
  if (is.na(all_data$MasVnrType[i])) {
    all_data$MasVnrType[i] <- 'None' 
  }
  if (is.na(all_data$BsmtQual[i])) {
    all_data$BsmtQual[i] <- 'None'
  } 
  if (is.na(all_data$BsmtCond[i])) {
    all_data$BsmtCond[i] <- 'None'
  }
  if (is.na(all_data$BsmtExposure[i])) {
    all_data$BsmtExposure[i] <- 'None'
  } 
  if (is.na(all_data$BsmtFinType1[i])) {
    all_data$BsmtFinType1[i] <- 'None'
  }
  if (is.na(all_data$BsmtFinType2[i])) {
    all_data$BsmtFinType2[i] <- 'None'
  }
  if (is.na(all_data$FireplaceQu[i])) {
    all_data$FireplaceQu[i] <- 'None'
  } 
  if (is.na(all_data$GarageType[i])) {
    all_data$GarageType[i] <- 'None'
  } 
  if (is.na(all_data$GarageFinish[i])) {
    all_data$GarageFinish[i] <- 'None'
  } 
  if (is.na(all_data$GarageQual[i])) {
    all_data$GarageQual[i] <- 'None'
  } 
  if (is.na(all_data$GarageCond[i])) {
    all_data$GarageCond[i] <- 'None'
  }
  if (is.na(all_data$PoolQC[i])) {
    all_data$PoolQC[i] <- 'None'
  }
  if (is.na(all_data$Fence[i])) {
    all_data$Fence[i] <- 'None'
  }
  if (is.na(all_data$MiscFeature[i])) {
    all_data$MiscFeature[i] <- 'None'
  }
  
  if (is.na(all_data$MSZoning[i])) {
    all_data$MSZoning[i] <- 'None'
  } 
  if (is.na(all_data$Utilities[i])) {
    all_data$Utilities[i] <- 'None' 
  }
  if (is.na(all_data$Exterior1st[i])) {
    all_data$Exterior1st[i] <- 'None'
  } 
  if (is.na(all_data$Exterior2nd[i])) {
    all_data$Exterior2nd[i] <- 'None'
  }
  if (is.na(all_data$BsmtFinSF1[i])) {
    all_data$BsmtFinSF1[i] <- 0
  } 
  if (is.na(all_data$BsmtFinSF2[i])) {
    all_data$BsmtFinSF2[i] <- 0
  }
  if (is.na(all_data$BsmtUnfSF[i])) {
    all_data$BsmtUnfSF[i] <- 0
  }
  if (is.na(all_data$TotalBsmtSF[i])) {
    all_data$TotalBsmtSF[i] <- 0
  } 
  if (is.na(all_data$Electrical[i])) {
    all_data$Electrical[i] <- 'None'
  } 
  if (is.na(all_data$BsmtFullBath[i])) {
    all_data$BsmtFullBath[i] <- 0
  } 
  if (is.na(all_data$BsmtHalfBath[i])) {
    all_data$BsmtHalfBath[i] <- 0
  } 
  if (is.na(all_data$KitchenQual[i])) {
    all_data$KitchenQual[i] <- 'None'
  }
  if (is.na(all_data$Functional[i])) {
    all_data$Functional[i] <- 'None'
  }
  if (is.na(all_data$GarageCars[i])) {
    all_data$GarageCars[i] <- 0
  }
  if (is.na(all_data$GarageArea[i])) {
    all_data$GarageArea[i] <- 0
  }
  if (is.na(all_data$SaleType[i])) {
    all_data$SaleType[i] <- 'None'
  }
}


#We categorize yearbuilt, yearremoteadd and garage year built into 3 or 4 categories based on periods since there's too many values in year to be factorized;
#For garage year built, if it's NA, we indicate it as "NoGar"
p1.train2 <- all_data %>%
  mutate(
    `YearBuilt` = case_when(all_data$YearBuilt < 1900 ~ "Before1900", 
                            all_data$YearBuilt >= 1900 & all_data$YearBuilt < 1950 ~ "1900-1950",
                            all_data$YearBuilt >= 1950 & all_data$YearBuilt < 2000 ~ "1950-2000",
                            all_data$YearBuilt >= 2000 ~ "After2000"))

p1.train3 <- p1.train2 %>%
  mutate(
    `YearRemodAdd` = case_when(p1.train2$YearRemodAdd < 1975 ~ "1950-1975", 
                            p1.train2$YearRemodAdd >= 1975 & p1.train2$YearRemodAdd < 2000 ~ "1975-2000",
                            p1.train2$YearRemodAdd >= 2000 ~ "After2000"))

p1.train4 <- p1.train3 %>%
  mutate(
    `GarageYrBlt` = case_when(p1.train3$GarageYrBlt < 1900 ~ "Before1900", 
                               p1.train3$GarageYrBlt >= 1900 & p1.train3$GarageYrBlt < 1950 ~ "1900-1950",
                               p1.train3$GarageYrBlt >= 1950 & p1.train3$GarageYrBlt < 2000 ~ "1950-2000",
                               p1.train3$GarageYrBlt >= 2000 ~ "After2000",
                               p1.train3$GarageYrBlt == "None" ~ "NoGar"))

#Now we factorize the remaining of the variables that are not numeric; note that we deem predictors such as number of bathrooms 
#to be categories as well

p1.train4$MSSubClass<-factor(p1.train4$MSSubClass)
class(p1.train4$MSSubClass)
p1.train4$OverallQual<-factor(p1.train4$OverallQual)
class(p1.train4$OverallQual)
p1.train4$OverallCond<-factor(p1.train4$OverallCond)
class(p1.train4$OverallCond)
p1.train4$YearBuilt<-factor(p1.train4$YearBuilt)
class(p1.train4$YearBuilt)
p1.train4$YearRemodAdd<-factor(p1.train4$YearRemodAdd)
class(p1.train4$YearRemodAdd)
p1.train4$BsmtFullBath<-factor(p1.train4$BsmtFullBath)
class(p1.train4$BsmtFullBath)
p1.train4$BsmtHalfBath<-factor(p1.train4$BsmtHalfBath)
class(p1.train4$BsmtHalfBath)
p1.train4$FullBath<-factor(p1.train4$FullBath)
class(p1.train4$FullBath)
p1.train4$YrSold<-factor(p1.train4$YrSold)
class(p1.train4$YrSold)
p1.train4$HalfBath<-factor(p1.train4$HalfBath)
class(p1.train4$HalfBath)
p1.train4$KitchenAbvGr<-factor(p1.train4$KitchenAbvGr)
class(p1.train4$KitchenAbvGr)
p1.train4$BedroomAbvGr<-factor(p1.train4$BedroomAbvGr)
class(p1.train4$BedroomAbvGr)
p1.train4$TotRmsAbvGrd<-factor(p1.train4$TotRmsAbvGrd)
class(p1.train4$TotRmsAbvGrd)
p1.train4$Fireplaces<-factor(p1.train4$Fireplaces)
class(p1.train4$Fireplaces)
p1.train4$GarageYrBlt<-factor(p1.train4$GarageYrBlt)
class(p1.train4$GarageYrBlt)
p1.train4$GarageCars<-factor(p1.train4$GarageCars)
class(p1.train4$GarageCars)
p1.train4$MoSold<-factor(p1.train4$MoSold)
class(p1.train4$MoSold)
p1.train4$MSZoning<-factor(p1.train4$MSZoning)
p1.train4$Street<-factor(p1.train4$Street)
p1.train4$Alley<-factor(p1.train4$Alley)
p1.train4$LandContour<-factor(p1.train4$LandContour)
p1.train4$LotShape<-factor(p1.train4$LotShape)
p1.train4$Utilities<-factor(p1.train4$Utilities)
p1.train4$LotConfig<-factor(p1.train4$LotConfig)
p1.train4$LandSlope<-factor(p1.train4$LandSlope)
p1.train4$Neighborhood<-factor(p1.train4$Neighborhood)
p1.train4$BldgType<-factor(p1.train4$BldgType)
p1.train4$HouseStyle<-factor(p1.train4$HouseStyle)
p1.train4$RoofStyle<-factor(p1.train4$RoofStyle)
p1.train4$RoofMatl<-factor(p1.train4$RoofMatl)
p1.train4$Exterior1st<-factor(p1.train4$Exterior1st)
p1.train4$Exterior2nd<-factor(p1.train4$Exterior2nd)
p1.train4$MasVnrType<-factor(p1.train4$MasVnrType)
p1.train4$ExterQual<-factor(p1.train4$ExterQual)
p1.train4$ExterCond<-factor(p1.train4$ExterCond)
p1.train4$Foundation<-factor(p1.train4$Foundation)
p1.train4$BsmtQual<-factor(p1.train4$BsmtQual)
p1.train4$BsmtCond<-factor(p1.train4$BsmtCond)
p1.train4$BsmtExposure<-factor(p1.train4$BsmtExposure)
p1.train4$BsmtFinType1<-factor(p1.train4$BsmtFinType1)
p1.train4$BsmtFinType2<-factor(p1.train4$BsmtFinType2)
p1.train4$Heating<-factor(p1.train4$Heating)
p1.train4$HeatingQC<-factor(p1.train4$HeatingQC)
p1.train4$CentralAir<-factor(p1.train4$CentralAir)
p1.train4$Electrical<-factor(p1.train4$Electrical)
p1.train4$KitchenQual<-factor(p1.train4$KitchenQual)
p1.train4$Functional<-factor(p1.train4$Functional)
p1.train4$FireplaceQu<-factor(p1.train4$FireplaceQu)
p1.train4$GarageType<-factor(p1.train4$GarageType)
p1.train4$GarageFinish<-factor(p1.train4$GarageFinish)
p1.train4$GarageQual<-factor(p1.train4$GarageQual)
p1.train4$GarageCond<-factor(p1.train4$GarageCond)
p1.train4$PavedDrive<-factor(p1.train4$PavedDrive)
p1.train4$PoolQC<-factor(p1.train4$PoolQC)
p1.train4$Fence<-factor(p1.train4$Fence)
p1.train4$MiscFeature<-factor(p1.train4$MiscFeature)
p1.train4$SaleType<-factor(p1.train4$SaleType)
p1.train4$SaleCondition<-factor(p1.train4$SaleCondition)

#Now we check our new dataset's predictor types
lv = sapply(p1.train4, function(x) levels(x))
lv

# Select subset to seperate the training set and the test set 
p1.train <- p1.train4[!is.na(p1.train4$SalePrice),]    
p1.pred <- p1.train4[is.na(p1.train4$SalePrice),]

# Select subset for cross-validation
sub1 <- sample(1:1460,size=730)
p1.train <- p1.train[sub1,]     
p1.test <- p1.train[-sub1,]

#Types of dwelling could affect price of house, hence plot a graph to see if there's a huge variance among different dwelling types
MS1<- p1.train %>% group_by(MSSubClass) %>% summarise(Count = n())
MS1$MSSubClass <- as.factor(MS1$MSSubClass)
plot(MS1$MSSubClass, MS1$Count)

#Neighborhood could affect price of house, hence plot a graph to see if there's a huge variance among different neighborhood
NH<- p1.train %>% group_by(Neighborhood) %>% summarise(Count = n())
NH$Neighborhood <- as.factor(NH$Neighborhood)
plot(NH$Neighborhood, NH$Count)

#For the above two categories dwelling and neiborhood, we deem them to be important to include judging from variability in the graphs against Saleprice
#Now we treat categorical variables that have less values and with their values as gradient levels
#We want to know with the levels of these variables increasing, whether the saleprice will increase too

group.cond <- p1.train %>%
  group_by(OverallCond) %>%
  summarise(mean_p = mean(SalePrice), n = n()) 
group.cond
# 
# OverallCond  mean_p     n
# <fct>         <dbl> <int>
#   1 1            61000      1
# 2 2           182811.     3
# 3 3           112246.    13
# 4 4           122256     25
# 5 5           205751.   403
# 6 6           152381.   136
# 7 7           157811.    97
# 8 8           155624.    39
# 9 9           215669.    13

group.qual <- p1.train %>%
  group_by(OverallQual) %>%
  summarise(mean_p = mean(SalePrice), n = n())  
group.qual

# OverallQual  mean_p     n
# <fct>         <dbl> <int>
#   1 1            50150      2
# 2 2            60000      2
# 3 3            94722.     8
# 4 4           108675.    60
# 5 5           135563.   196
# 6 6           166342.   192
# 7 7           207035.   154
# 8 8           270221.    82
# 9 9           373121.    27
# 10 10          403540      7


group.built <- p1.train %>%
  group_by(YearBuilt) %>%
  summarise(mean_p = mean(SalePrice), n = n())  
group.built

# A tibble: 4 x 3
# YearBuilt   mean_p     n
# <fct>        <dbl> <int>
#   1 1900-1950  128354.   147
# 2 1950-2000  170606.   375
# 3 After2000  241106    202
# 4 Before1900 233063.     6

#The conclusion is for overall quality, overall condition and year built, the sale price would increase with their levels increasing

#Now we move on to correlation analysis for numerical predictors
nums <- unlist(lapply(p1.train, is.numeric))  
p2.train<-p1.train[ , nums] 

#Get a list of numerical values, then get a correlation chart
Var <- cor(p2.train, use="pairwise.complete.obs")

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(Var[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
Var1 <- Var[CorHigh, CorHigh]

#plot the correlation chart
library(corrplot)
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
#As we can see from the chart, Garage area, ground living area, total basement square feet and 1st floor square feet
#are more relevant to sale price

#We applied log to sale price to obtain more precision since the original price data was too sparsed
p1.train$Saleprice<-log(as.numeric(p1.train$SalePrice),base=exp(1))

#We combine our previous analysis to come up with the following model
p1.lm <- lm(Saleprice ~ MSSubClass+GarageArea+OverallQual+OverallCond+GrLivArea+YearBuilt+TotalBsmtSF+X1stFlrSF, data=p1.train)
summary(p1.lm)
mse11 <- mean(p1.lm$residuals^2)
mse11
#The mean squared error is 0.02204817.

#We use the step function to optimize the multi-linear model
step(p1.lm)

#Now we update p1.lm
p1.lm<-lm(formula = Saleprice ~ LotArea + OverallQual + OverallCond + GrLivArea + YearBuilt, data = p1.train)
mse1 <- mean(p1.lm$residuals^2)
mse1
#The mean square error is 0.02356937.


p1<-as.vector(predict(p1.lm,newdata=p1.test, type="response"))
mse2 <- mean((p1-p1.test$Saleprice)^2)
mse2
#The cross-validated result is 0.02626073. Which aligns with our previous conclusions. Hence we go with this model. 

#Make the prediction with appropriate values and headers
preds<-as.vector(predict(p1.lm,newdata=p1.pred, type="response"))
predsunlog<-exp(preds) #Retain the previously logged sale price
prediction<-cbind.data.frame(p1.pred$Id,predsunlog)
colnames(prediction) <- c("Id", "SalePrice")
write.csv(prediction,file="predictions_lr.csv")

