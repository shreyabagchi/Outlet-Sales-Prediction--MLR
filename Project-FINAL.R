###################################### Project ################################

#Setting the work directory
setwd("C:\\MBS-Rutgers programs\\Regression Analysis\\Project\\Data Mart")

#Importing data into R
data = read.csv("Train_UWu5bXk.csv",header=T)

#Displaying the first few rows of the data
head(data)

#Looking at the structure of data set
str(data)
summary(data)

##***************************************************************************##
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
##***************************************************************************##
# Checking for issues:
# NA values
colSums(is.na(data))
is.na(data)
# Unique values 
unique(data$Outlet_Size)

################ Item_Visibility ##########################
library(data.table)

#Replacing the zero value in Item_Visibility variable with the mean value of the item_visibility for that specific item
#Converting dataset into dataframe
df <- data.frame(data, stringsAsFactors = FALSE)
df

#Filtering out the records with 0 item_visibility so as to take the mean of the item excluding zero
df_filtered = subset(df,Item_Visibility != 0)
df_filtered

#Taking the mean of item_visibility for that specific item after removing 0 value records
data_mean_itmvisibility <- setDT(df_filtered)[, mean(Item_Visibility), by = Item_Identifier]
data_mean_itmvisibility

#Merging the above mean data back into dataset
merged_data <- merge(df,data_mean_itmvisibility, by="Item_Identifier")
merged_data

##write.csv(merged_data, file = "SalesDatamart.csv")

#Creating a new column for Item_Visibility with zero treated with mean of item visibilty
merged_data$Item_Visibility1 <- ifelse(merged_data$Item_Visibility == 0,merged_data$V1,merged_data$Item_Visibility)

###############Item_Fat_Content############################
#To check distinct value in the variable
unique(merged_data$Item_Fat_Content)

#To change the variable value to same format like "LF", "low fat" to "Low Fat" and "reg" to "Regular"
merged_data$Item_Fat_Content <- gsub("LF", "Low Fat", merged_data$Item_Fat_Content)
merged_data$Item_Fat_Content <- gsub("low fat", "Low Fat", merged_data$Item_Fat_Content)
merged_data$Item_Fat_Content <- gsub("reg", "Regular", merged_data$Item_Fat_Content)

#To check distinct value in the variable now it shows the values are in sync
unique(merged_data$Item_Fat_Content)

#############Item_Type#####################################
#Creating a broader group on item type by classifying them into three groups drink, food and Non-Consumable
merged_data$ItemGroup <- substr(merged_data$Item_Identifier, 1, 2)

#############Item Weight#####################################
#Filtering out the records with NA item_weight so as to take the mean of the item excluding NA
df_filtered1 = subset(merged_data,!is.na(merged_data$Item_Weight))
df_filtered1
sum(is.na(df_filtered1$Item_Weight))

#Taking the mean of item_weight for that specific item after removing NA value records
data_mean_itmWeight <- setDT(df_filtered1)[, mean(Item_Weight), by = Item_Identifier]
data_mean_itmWeight

#Merging the above mean data back into dataset
merged_data1 <- merge(merged_data,data_mean_itmWeight, by="Item_Identifier")
merged_data1

#replacing Item_Weight by newly created V1 column 
merged_data1$Item_Weight<-merged_data1$V1.y

#finally there 8519 records, excluding 4 records having identifiers as "FDE52", FDK57, FDN52, FDQ60 as they had only one record of null value and didn't had any reference value to same item identifier which can be replaced

################Dropping extra columns#############
#Removing V1.y column as it is an intermediate value of Item_Weight and its treated value is present in the column of "Item_Weight"
merged_data1$V1.y<-NULL 
#Removing V1.x column as it is an intermediate value of Item_Visibilty and its treated value is present in the column of "Item_Visibility1"
merged_data1$V1.x<-NULL
#Removing Item Visibility as its new treated column is created as Item_Visibilty1
merged_data1$Item_Visibility<-NULL
#Removing Item_Type column as new column "Item_Group" is created containing the three categories of DR, FD, NC
merged_data1$Item_Type<-NULL
#Removing Outlet_Size column as it has ~28% missing values in it
merged_data1$Outlet_Size<-NULL

#Exporting the cleaned data
write.csv(merged_data1, file = "cleaned_data.csv")

##***************************************************************************##
#                                                                             #
#                             Exploratory Data Analysis                       #
#                                                                             #
##***************************************************************************##

#Looking at the overall summary of the data
summary(merged_data1)

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

#Analysing the density distribution of the item_outlet_sales
#The plot showed left skewness
plot(density(merged_data1$Item_Outlet_Sales))

#Apply transformation to rectify the response variable

#Slightly better result but still skewed
plot(density(log(merged_data1$Item_Outlet_Sales)))

#Skewness is resolved future using sqrt
plot(density(sqrt(merged_data1$Item_Outlet_Sales)))

#Using box plot to treat the outliers
b = boxplot(sqrt(merged_data1$Item_Outlet_Sales))
c = b$out
min(c)

#less than min(c) must be kept
treated_data<-merged_data1[which(sqrt(merged_data1$Item_Outlet_Sales) < min(c)),]
#8499 records remaining after dropping outliers
length(treated_data$Item_Outlet_Sales)

#Checking the response variable (item_outlet_sales) distribution after treating it 
boxplot(sqrt(treated_data$Item_Outlet_Sales))
plot(density(sqrt(treated_data$Item_Outlet_Sales)), main = "Distribution of item_outlet_sales") # #item_outlet_sales is fairly normally distributed

#Creating a transformed column of item_outlet_sales using square root
treated_data$Transfmd_IO_Sales = sqrt(treated_data$Item_Outlet_Sales)
treated_data

##################Item_Fat_Content#####################################
# Histogram of Item_Fat_Content
counts <- table(treated_data$Item_Fat_Content)
barplot(counts, main = "Histogram of Item_Fat_Content", 
        xlab = "Item_Fat_Content",col = "blue")

#Boxplot of Item_Fat_Content
boxplot(Transfmd_IO_Sales~Item_Fat_Content,data=treated_data, main="Sales Data", 
        xlab = "Item_Fat_Content", ylab = "sqrt(Item_Outlet_Sales)")

#Intrepretation
#Low fat content have higher frequency than regular fat content
#Median of both low fat content and regular fat content lie around 40 of sqrt(Item_Outlet_Sales)

##################Outlet_Establishment_Year#####################################
# Histogram of Outlet_Establishment_Year
counts <- table(treated_data$Outlet_Establishment_Year)
barplot(counts, main="Histogram of Outlet_Establishment_Year", 
        xlab="Outlet_Establishment_Year",col="blue")

# Boxplot of Outlet_Establishment_Year
boxplot(Transfmd_IO_Sales~Outlet_Establishment_Year,data=treated_data, main="Sales Data", 
        xlab="Outlet_Establishment_Year", ylab="sqrt(Item_Outlet_Sales)")

#Intrepretation
#Year 1998 had the lowest sales among all the years
#Median of rest of the years lie approximately around 40 to 45 of sqrt(Item_Outlet_Sales)

##################ItemGroup#####################################
# Histogram of ItemGroup
counts <- table(treated_data$ItemGroup)
barplot(counts, main="Histogram of ItemGroup", 
        xlab="ItemGroup",col="blue")

# Boxplot of ItemGroup
boxplot(Transfmd_IO_Sales~ItemGroup,data=treated_data, main="Sales Data", 
        xlab="ItemGroup", ylab="sqrt(Item_Outlet_Sales)")

#Intrepretation
#Histogram of Item Group showed that Food has the highest frequency which is followed by Non-Consumable items and then Drinks.  
#Median of Item Group lie approximately around 40 to 45 of sqrt(Item_Outlet_Sales)

##################Outlet_Location_Type#####################################
#Histogram of Outlet_Location_Type
counts <- table(treated_data$Outlet_Location_Type)
barplot(counts, main="Histogram of Outlet_Location_Type", 
        xlab="Outlet_Location_Type",col="blue")

# Boxplot of Outlet_Location_Type
boxplot(Transfmd_IO_Sales~Outlet_Location_Type,data=treated_data, main="Sales Vs Outlet_Location_Type ",xlab="Outlet_Location_Type", ylab="sqrt(Item_Outlet_Sales)")

#Intrepretation
#From histogram, The count of different locations of outlet stores in the data follows a pattern from being lowest in Tier1 followed by Tier 2 and then Tier 3 
#From boxplot, it can be seen as the median value of sales corresponding to tier1, tier2 and tier3 fall between 40 and 45. Moreover, there ocuurs some outliers pertaining to sales corresponding to tier2.

##################Outlet_Type#####################################
# Histogram of Outlet_Type
counts <- table(treated_data$Outlet_Type)
barplot(counts, main="Histogram of Outlet_Type", 
        xlab="Outlet_Type",col="blue")

# Boxplot of Outlet_Type
boxplot(Transfmd_IO_Sales~Outlet_Type,data=treated_data, main="Sales Vs Outlet_Type", 
        xlab="Outlet_Type", ylab="sqrt(Item_Outlet_Sales)")

#Intrepretation
#From histogram, the count of Supermarket Type 1 outlet came out to be significantly higher than that of other three outlet types
#From boxplot, it could be seen that the median sales pertaining to Grocery store is lower than that of other three outlets and lie in range of ~(15-20). Whereas, the median sales of other 3 outlets lie in range of ~(40-60).
#Moreover, outlers corresponding to sales in Grocery store and Supermarket1 are found.

######################################Annova - One way##################################

#Annova - To test if the levels within the variable are significantly different

#For variable Item_Fat_Content
mdl_fat_content = aov( Transfmd_IO_Sales ~ Item_Fat_Content, data=treated_data)
summary(mdl_fat_content)
TukeyHSD(mdl_fat_content)

#Intrepretation
#At 10% level, the main effects of Item_Fat_Content as well as the effects of its different levels are significant towards Item_Outlet_Sales
#p = 0.098 (main effect)

#For variable Item_Type
mdl_Item_Type=aov( Item_Outlet_Sales ~ Item_Type, data=merged_data)
summary(mdl_Item_Type)
TukeyHSD(mdl_Item_Type)

#Intrepretation
#At 5% level, the main effect of Item_Type is significant (p-value = 0.000388) but many of its individual levels are insignificant. Hence we have clubbed the variable into ItemGroup (FD,DR,NC)

#For variable ItemGroup
mdl_ItemGroup=aov( Transfmd_IO_Sales ~ ItemGroup, data=treated_data)
summary(mdl_ItemGroup)
TukeyHSD(mdl_ItemGroup)

#Intrepretation
#At 5% level, the main effect of ItemGroup is significant (p-value = 0.0033) as well as for level FD-DR but the effects of its two other levels are insignificant for NC-DR and NC-FD. 
#But, since NC (non consumable ) is a broad category, so we decided to keep this at this point and will check in the step of variable selection procedure. 

#for variable Outlet_Location_Type
mdl_Outlet_Location_Type = aov(Transfmd_IO_Sales ~ Outlet_Location_Type, data = treated_data)
summary(mdl_Outlet_Location_Type)
TukeyHSD(mdl_Outlet_Location_Type)

#Intrepretation
#At 5% level, the main effects as well as individual effects of Outlet_Location_Type are significant towards its contribution to Item_Outlet_Sales

#for variable Outlet_Type
mdl_Outlet_Type = aov(Transfmd_IO_Sales ~ Outlet_Type, data = treated_data)
summary(mdl_Outlet_Type)
TukeyHSD(mdl_Outlet_Type)

#Intrepretation
#At 5% level, the main effects of Outlet_Type as well as the effects of its different levels are significant towards Item_Outlet_Sales

######################Transposing categorical variable######################################
# Since there a couple of variables, which are factors (Item_Fat_Content, Outlet_Location_Type, Outlet_Type, ItemGroup)

#As Item_Fat_Content has only two levels so we can replace low fat with 0 and regular with 1 within a single column
treated_data$Item_Fat_Content <- gsub("Low Fat", 0, treated_data$Item_Fat_Content)
treated_data$Item_Fat_Content <- gsub("Regular",1, treated_data$Item_Fat_Content)

#Check the levels of Item_Fat_Content
head(treated_data)

#Add dummy variables(n-1) for such factors , so that they can be used in model 

############# Create dummy fields for Outlet_Location_Type #########
Field_Factor <- factor(treated_data$Outlet_Location_Type)
xField_Factor <- model.matrix(~ Field_Factor -1 )

#Check the levels of Outlet_Location_Type
head(xField_Factor)

#Get One level less
xField_Factor <-data.frame(xField_Factor)
drop_length <- length(names(xField_Factor)) - 1

#Add the dummy field in the main Table
treated_data_trans <- cbind(treated_data, xField_Factor[1:drop_length])

#Viewing the transformed data
head(treated_data_trans)

######################## Create dummy fields for ItemGroup #################

Field_Factor <- factor(treated_data_trans$ItemGroup)
xField_Factor <- model.matrix(~ Field_Factor -1 )

#Check the levels of ItemGroup
head(xField_Factor)

#Get One level less
xField_Factor <-data.frame(xField_Factor)
drop_length <- length(names(xField_Factor)) - 1

#Add the dummy field in the main Table
treated_data_trans <- cbind(treated_data_trans, xField_Factor[1:drop_length])

#Viewing the transformed data
head(treated_data_trans)

####################### Create dummy fields for Outlet_Type #####################
Field_Factor <- factor(treated_data_trans$Outlet_Type)
xField_Factor <- model.matrix(~ Field_Factor -1 )

#Check the levels of Outlet_Type
head(xField_Factor)

#Get One level less
xField_Factor <-data.frame(xField_Factor)
drop_length <- length(names(xField_Factor)) - 1

#Add the dummy field in the main Table
treated_data_trans <- cbind(treated_data_trans, xField_Factor[1:drop_length])

#Viewing the transformed data
head(treated_data_trans)

################Removing the variables that had been transformed###############
treated_data_trans$Outlet_Location_Type<-NULL 
treated_data_trans$Outlet_Type<-NULL 
treated_data_trans$ItemGroup<-NULL

#Viewing the transformed data
head(treated_data_trans)

###################### Modifying the time variable #############################
# Counting number of years from the outlet establishment year to 2013
treated_data_trans$Outlet_NumberofYears = 2013 - treated_data_trans$Outlet_Establishment_Year
head(treated_data_trans)

#Removing the redundant and unstandardized column "Outlet_Establishment_Year"
treated_data_trans$Outlet_Establishment_Year<-NULL

#Exporting the cleaned data
write.csv(treated_data_trans, file = "model_data.csv")

################################ Correlation Matrix #############################
library(corrplot)

data_df<-data.frame(train$Transfmd_IO_Sales, train$Item_Weight, as.numeric(train$Item_Fat_Content), train$Item_MRP, train$Item_Visibility1, train$Field_FactorTier.1, train$Field_FactorTier.2, train$Field_FactorDR, train$Field_FactorFD, train$Field_FactorGrocery.Store, train$Field_FactorSupermarket.Type1, train$Field_FactorSupermarket.Type2, train$Outlet_NumberofYears)

names(data_df)<-c("SaleT","Wgt", "Fat", "MRP", "Vis", "Tier1","Tier2","DR","FD","GroceryStore", "Supmt1", "Supmt2","Years")

#str(data_df)
M <- cor(data_df)
corrplot(M,method ="number")

#Intrepretation 
#The response variable "Sales" is correlated significantly with the MRP of items and the type of outlets,
#But,it has not shown any significant correlation with number of years of outlet establishment
#Moreover, this establishment year has showed a significant correlation Outlet location and outlet type.

##***************************************************************************##
#                                                                             #
#                             Regressor Selection                             #
#                                                                             #
##***************************************************************************##

#################### Divide into training and test data set ##############
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices = sample(1:nrow(treated_data_trans), 0.9*nrow(treated_data_trans))
# generate the train data set
train = treated_data_trans[trainindices,]

#Number of rows in train
length(train$Item_Identifier)
head(train)

#Similarly store the rest of the observations into an object "test".
test = treated_data_trans[-trainindices,]

#Number of rows in test
length(test$Item_Identifier)
head(test)

mdl = lm(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + Item_MRP + Item_Visibility1 + Field_FactorTier.1 + Field_FactorTier.2 + Field_FactorDR + Field_FactorFD + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Outlet_NumberofYears, data = train) 

summary(mdl)


############################### Variable Selection Procedures################################

library(MASS)

#setting null model having just intercept
null = lm(Transfmd_IO_Sales~1, data = train)                          

#setting full model having all terms

full = lm(Transfmd_IO_Sales ~ Item_Weight + Item_Fat_Content + Item_MRP + Item_Visibility1 + Field_FactorTier.1 + Field_FactorTier.2 + Field_FactorDR + Field_FactorFD + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Outlet_NumberofYears, data = train) 

summary(full)

#Forward selection procedure

step(null, scope = list(lower=null, upper=full), direction="forward")

#Backwrd selection procedure

step(full, scope = list(lower=null, upper=full), direction="backward")

#Stepwise selection procedure

step(null, scope = list(upper=full), data=heat, direction="both")

#Interpretation
#We got the same result from all three selection procedures - Item_MRP, Field_FactorGrocery.Store, Outlet_NumberofYears, Field_FactorSupermarket.Type1, Field_FactorSupermarket.Type2, Item_Fat_Content

############################### Procedures for subsetting variables ################################

############################### All possible subset models - (2^12) ################################
#Computing All models
library(leaps)

#Creating all possible subset models 
tmp = regsubsets(Transfmd_IO_Sales ~ Item_Weight + Item_Fat_Content + Item_MRP + Item_Visibility1 + Field_FactorTier.1 + Field_FactorTier.2 + Field_FactorDR + Field_FactorFD + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Outlet_NumberofYears, data = train, nbest=10, really.big=T, intercept=T)

almdl = summary(tmp)[[1]]
RSQ = summary(tmp)[[2]]
SSE = summary(tmp)[[3]]
adjR2 = summary(tmp)[[4]]
Cp = summary(tmp)[[5]]
BIC = summary(tmp)[[6]]

tabl_criteria = cbind(almdl,SSE,RSQ,adjR2,Cp,BIC)        #Column bind all the criteria
ordered_tabl_criteria = tabl_criteria[order(-adjR2),]    #Sorting based on AdjR2 in decreasing order

ordered_tabl_criteria

write.csv(ordered_tabl_criteria, file = "all_model12.csv")

#Interpretation

## We got 2 finallist model one with 6 regressors (Item_MRP, Field_FactorGrocery.Store, Outlet_NumberofYears, Field_Factor, Supermarket.Type1, Field_FactorSupermarket.Type2, Item_Fat_Content) 
#which was same as the selection procedure and other model with 5 regressors (Item_MRP, Field_FactorGrocery.Store, Field_FactorSupermarket.Type1, Field_FactorSupermarket.Type2, Item_Fat_Content). 
## "Outlet_NumberofYears" didn't show any strong correlation with response variable also in correlation matrix. Moreover, it was correlated with one of other regressor (Field_FactorGrocery.Store). On further running the MLR model(mdl1) below, we saw "Outlet_NumberofYears" was coming insignificant (p-value = 0.117)

##***************************************************************************##
#                                                                             #
#                             Regressor Model                                 #
#                                                                             #
##***************************************************************************##

########### Running model with the selected regressors

#Model 1: Sales Vs Selected Regressors from stepwise including "Outlet_NumberofYears" 
mdl1 = lm(Transfmd_IO_Sales ~  Item_MRP + Field_FactorGrocery.Store + Outlet_NumberofYears + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train)

summary(mdl1)
#Intrepretation:
#On running the MLR we saw "Outlet_NumberofYears" was coming insignificant (p-value = 0.117). So, mdl2 is run excluding "Outlet_NumberofYears".

#Model 2: Sales Vs Selected Regressors excluding "Outlet_NumberofYears"
mdl2 = lm(Transfmd_IO_Sales ~  Item_MRP + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train)

summary(mdl2)
#Intrepretation: 
#On running the MLR we saw all variables were coming significant (p-value < 0.05). 

#Model 3: Transformation of one of the regressors "Item_MRP" as it showed non-linear relationship with response variable in multiple pair plot.
############################## Pair Plot before running model ############################################
# Multiple pair plots to see relationship between response variable and regressors
pairs(~ Transfmd_IO_Sales + Item_MRP + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + as.numeric(Item_Fat_Content) , data = train, cex.labels=2, cex.axis=1,cex=1, main = 'Multiple Plots')

pairs(~ Transfmd_IO_Sales + Item_MRP, data = train, cex.labels=2, cex.axis=1,cex=1, main = 'Multiple Plots')

mdl3 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train)

summary(mdl3)
#Intrepretation:
#This model showed a slight improvement in the adj-R square value over last model with untransformed regressor

############################## Pair Plot after model run ############################################

## Multiple pair plots to see relationship between response variable and regressors
pairs(~ Transfmd_IO_Sales + sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + as.numeric(Item_Fat_Content) , data = train, cex.labels=2, cex.axis=1,cex=1, main = 'Multiple Plots')

pairs(~ Transfmd_IO_Sales + sqrt(Item_MRP), data = train, cex.labels=2, cex.axis=1,cex=1, main = 'Multiple Plots')


##***************************************************************************##
#                                                                             #
#                             Residual Analysis                               #
#                                                                             #
##***************************************************************************##

#install.packages("MASS")

library(MASS)
#Residual analysis of model 3
c = resid(mdl3)

#Normal probability Plot showed to be almost linear distribution  
plot(mdl3,  which = 2,col = "blue", main = "Normal probability Plot")
abline(0,1)

# Plotting R-Student against Y-HATS
t = rstudent(mdl3)
yhat = mdl3$fit
plot(yhat, t, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

#Residual Analysis of model
Y_Value = train$Transfmd_IO_Sales
Residual = residuals(mdl3) 			## RESIDUAL
Stand_Res =stdres(mdl3)			    ## STANDARDIZED RESIDUAL
Student_Res = studres(mdl3)   	## STUDENTIZED RESIDUAL 
R_Student = rstudent(mdl3) 	   	## COMPUTING R-Student
Lev_hii = hatvalues(mdl3)       ## LEVERAGE POINTS 
CookD = cooks.distance(mdl3)  	## COOKS DISTANCE 
Dffit = dffits(mdl3)        		## DFFITS
Dfbetas = dfbeta(mdl3)			  	## COMPUTING DFBETAS

#Combing all the actual response variable, residuals and influencers into one dataframe 
allres = cbind.data.frame(Y_Value,Lev_hii, R_Student, CookD, Dffit)

#Inspecting the outliers based on R-Student, CookD and DFFit at 5% level of significance
allres$I_RStu = ifelse(abs(allres$R_Student) > qt(0.975,7643), c("Inspect"), c("0"))
allres$I_CookD =  ifelse(allres$CookD > qf(0.95,6,7643), c("Inspect"), c("0"))
allres$I_Dffit =  ifelse(abs(allres$Dffit) > 2*sqrt(6/nrow(train)), c("Inspect"), c("0"))

#Including R-Student and DFFit into training data
train$R_Student = allres$R_Student
train$Dffit = allres$Dffit
head(train)

######################## Outlier based on Rstudent and Dffit are removed ####################

#Data having outliers on comparing Rstudent value with cut off value of t distribution and Dffit value with its cut-off value.
train_outlier_rev1  = train[(abs(train$R_Student) > qt(0.975,7643)) & (abs(train$Dffit)) > 2*sqrt(6/nrow(train)),]

library(plyr)
library(dplyr)

#Removing above outliers from train dataset
train_out_rm = anti_join(train, train_outlier_rev1, by = c("Item_Identifier","Outlet_Identifier"))

## Model 4: Built on model 3, but on data without outliers found in previous step 
mdl4 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train_out_rm)

summary(mdl4)
#Intrepretation:
#Adj-R square showed further improvement to 69%.

######## Check for VIF(Variance Inflation Factor) of Model ##########
library(car)
vif(mdl4)
#Intrepretation
# All the Variance coeff < 5 ==> there is low variance 

# Check if there exist multicollinearity problem
sqrt(vif(mdl4)) > 2

#Conclusion: There is no problem and variance inflation factor is under control for the normal fit

################################ Checking Normality and linearity of the model 4 ###################################

## Normal probability Plot showed to be almost linear distribution  
plot(mdl4,  which = 2,col="blue", main = "Normal probability Plot")
abline(0,1)

# Plotting R-Student against Y-HATS
t1 = rstudent(mdl4)
yhat1 = mdl4$fit
plot(yhat1, t1, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')


################# Second round of removing outliers ##################
################# Outliers based just on Rstudent are removed ####################

## Data having outliers on comparing Rstudent value with cut off value of t distribution.
train_out_rm2  = subset(train, (abs(train$R_Student) <= qt(0.975,7643)))

## Model 5: Tried a different model with outliers removed on the basis of just Rstuedent and not considering Dffit. 

mdl5 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train_out_rm2)

summary(mdl5)
#Intrepretation:
#Adj-R square showed further a significant improvement to 73.6%.

######## Check for VIF(Variance Inflation Factor) of Model ##########
library(car)
vif(mdl5)
#Intrepretation
# All the Variance coeff < 5 ==> there is low variance 

# Check if there exist multicollinearity problem
sqrt(vif(mdl5)) > 2
#Conclusion: There is no problem and variance inflation factor is under control for the normal fit

################################ Checking Normality and linearity of the model 5 ###################################

#Normal probability Plot showed to be almost linear distribution  
plot(mdl5,  which = 2,col="blue", main = "Normal probability Plot")
abline(0,1)

# PLOTTING R-Student against Y-HATS
t1 = rstudent(mdl5)
yhat1 = mdl5$fit
plot(yhat1, t1, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

################# Third round of removing outliers #####################
################# Outliers based on both Rstudent or Dffit are removed ####################

## Data having outliers on comparing Rstudent value with cut off value of t distribution or Dffit with its cut off value
train_outlier_rev2  = train[(abs(train$R_Student) > qt(0.975,7643)) | (abs(train$Dffit)) > 2*sqrt(6/nrow(train)),]

library(dplyr)
#Removing above outliers from train dataset
train_out_rm3 = anti_join(train, train_outlier_rev2, by = c("Item_Identifier","Outlet_Identifier"))

## Model 6: Tried again a different model with outliers removed on the basis of either Rstudent > cut-off value or Dffit value > cut-off value. 

mdl6 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train_out_rm3)

summary(mdl6)

#Intrepretation:
#Adj-R square showed further a significant improvement to 74.8%.

######## Check for VIF(Variance Inflation Factor) of Model ##########
library(car)
vif(mdl6)
#Intrepretation
# All the Variance coeff < 5 ==> there is low variance 

# Check if there exist multicollinearity problem
sqrt(vif(mdl6)) > 2

#Conclusion: There is no problem and variance inflation factor is under control for the normal fit


################################ Checking Normality and linearity of the model 6 ###################################

#Normal probability Plot showed to be almost linear distribution  
plot(mdl6,  which = 2,col="blue", main = "Normal probability Plot")
abline(0,1)

# PLOTTING R-Student against Y-HATS
t1 = rstudent(mdl6)
yhat1 = mdl6$fit
plot(yhat1, t1, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

########## Trying to improve R square of model 4 by further treating outliers in resulting mdl4 model ################

############# Residual analysis of model4 ###############

library(MASS)
#Residual analysis of model 4
c4 = resid(mdl4)
head(c4)

#Normal probability Plot showed to be almost linear distribution  
plot(mdl4,  which = 2,col="blue", main = "Normal probability Plot")
abline(0,1)

# Plotting R-Student against Y-HATS
t4 = rstudent(mdl4)
yhat4 = mdl4$fit
plot(yhat4, t4, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

Y_Value = train_out_rm$Transfmd_IO_Sales
Residual = residuals(mdl4) 			## RESIDUAL
Stand_Res =stdres(mdl4)			    ## STANDARDIZED RESIDUAL
Student_Res = studres(mdl4)   	## STUDENTIZED RESIDUAL 
R_Student = rstudent(mdl4) 	   	## COMPUTING R-Student
Lev_hii = hatvalues(mdl4)       ## LEVERAGE POINTS 
CookD = cooks.distance(mdl4)  	## COOKS DISTANCE 
Dffit = dffits(mdl4)        		## DFFITS
Dfbetas = dfbeta(mdl4)			  	## COMPUTING DFBETAS

#Combing all the actual response variable, residuals and influencers into one dataframe 
allres = cbind.data.frame(Y_Value,Lev_hii, R_Student, CookD, Dffit)

#Inspecting the outliers based on R-Student, CookD and DFFit at 5% level of significance
allres$I_RStu = ifelse(abs(allres$R_Student) > qt(0.975,7451), c("Inspect"), c("0"))
allres$I_CookD =  ifelse(allres$CookD > qf(0.95,6,7451), c("Inspect"), c("0"))
allres$I_Dffit =  ifelse(abs(allres$Dffit) > 2*sqrt(6/nrow(train)), c("Inspect"), c("0"))

######################## Outlier based on Rstudent and Dffit are removed ####################
#Including R-Student and DFFit into model 4 dataset
train_out_rm$R_Student = allres$R_Student
train_out_rm$Dffit = allres$Dffit

#Data having outliers on comparing Rstudent value with cut off value of t distribution and Dffit value with its cut-off value.
train_outlier_rev5  = train_out_rm[(abs(train_out_rm$R_Student) > qt(0.975,7451)) & (abs(train_out_rm$Dffit)) > 2*sqrt(6/nrow(train_out_rm)),]

library(dplyr)
train_out_rm5 = anti_join(train_out_rm, train_outlier_rev5, by = c("Item_Identifier","Outlet_Identifier"))

mdl7 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train_out_rm5)

summary(mdl7)
#Interpretation
##Adj-R square showed further a slight improvement from model4 to model7  69.9 to 71.1%.

######## Check for VIF(Variance Inflation Factor) of Model ##########
library(car)
vif(mdl7)
#Intrepretation
# All the Variance coeff < 5 ==> there is low variance 

# Check if there exist multicollinearity problem
sqrt(vif(mdl7)) > 2
#Conclusion: There is no problem and variance inflation factor is under control for the normal fit

############# Further removal of residual analysis after model7 ###############
library(MASS)
#Residual analysis of model 3
c7 = resid(mdl7)
head(c7)

#Normal probability Plot showed to be almost linear distribution  
plot(mdl7,  which = 2,col="blue", main = "Normal probability Plot")
abline(0,1)

# Plotting R-Student against Y-HATS
t7 = rstudent(mdl7)
yhat7 = mdl7$fit
plot(yhat7, t7, pch = 20, type = 'p', las = 1,
     xlab="Predicted", ylab="R-Student",  main = 'Predicted vs R-Student')

Y_Value = train_out_rm5$Transfmd_IO_Sales
Residual = residuals(mdl7) 			## RESIDUAL
Stand_Res =stdres(mdl7)			    ## STANDARDIZED RESIDUAL
Student_Res = studres(mdl7)   	## STUDENTIZED RESIDUAL 
R_Student = rstudent(mdl7) 	   	## COMPUTING R-Student
Lev_hii = hatvalues(mdl7)       ## LEVERAGE POINTS 
CookD = cooks.distance(mdl7)  	## COOKS DISTANCE 
Dffit = dffits(mdl7)        		## DFFITS
Dfbetas = dfbeta(mdl7)			  	## COMPUTING DFBETAS

#Combing all the actual response variable, residuals and influencers into one dataframe 
allres = cbind.data.frame(Y_Value,Lev_hii, R_Student, CookD, Dffit)

#Inspecting the outliers based on R-Student, CookD and DFFit at 5% level of significance
allres$I_RStu = ifelse(abs(allres$R_Student) > qt(0.975,7361), c("Inspect"), c("0"))
allres$I_CookD =  ifelse(allres$CookD > qf(0.95,6,7361), c("Inspect"), c("0"))
allres$I_Dffit =  ifelse(abs(allres$Dffit) > 2*sqrt(6/nrow(train)), c("Inspect"), c("0"))

######################## Outlier based on Rstudent and Dffit are removed ####################
#Including R-Student and DFFit into model 4 dataset
train_out_rm5$R_Student = allres$R_Student
train_out_rm5$Dffit = allres$Dffit

#Data having outliers on comparing Rstudent value with cut off value of t distribution and Dffit value with its cut-off value.
train_outlier_rev7  = train_out_rm5[(abs(train_out_rm5$R_Student) > qt(0.975,7361)) & (abs(train_out_rm5$Dffit)) > 2*sqrt(6/nrow(train_out_rm5)),]

library(dplyr)
train_out_rm8 = anti_join(train_out_rm5, train_outlier_rev7, by = c("Item_Identifier","Outlet_Identifier"))

mdl8 = lm(Transfmd_IO_Sales ~  sqrt(Item_MRP) + Field_FactorGrocery.Store + Field_FactorSupermarket.Type1 + Field_FactorSupermarket.Type2 + Item_Fat_Content, data = train_out_rm8)

summary(mdl8)
#Interpretation
##Adj-R square showed further a slight improvement from model7 to model8  71.1 to 71.5%.

######## Check for VIF(Variance Inflation Factor) of Model ##########
library(car)
vif(mdl7)
#Intrepretation
# All the Variance coeff < 5 ==> there is low variance 

# Check if there exist multicollinearity problem
sqrt(vif(mdl7)) > 2
#Conclusion: There is no problem and variance inflation factor is under control for the normal fit

##***************************************************************************##
#                                                                             #
#                             Model Accuracy                                  #
#                                                                             #
##***************************************************************************##

#As we saw that model 6 has the highest Adj-Rsquare and minimum RMSE with all significant regressors. Hence model 6 is the final model with Adj-R square of 74.8%.

###################  K-FOLD CROSS VALIDATION  ##############
library(DAAG)
par(mar=c(5.1,4.1,4.1,2.1))
cv.lm(data = train_out_rm3, mdl6, m = 4 ) # 4 fold cross-validation

##Testing our model with test data based on the model 6 from training data
Predict_1 <- predict(mdl6,test[, c(3,4,13,14,15)])
test$test_sales <- Predict_1

################################ Accuracy of the predictions - Metrics ####################################
## Make predictions and compute the R2, RMSE and MAE of test data #################

library(tidyverse)
library(caret)

predictions <- mdl6 %>% predict(test)
data.frame( R2   = R2(predictions, test$Transfmd_IO_Sales ),
            RMSE = RMSE(predictions, test$Transfmd_IO_Sales ),
            MAE  = MAE(predictions, test$Transfmd_IO_Sales ))

## With Rsquare from Train and Test being 0.748 and 0.66 respectively and also has the P-Values significant 
## for all the variables, We should keep this model6. 
## RMSE of test data came out to be nearly closer to RMSE of model based on the trained dataset

## The curves for variations
library(ggplot2)
ggplot(test, aes(x = c(1:nrow(test)))) + geom_line(aes(y = test_sales), col = 'red') + geom_line(aes(y = Transfmd_IO_Sales), col = 'black')

## Conclusiom:
## On comparing the sales prediction with actual sales of test data, there seems to be a good overlap. This should be a good model to propose.

#The ﬁnal model is the best working model because:
#a. Only 5 regressors were sufficient to predict the item outlet sales, these regressors were MRP, type of outlet:     grocery, supermarket 1 & supermarket 2 and  item fat content 
#b. It has all significant regressors at 5% level 
#c. It has the highest adj-R2 of 74.8%
#d. It has the lowest RMSE = 8.58
#e. When this model was validated with test data it gave the RMSE of 10.4 which was close enough with the training     model

####################################################### Thankyou!! ################################################






