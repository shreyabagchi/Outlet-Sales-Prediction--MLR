#SETTING WORK DIRECTORY

install.packages("caret")

setwd("C:\\MBS-Rutgers programs\\Regression Analysis\\Project\\Data Mart")



# READING DATA FROM AN OUTSIDE FILE - FILE TYPE EXCEL (.XLSX) IN WORK DIRECTORY

library(xlsx)

datamart=read.csv("Train_Sales.csv",1) # READING AND CREATING DATA 
datamart

traindata = read.csv("Train_Shreya.csv",1)
testdata = read.csv("Test_Shreya.csv",1)


# Data cleaning and pre-processing  : Converting categorical variable to dummy



#datamart <- datamart[!(datamart$Outlet_Size == ""), ]

datamart[datamart==""] <- NA

#Checkfor missing values using the saply() function
#sapply(datamart,function(x) sum(is.na(x)))
#length(datamart$Item_MRP)

#data.new <- subset(datamart,select=c())
#library(dummies)
#data.new$sex=dummy(data.new$sex)
#cor(dataindicator)

datamart$Item_Identifier <- NULL

datamart$Outlet_Identifier <- NULL

datamart$Item_Type <- NULL

datamart

datamart$Item_Fat_Content=ifelse(datamart$Item_Fat_Content=="Low Fat",0,1)
datamart

levels(datamart$Outlet_Location_Type)


library(caret)


#sapply(datamart,function(x) length(unique(x)))


dummies <- dummyVars( ~ ., data = datamart, fullRank = TRUE)
train_dummies<-as.data.frame(predict(dummies, newdata = datamart))
head(train_dummies)

#datamerge <- merge(datamart,train_dummies)
#datamerge
write.csv(train_dummies, file = "SalesData3.csv")


salesmart=read.csv("SalesData3.csv",1) # READING AND CREATING DATA 
salesmart

salesmart$Outlet_Size.High <- NULL
salesmart$Outlet_Size.Medium <- NULL
salesmart$Outlet_Size.Small <- NULL

salesmart

######### JUST LOOKING AT THE CORRRELATION 

sales = salesmart[,2:length(salesmart)]
sales
round(cor(sales),2)



#################  SELECTION PROCEDURES
install.packages("mixlm")

library(mixlm)

mod_sales=lm(Item_Outlet_Sales ~ Item_Weight+Item_Fat_Content+Item_Visibility+Item_MRP+Outlet_Establishment_Year+Outlet_Location_Type.Tier.2+Outlet_Location_Type.Tier.3+Outlet_Type.Supermarket.Type1+Outlet_Type.Supermarket.Type2+Outlet_Type.Supermarket.Type3, data=sales)
summary(mod_sales)
anova(mod_sales)


forward(mod_sales, alpha = 0.05, full = TRUE)
backward(mod_sales, alpha = 0.05, full = TRUE, hierarchy = TRUE)
stepWise(mod_sales, alpha.enter = 0.05, alpha.remove = 0.05, full = TRUE)

######################################################################## AFter removing variables####################################

mod_sales2=lm(Item_Outlet_Sales ~ Item_Fat_Content+Item_MRP+Outlet_Type.Supermarket.Type1+Outlet_Type.Supermarket.Type2+Outlet_Type.Supermarket.Type3, data=sales)
summary(mod_sales2)
anova(mod_sales2)

sales

data.new <- subset(sales,select=c('Item_Fat_Content','Item_MRP','Outlet_Type.Supermarket.Type1','Outlet_Type.Supermarket.Type2','Outlet_Type.Supermarket.Type3'))
data.new

round(cor(data.new),2)

coef=mod_sales2$coefficients # COEFFICIENTS OF THE MODEL

coef # PRINTING THE COEFFICIENTS 

# MULTIPLE PLOTS

pairs(~ Item_Outlet_Sales + Item_Fat_Content+Item_MRP+Outlet_Type.Supermarket.Type1+Outlet_Type.Supermarket.Type2+Outlet_Type.Supermarket.Type3, data=sales, cex.labels=2, cex.axis=1,cex=1)

# COMPUTING PREDICTED VALUES AND PLOTING AGAINST OBSERVED 

obsy=sales$Item_Outlet_Sales  # OBSERVED VALUES
obsy

yhat_y=cbind(predict(mod_sales2), sales$Item_Outlet_Sales) # COMBINING PREDICTED AND OBSERVED 

head(yhat_y) # PRINTING PART OF IT

plot(sales$Item_Outlet_Sales, predict(mod_sales2), pch = 20, type = 'p', las = 1,
     xlab="Observed", ylab="Predicted", main = 'Observed vs Predicted')

abline(0,1)

resid(mod_sales2)

cat("Residuals vs Normal Probability")
plot(mod_sales2,which =2,col="blue", main="Normal probability Plot")

cat("The model does not follow normal distribution. It looks like a positive skewed distribution")

########################### Plotting residual vs Fitted values
library(MASS)
stud_res=studres(mod_sales2)


plot(mod_sales2, which=1)

qqnorm(stud_res, ylab="Studentized Residuals", xlab="Normal Scores", 
       main="Q-Q Plot with Studentized Residual ") 

qqline(stud_res)

########################### Influence Analysis by cook's distance

plot(mod_sales2, which=4)

###########################Residual analysis

cook_dist = cooks.distance(mod_sales2)
cook_dist
dffits_model=dffits(mod_sales2)
dfbetas_model = dfbetas(mod_sales2)
hii = hatvalues(mod_sales2)
stdres_model = stdres(mod_sales2)
studres_model = studres(mod_sales2)
rstud = rstudent(mod_sales2)

allresid = cbind(dt_model$y,stdres_model,studres_model,rstud,cook_dist,dffits_model,hii)
allresid

#t-alfa/2,n-p-1 = t0.05/2,40-2-1 = 

#comparing the R-stud all the values are less than t-alfa except point 28.SO that may be considered as an outlier.


################################# Data Transformation



plot(data.new$Item_MRP,data.new$Item_Outlet_Sales, ylab="Item_Outlet_Sales", xlab="Item_MRP", main="Item_Outlet_Sales vs Item_MRP")
mdl = lm(Item_Outlet_Sales ~  Item_Fat_Content  + sqrt(Item_MRP) + Outlet_Type.Supermarket.Type1 + Outlet_Type.Supermarket.Type2 + Outlet_Type.Supermarket.Type3, data=sales)

xbar=data.new$Item_MRP

plot(sqrt(xbar),data.new$Item_Outlet_Sales, ylab="Y", xlab="x1 values", main="Y vs X")

summary(mdl)

#Transformation
plot(dt)
#We can see from the plots that relationship between x2 and y is not linear
#but resembles a sqrt relation.
mdl3=lm(y~x1+sqrt(x2),dt)
summary(mdl3)

x2=dt$x2
plot(sqrt(x2),dt$y, ylab="Y", xlab="x1 values", main="Y vs X")





