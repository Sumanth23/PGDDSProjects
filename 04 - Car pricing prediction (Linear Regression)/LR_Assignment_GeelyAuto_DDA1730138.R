# Loading of additional libraries
library(car)
library(stringr)
library(MASS)

# Clear environment of all variables
rm(list = ls())

# Load the provided cars dataset
#setwd("D:/Sumanth/Personal/12 - UpGrad/Section 3 & 4/02 - Linear Regression - Assignment")
car_pricing_df <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE, header = TRUE)

# View the dataset
View(car_pricing_df)

# Check the structure of the dataset and identify any initial patterns
str(car_pricing_df)

# As per the requirement the CarName comprises of the "Company name" & 
# "car model"; 
car_name <- data.frame(str_split_fixed(car_pricing_df$CarName, ' ', 2))
car_pricing_df <- cbind(car_pricing_df, car_name)

rm(car_name)

# Rename column names from default value
colnames(car_pricing_df)[which(colnames(car_pricing_df) == "X1")] <- "Company_name"
colnames(car_pricing_df)[which(colnames(car_pricing_df) == "X2")] <- "Car_model"
# Converting columns from factor to character
car_pricing_df$Company_name <- as.character(car_pricing_df$Company_name)
car_pricing_df$Car_model <- as.character(car_pricing_df$Car_model)

car_pricing_df <- car_pricing_df[,-28] # remove car model from data as it is not required
car_pricing_df <- car_pricing_df[,-3]  # remove original car name from data as it is not reqd.
car_pricing_df <- car_pricing_df[,-1]  # remove car ID from data as it is not required

car_pricing_df$Company_name[which(car_pricing_df$Company_name == "vw")] <- "volkswagen"
car_pricing_df$Company_name[which(car_pricing_df$Company_name == "vokswagen")] <- "volkswagen"
car_pricing_df$Company_name[which(car_pricing_df$Company_name == "toyouta")] <- "toyota"
car_pricing_df$Company_name[which(car_pricing_df$Company_name == "Nissan")] <- "nissan"
car_pricing_df$Company_name[which(car_pricing_df$Company_name == "maxda")] <- "mazda"

# Validating if there are any outliers in the dataset

# wheelbase
boxplot(car_pricing_df$wheelbase) 
# plotting the box plot we see there are 2 outliers - capping the 2 outliers
quantile(car_pricing_df$wheelbase, seq(0,1,0.01))
car_pricing_df$wheelbase[which(car_pricing_df$wheelbase > 114.2)] <- 114.2

# carlength 
boxplot(car_pricing_df$carlength) 
# (Q1 - 1.5IQR) and (Q3 + 1.5IQR)
# On validating the data it all lies between the outlier range defined as per above formula

# carwidth
boxplot(car_pricing_df$carwidth) 
quantile(car_pricing_df$carwidth, seq(0,1,0.01))
# (Q1 - 1.5IQR) and (Q3 + 1.5IQR)
# On validating the data 4 values lie above the upper limit (71.175)
car_pricing_df$carwidth[which(car_pricing_df$carwidth > 70.852)] <- 70.852

# carheight 
boxplot(car_pricing_df$carheight) # No outliers present
quantile(car_pricing_df$carheight, seq(0,1,0.01)) 
         # All data within the range 
         # (Q1 - 1.5IQR) and (Q3 + 1.5IQR)

# curbweight 
boxplot(car_pricing_df$curbweight)
quantile(car_pricing_df$curbweight, seq(0,1,0.01)) 
        # All data within the range 
        # (Q1 - 1.5IQR) and (Q3 + 1.5IQR)

# enginesize - validating the data and bigger cars will have bigger engine size; leave as is
boxplot(car_pricing_df$enginesize)
quantile(car_pricing_df$enginesize, seq(0,1,0.01))

# boreratio 
boxplot(car_pricing_df$boreratio)
quantile(car_pricing_df$boreratio, seq(0,1,0.01)) 
        # All data within the range 
        # (Q1 - 1.5IQR) and (Q3 + 1.5IQR) 

# stroke
boxplot(car_pricing_df$stroke)
quantile(car_pricing_df$stroke, seq(0,1,0.01))
# Updating outliers with the highest and lowest permissable value - (Q1 - 1.5IQR) and (Q3 + 1.5IQR) 
car_pricing_df$stroke[which(car_pricing_df$stroke > 3.8600)] <- 3.8600
car_pricing_df$stroke[which(car_pricing_df$stroke < 2.7056)] <- 2.7056

# compressionratio
boxplot(car_pricing_df$compressionratio)
quantile(car_pricing_df$compressionratio, seq(0,1,0.01))
# Updating outliers with the highest and lowest permissable value - (Q1 - 1.5IQR) and (Q3 + 1.5IQR) 
car_pricing_df$compressionratio[which(car_pricing_df$compressionratio > 10.0000)] <- 10.0000
car_pricing_df$compressionratio[which(car_pricing_df$compressionratio < 7.5000)] <- 7.5000

# horsepower 
boxplot(car_pricing_df$horsepower)
quantile(car_pricing_df$horsepower, seq(0,1,0.01))
# horsepower - validating the data and cars have higher horsepower by design; leave as is

# peakrpm 
boxplot(car_pricing_df$peakrpm)
quantile(car_pricing_df$peakrpm, seq(0,1,0.01))
# Updating outliers with the highest and lowest permissable value - (Q1 - 1.5IQR) and (Q3 + 1.5IQR) 
car_pricing_df$peakrpm[which(car_pricing_df$peakrpm > 6000)] <- 6000

# citympg
boxplot(car_pricing_df$citympg)
quantile(car_pricing_df$citympg, seq(0,1,0.01))
# citympg - validating the data and these cars are built for better milage; leave as is

# highwaympg 
boxplot(car_pricing_df$highwaympg)
quantile(car_pricing_df$highwaympg, seq(0,1,0.01))
# citympg - validating the data and these cars are built for better milage; leave as is

##################################################################################################
# Need to review and create dummy variables for all character variables

# Creating dummy variable for "fuel type" - there are only 2 values "gas" & "diesel"
car_pricing_df$fueltype <- as.factor(car_pricing_df$fueltype)
levels(car_pricing_df$fueltype) <- c(0,1) # assigning 0 for "diesel" and 1 for "gas"
car_pricing_df$fueltype <- as.numeric(car_pricing_df$fueltype)


# Creating dummy variable for "aspiration" - there are only 2 values "std" & "turbo"
car_pricing_df$aspiration <- as.factor(car_pricing_df$aspiration)
levels(car_pricing_df$aspiration)
levels(car_pricing_df$aspiration) <- c(1,0) # assigning 0 for "turbo" and 1 for "std"
car_pricing_df$aspiration <- as.numeric(car_pricing_df$aspiration)

# Creating dummy variable for "doornumber" - there are only 2 values "four" & "two"
car_pricing_df$doornumber <- as.factor(car_pricing_df$doornumber)
levels(car_pricing_df$doornumber)
levels(car_pricing_df$doornumber) <- c(1,0) # assigning 1 for "four" and 0 for "two"
car_pricing_df$doornumber <- as.numeric(car_pricing_df$doornumber)

# Creating dummy variable for "drivewheel" - there are 3 values "fwd", "rwd" & "4wd"
# Updating "rwd" and "fwd" to "1wd" - rear and front wheel drive to single wheel drive "2wd"
# we shall then update it using levels for "2wd" and "4wd"
car_pricing_df$drivewheel[which(car_pricing_df$drivewheel=="rwd")] <- "fwd"
car_pricing_df$drivewheel[which(car_pricing_df$drivewheel=="fwd")] <- "2wd"
car_pricing_df$drivewheel <- as.factor(car_pricing_df$drivewheel)
levels(car_pricing_df$drivewheel)
levels(car_pricing_df$drivewheel) <- c(1,0) # assigning 1 for "2wd" and 0 for "4wd"
car_pricing_df$drivewheel <- as.numeric(car_pricing_df$drivewheel)


# Creating dummy variable for "enginelocation" - there are 2 values "front" & "rear"
car_pricing_df$enginelocation <- as.factor(car_pricing_df$enginelocation)
levels(car_pricing_df$enginelocation)
levels(car_pricing_df$enginelocation) <- c(1,0) # assigning 1 for "front" and 0 for "rear"
car_pricing_df$enginelocation <- as.numeric(car_pricing_df$enginelocation)

# Creating dummy variables for "carbody" having 5 values - "convertible" "hatchback", "sedan", 
# "wagon", "hardtop"
dummy_1 <- data.frame(model.matrix(~carbody, car_pricing_df))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_1 <- cbind(car_pricing_df[,-5], dummy_1)

# Creating dummy variables for "enginetype" having 5 values - "dohc", "ohcv", "ohc", "l", "rotor",
# "ohcf", "dohcv"
rm(dummy_1)
dummy_1 <- data.frame(model.matrix(~enginetype, car_pricing_df))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_2 <- cbind(car_pricing_df_1[,-12], dummy_1)

# Creating dummy variables for "cylindernumber" having 7 values - "four", "six", 
# "five", "three", "twelve", "two", "eight"
rm(dummy_1)
dummy_1 <- data.frame(model.matrix(~cylindernumber, car_pricing_df))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_3 <- cbind(car_pricing_df_2[,-12], dummy_1)

# Creating dummy variables for "fuelsystem" having 7 values - "mpfi", "2bbl", "mfi", 
# "1bbl", "spfi", "4bbl", "idi", "spdi"
rm(dummy_1)
dummy_1 <- data.frame(model.matrix(~fuelsystem, car_pricing_df))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_4 <- cbind(car_pricing_df_3[,-13], dummy_1)

# Creating dummy variables for "symboling" having 6 values - 
rm(dummy_1)
car_pricing_df_4$symboling <- as.character(car_pricing_df_4$symboling)
dummy_1 <- data.frame(model.matrix(~symboling, car_pricing_df_4))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_4 <- cbind(car_pricing_df_4[,-1], dummy_1)

# Creating dummy variables for "Company name" having 23 values
rm(dummy_1)
dummy_1 <- data.frame(model.matrix(~Company_name, car_pricing_df_4))
dummy_1 <- dummy_1[,-1]
# Append the new columns to the dataset after removal carbody column 
car_pricing_df_4 <- cbind(car_pricing_df_4[,-20], dummy_1)

# clean up of unrequired variables
rm(dummy_1)

##################################################################################################

# Setting seed to get same repeated samples
set.seed(300)

# Creating the training data - deriving the training dataset indices
car_pricing_training_ind <- sample(1:nrow(car_pricing_df_4), 0.7*nrow(car_pricing_df_4))

# Creating the training data using the derived indices from previous step
car_pricing_training <- car_pricing_df_4[car_pricing_training_ind,]

# Creating the testing data using the derived indices from previous step
car_pricing_testing <- car_pricing_df_4[-car_pricing_training_ind,]

model_car_price1 <- lm(price~., data = car_pricing_training)
summary(model_car_price1)

step_AIC_car_pricing <- stepAIC(model_car_price1, direction = "both")
step_AIC_car_pricing

#Step AIC (multiple columns)
model_car_price2 <- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
                         wheelbase + carlength + carwidth + carheight + curbweight + 
                         enginesize + boreratio + compressionratio + peakrpm + highwaympg + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price2)
vif(model_car_price2)

# boreratio
model_car_price3 <- lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
                         wheelbase + carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + highwaympg + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price3)
vif(model_car_price3)
 
# aspiration
model_car_price4 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         wheelbase + carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + highwaympg + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price4)
vif(model_car_price4)

#carbodywagon
model_car_price5 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         wheelbase + carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + highwaympg + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price5)
vif(model_car_price5)

#highwaympg
model_car_price6 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         wheelbase + carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price6)
vif(model_car_price6)

#wheelbase
model_car_price7 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + Company_nameisuzu + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price7)
vif(model_car_price7)

#Company_nameisuzu
model_car_price8 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + 
                         carbodyhardtop + carbodyhatchback + carbodysedan + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price8)
vif(model_car_price8)

# carbodysedan
model_car_price9 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                         carlength + carwidth + carheight + curbweight + 
                         enginesize + compressionratio + peakrpm + 
                         carbodyhardtop + carbodyhatchback + 
                         enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                         cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                         symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                         Company_namechevrolet + Company_namedodge + 
                         Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                         Company_nameplymouth + Company_namerenault + Company_namesaab + 
                         Company_nametoyota, data = car_pricing_training)

summary(model_car_price9)
vif(model_car_price9)

# carbodyhardtop
model_car_price10 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + peakrpm + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namechevrolet + Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price10)
vif(model_car_price10)

# peakrpm
model_car_price11 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namechevrolet + Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price11)
vif(model_car_price11)

# fuelsystemmpfi
model_car_price12 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + fuelsystem2bbl+ symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namechevrolet + Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price12)
vif(model_car_price12)

# Company_namechevrolet
model_car_price13 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + fuelsystem2bbl+ symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price13)
vif(model_car_price13)


# fuelsystem2bbl
model_car_price14 <- lm(formula = price ~ fueltype + doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price14)
vif(model_car_price14)
# 
# fueltype
model_car_price15 <- lm(formula = price ~ doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemazda + Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price15)
vif(model_car_price15)

# Company_namemazda
model_car_price16 <- lm(formula = price ~ doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + compressionratio + 
                          carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price16)
vif(model_car_price16)

# compressionratio
model_car_price17 <- lm(formula = price ~ doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + Company_namenissan + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price17)
vif(model_car_price17)

# Company_namenissan
model_car_price18 <- lm(formula = price ~ doornumber + enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price18)
vif(model_car_price18)
# 
# doornumber
model_car_price19 <- lm(formula = price ~ enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          symboling3 + Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price19)
vif(model_car_price19)

# symboling3
model_car_price20 <- lm(formula = price ~ enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + enginetyperotor + 
                          cylindernumberfive + symboling2 + 
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price20)
vif(model_car_price20)

# enginetyperotor
model_car_price21 <- lm(formula = price ~ enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize + carbodyhatchback + 
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive + symboling2 + 
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price21)
vif(model_car_price21)

# carbodyhatchback
model_car_price22 <- lm(formula = price ~ enginelocation + 
                          carlength + carwidth + carheight + curbweight + 
                          enginesize +  
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive + symboling2 + 
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price22)
vif(model_car_price22)

# carlength
model_car_price23 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginesize +  
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive + symboling2 + 
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price23)
vif(model_car_price23)

# symboling2
model_car_price24 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginesize +  
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + Company_namesaab + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price24)
vif(model_car_price24)

# Company_namesaab
model_car_price25 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginesize +  
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price25)
vif(model_car_price25)

# enginesize
model_car_price26 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + enginetypeohcf + enginetypeohcv + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price26)
vif(model_car_price26)

# enginetypeohcv
model_car_price27 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + enginetypeohcf + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price27)
vif(model_car_price27)

# enginetypeohcf
model_car_price28 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namedodge + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price28)
vif(model_car_price28)

# Company_namedodge
model_car_price29 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namemitsubishi + 
                          Company_nameplymouth + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price29)
vif(model_car_price29)

# Company_nameplymouth
model_car_price30 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namemitsubishi + Company_namerenault + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price30)
vif(model_car_price30)

# Company_namerenault
model_car_price31 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namemitsubishi + 
                          Company_nametoyota, data = car_pricing_training)

summary(model_car_price31)
vif(model_car_price31)

# Company_nametoyota
model_car_price32 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          enginetypel + cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namemitsubishi, data = car_pricing_training)

summary(model_car_price32)
vif(model_car_price32)

# enginetypel
model_car_price33 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick + 
                          Company_namemitsubishi, data = car_pricing_training)

summary(model_car_price33)
vif(model_car_price33)

# Company_namemitsubishi
model_car_price34 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + curbweight + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick
                          , data = car_pricing_training)

summary(model_car_price34)
vif(model_car_price34)


cor(car_pricing_df_4$carwidth, car_pricing_df_4$curbweight) #0.8742732
 
# curbweight - because there is HIGH correlation between "carwidth" & "curbweight"
model_car_price35 <- lm(formula = price ~ enginelocation + 
                          carwidth + carheight + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick
                        , data = car_pricing_training)

summary(model_car_price35)
vif(model_car_price35)

# carheight
model_car_price36 <- lm(formula = price ~ enginelocation + 
                          carwidth + 
                          cylindernumberfive +  
                          Company_nameaudi + Company_namebmw + Company_namebuick
                        , data = car_pricing_training)

summary(model_car_price36)
vif(model_car_price36)

# Company_nameaudi
model_car_price37 <- lm(formula = price ~ enginelocation + 
                          carwidth + 
                          cylindernumberfive + Company_namebmw + Company_namebuick
                        , data = car_pricing_training)

summary(model_car_price37) # Adjusted R^2 - 0.8497
vif(model_car_price37)

# Below are the iterations and the corresponding R^2 values

##############################################################################################

# Iteration Measure       Value   Variable/step
# No.			
# =============================================
# 1	  Adjusted R-squared	0.9678	initial model
# 2	  Adjusted R-squared	0.9577	Post AIC
# 3	  Adjusted R-squared	0.9572	Boreratio
# 4	  Adjusted R-squared	0.9570	Aspiration
# 5	  Adjusted R-squared	0.9564	Carbodywagon
# 6	  Adjusted R-squared	0.9562	Highwaympg
# 7	  Adjusted R-squared	0.9541	Wheelbase
# 8	  Adjusted R-squared	0.9538	Company_nameisuzu
# 9	  Adjusted R-squared	0.9522	Carbodysedan
# 10	Adjusted R-squared	0.9519	Carbodyhardtop
# 11	Adjusted R-squared	0.9499	Peakrpm
# 12	Adjusted R-squared	0.9478	Fuelsystemmpfi
# 13	Adjusted R-squared	0.9471	Company_namechevrolet
# 14	Adjusted R-squared	0.9444	Fuelsystem2bbl
# 15	Adjusted R-squared	0.9444	Fueltype
# 16	Adjusted R-squared	0.9439	Company_namemazda
# 17	Adjusted R-squared	0.9432	Compressionratio
# 18	Adjusted R-squared	0.9425	Company_namenissan
# 19	Adjusted R-squared	0.9406	Doornumber
# 20	Adjusted R-squared	0.9391	Symboling3
# 21	Adjusted R-squared	0.9389	Enginetyperotor
# 22	Adjusted R-squared	0.9376	Carbodyhatchback
# 23	Adjusted R-squared	0.9355	Carlength
# 24	Adjusted R-squared	0.9348	Symboling2
# 25	Adjusted R-squared	0.9341	Company_namesaab
# 26	Adjusted R-squared	0.9311	Enginesize
# 27	Adjusted R-squared	0.9302	Enginetypeohcv
# 28	Adjusted R-squared	0.9275	Enginetypeohcf
# 29	Adjusted R-squared	0.9267	Company_namedodge
# 30	Adjusted R-squared	0.9260	Company_nameplymouth
# 31	Adjusted R-squared	0.9231	Company_namerenault
# 32	Adjusted R-squared	0.9194	Company_nametoyota
# 33	Adjusted R-squared	0.9136	Enginetypel
# 34	Adjusted R-squared	0.9090	Company_namemitsubishi
# 35	Adjusted R-squared	0.8545	Curb weight (since it has high correlation with car width)
# 36	Adjusted R-squared	0.8528	Carheight
# 37	Adjusted R-squared	0.8497	Company_nameaudi

##############################################################################################

# All the remaining variables in the model have a good p value and hence have been retained:
# enginelocation, carwidth, cylindernumberfive, Company_namebmw, Company_namebuick

predict1 <- predict(model_car_price37, car_pricing_testing)

car_pricing_testing$price_test <- predict1

r <- cor(car_pricing_testing$price, car_pricing_testing$price_test) # 0.8572049

r_squared <- r^2 # 0.7348002

# Difference of 11.489% between model and on evaluating with testing model R^2
