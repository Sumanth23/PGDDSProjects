#############################HR Solution###################
################################################################
# Business Understanding
# Data Understanding
# Data Preparation & EDA
# Model Building 
# Model Evaluation
################################################################


################################################################
### Data Understanding
# Clearing workspace
rm(list = ls())


# Install and Loading the required packages
# install.packages("MASS")
# install.packages("car")
# install.packages("e1071")
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("cowplot")
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("rlang")
# install.packages("chron")

library(MASS)
library(car)
library(ggplot2)
library(e1071)
library(cowplot)
library(caTools)
library("chron")
library("tidyr")
library(lubridate)
library("dplyr")
library(caret)
library(ROCR)
library(corrplot)

## Loading All the Files. 
emp_survey<-read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
emp_data<-read.csv("general_data.csv",stringsAsFactors = FALSE)
emp_in_time<-read.csv("in_time.csv",stringsAsFactors = FALSE)
mngr_survey<-read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
emp_out_time<-read.csv("out_time.csv",stringsAsFactors = FALSE)

## Checking the structure of the data.
str(emp_survey) #4410
str(emp_data)  #4410
str(emp_in_time)#4410
str(mngr_survey)#4410
str(emp_out_time)#4410

## Renaming the column name to EmployeeID in the emp_in_time and emp_out_time
colnames(emp_in_time)[1]<-"EmployeeID"
colnames(emp_out_time)[1]<-"EmployeeID"

## Checking the unique employee ID
length(unique(tolower(emp_survey$EmployeeID))) #4410
length(unique(tolower(emp_data$EmployeeID))) #4410
length(unique(tolower(emp_in_time$EmployeeID))) #4410
length(unique(tolower(mngr_survey$EmployeeID))) #4410
length(unique(tolower(emp_out_time$EmployeeID))) #4410

## checking if there are any differnce in the employee ID across data files.
setdiff(emp_survey$EmployeeID,emp_data$EmployeeID) #0
setdiff(emp_data$EmployeeID,emp_in_time$EmployeeID) #0
setdiff(emp_data$EmployeeID,mngr_survey$EmployeeID) #0
setdiff(emp_data$EmployeeID,emp_out_time$EmployeeID) #0

 
## Finding the average time spent in office by an employee
## Changing the format of the emp_in_time fields  
head(emp_in_time)
in_time <- emp_in_time %>% gather("Date","Value",colnames(emp_in_time[c(2:262)]),na.rm = TRUE)
in_time$Value <- parse_date_time(in_time$Value,tz="Asia/Calcutta", orders=c("%d-%m-%Y %H:%M:%S","%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S"))

## Spliting the Value field in Data  and time. 
in_time <- data.frame(EmployeeID= in_time$EmployeeID,date=in_time$Value,time=format(in_time$Value,"%H:%M:%S"))

## Finding the Average time in employee in the office
#converting the time in seconds and then calculating mean.
in_time$Sec <- period_to_seconds(hms(in_time$time))
in_time_group <- in_time %>% group_by(EmployeeID)
in_time_summary <- in_time_group%>%summarise(emp_in_time=mean(Sec))

## Changing the format of the emp_out_time fields 
out_time <- emp_out_time %>% gather("Date","Value",colnames(emp_out_time[c(2:262)]),na.rm = TRUE)
out_time$Value <- parse_date_time(out_time$Value,tz="Asia/Calcutta", orders=c("%d-%m-%Y %H:%M:%S","%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S"))
head(out_time)

## Spliting the Value field in Data  and time. 
out_time <- data.frame(EmployeeID= out_time$EmployeeID,date=out_time$Value,time=format(out_time$Value,"%H:%M:%S"))

## Finding the AVerage time out employee in the office
#converting the time in seconds and then calculating mean.
out_time$Sec <- period_to_seconds(hms(out_time$time))
out_time_group <- out_time %>% group_by(EmployeeID)
out_time_summary <- out_time_group %>% summarise(emp_out_time=mean(Sec))


## Merging the in_time_summary and out_time_summary data sets
emp_work_time <- merge(in_time_summary,out_time_summary,by="EmployeeID")
head(emp_work_time)
str(emp_work_time)

## Finding the average hours spent in office.
emp_work_time$avg_hours<-abs(emp_work_time$emp_out_time-emp_work_time$emp_in_time)/(60*60)
head(emp_work_time)


## Checking the NA values in the dataframes
sum(is.na(emp_data$Age)) #0
sum(is.na(emp_data$BusinessTravel)) #0
sum(is.na(emp_data$Department)) #0
sum(is.na(emp_data$DistanceFromHome)) #0
sum(is.na(emp_data$Education)) #0
sum(is.na(emp_data$Gender)) #0
sum(is.na(emp_data$EmployeeCount)) #0
sum(is.na(emp_data$JobLevel)) #0
sum(is.na(emp_data$JobRole)) #0
sum(is.na(emp_data$MaritalStatus)) #0
sum(is.na(emp_data$MonthlyIncome)) #0
sum(is.na(emp_data$NumCompaniesWorked)) #19
sum(is.na(emp_data$Over18)) #0
sum(is.na(emp_data$PercentSalaryHike)) #0
sum(is.na(emp_data$StandardHours)) #0
sum(is.na(emp_data$StockOptionLevel)) #0
sum(is.na(emp_data$TotalWorkingYears)) #9
sum(is.na(emp_data$TrainingTimesLastYear)) #0
sum(is.na(emp_data$YearsAtCompany)) #0
sum(is.na(emp_data$YearsSinceLastPromotion)) #0
sum(is.na(emp_data$YearsWithCurrManager)) #0
sum(is.na(emp_survey)) #83
sum(is.na(emp_survey$EnvironmentSatisfaction)) #25
sum(is.na(emp_survey$JobSatisfaction)) #20
sum(is.na(emp_survey$WorkLifeBalance)) #38
sum(is.na(mngr_survey)) #0
#Removing the in_time and out_time from the data set as we need only average hours.
emp_work_time$emp_in_time <- NULL
emp_work_time$emp_out_time <- NULL

#--------------------------------------------------------------------#
#                 Merging All the Data Set                           #
#--------------------------------------------------------------------#
emp_merge<-merge(emp_data,emp_survey,by="EmployeeID")
emp_merge<-merge(emp_merge,mngr_survey,by="EmployeeID")
emp_merge<-merge(emp_merge,emp_work_time,by="EmployeeID")

head(emp_merge)
str(emp_merge)

## Checking what all date are holidays
# emp_in_time
all_na_in_time <- apply(emp_in_time,2, function(x)all(is.na(x)))
all_na_in_time_colnames <- names(all_na_in_time[all_na_in_time>0])
all_na_in_time_colnames

# emp_out_time
all_na_out_time <- apply(emp_out_time,2, function(x)all(is.na(x)))
all_na_out_time_colnames <- names(all_na_out_time[all_na_out_time>0])
all_na_out_time_colnames

setdiff(all_na_in_time_colnames, all_na_out_time_colnames)    #0
# All the 12 days are holiday as emp out and in time for all employees are NA
# "X2015.01.01" "X2015.01.14" "X2015.01.26" "X2015.03.05" "X2015.05.01" "X2015.07.17" "X2015.09.17" "X2015.10.02"
# "X2015.11.09" "X2015.11.10" "X2015.11.11" "X2015.12.25"

## Checking the percentage on NA values in the merged data set
# Finding NA values for the merged emp_merge dataset
emp_merge_na <- data.frame(sapply(emp_merge, function(x) round(sum(is.na(x))*100/nrow(emp_merge),2)))
names <- rownames(emp_merge_na)
emp_merge_na <- cbind(names,emp_merge_na)
colnames(emp_merge_na)[2] <- 'na_percentage'
emp_merge_na <- emp_merge_na[which(emp_merge_na$na_percentage > 0),]

# The percentage of NAs are very less for five columns. Hence we can remove those columns
#         names           na_percentage
# NumCompaniesWorked          0.43%
# TotalWorkingYears           0.20%
# EnvironmentSatisfaction     0.57%
# JobSatisfaction             0.45%
# WorkLifeBalance             0.86%
emp_merge <- na.omit(emp_merge)

## converting the columns to factor to be used for univariate and bivariate analysis
## Getting all the factor column names
factor_columns <- c("Attrition", "BusinessTravel", "Department", "EducationField", 
                    "Gender", "JobLevel", "JobRole", "MaritalStatus", "Over18", "JobSatisfaction",
                    "JobInvolvement", "PerformanceRating", "WorkLifeBalance", "EnvironmentSatisfaction",
                    "Education", "StockOptionLevel")
emp_merge[factor_columns] <- lapply(emp_merge[factor_columns], factor)


## Setting the theme for the plot 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="right")

#-----------------------------------------------------------------------------#
#   Data Visualisation for categorical variables - emp_merge                  #
#-----------------------------------------------------------------------------#
plot_grid(ggplot(emp_merge, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(emp_merge, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 


plot_grid(ggplot(emp_merge, aes(x=MaritalStatus,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(emp_merge, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_merge, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

# Visualising Gender Vs Business Travel for employees whose attrition is 'Yes'
ggplot(data = emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(x=Gender, fill = BusinessTravel)) + 
  geom_bar(position = position_dodge(width=1)) + 
  ggtitle('Gender Vs Business Travel For Employees Who Left The Company') +
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count', position = position_dodge(width = 1))
# Observation -
# 41% of Males and 25% of Females who Travel Rarely tend to leave the company than others.

# Visualising Department Vs Business Travel for employees whose attrition is 'Yes'
ggplot(data = emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(x=BusinessTravel, fill = Department)) + 
  geom_bar(position = position_dodge(width=1)) + 
  ggtitle('BusinessTravel Vs Department For Employees Who Left The Company') +
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count', position = position_dodge(width = 1))

# Observation -
#NEED TO UPDATE THIS ACCORDINGLY
# Employees who travels rarely and are in Research & Development (around 41%) are tend to leave the company.

# Visualising Department Vs Monthly Income for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(MonthlyIncome, fill = Department)) + 
  geom_histogram(binwidth = 10000) + ggtitle('Department Vs Monthly Income For Employees Who Left The Company') 

mean(emp_merge$MonthlyIncome[which(emp_merge$Department == 'Research & Development' & emp_merge$Attrition=='Yes')])
# Observation -
# Employees below the income range of Rs. 70,000 from Research & Development have higher attrition rate.

# Visualising BusinessTravel Vs Monthly Income for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(MonthlyIncome, fill = BusinessTravel)) + 
  geom_histogram(binwidth = 10000)+ggtitle('Business Travel Vs Monthly Income For Employees Who Left The Company') 
mean(emp_merge$MonthlyIncome[which(emp_merge$BusinessTravel == 'Travel_Rarely' & emp_merge$Attrition=='Yes')])
# Observation -
# Employees below the income range Rs. 70,000 and who Travel Rarely have higher attrition rate.

# Visualising Gender Vs MaritalStatus for employees whose attrition is 'Yes'
ggplot(data = emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(x=MaritalStatus, fill = Gender)) + 
  geom_bar(position = position_dodge(width=1)) + 
  ggtitle('Gender Vs Marital Status For Employees Who Left The Company') +
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count', position = position_dodge(width = 1))
# Observation -
# Attrition is higher among Male employees irrespective of the Marital Status. Additionally, 
#32% of Male Employee who tend to leave the company are Single.


# Visualising Job Satisfaction Vs Attrition for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(DistanceFromHome, fill = JobSatisfaction)) + 
  geom_histogram(binwidth = 4)
mean(emp_merge$DistanceFromHome[which(emp_merge$JobSatisfaction == '1' & emp_merge$Attrition=='Yes')])
mean(emp_merge$DistanceFromHome[which(emp_merge$JobSatisfaction == '2' & emp_merge$Attrition=='Yes')])
mean(emp_merge$DistanceFromHome[which(emp_merge$JobSatisfaction == '3' & emp_merge$Attrition=='Yes')])
mean(emp_merge$DistanceFromHome[which(emp_merge$JobSatisfaction == '4' & emp_merge$Attrition=='Yes')])
# Observation -
# There is no noticeble difference in Job Satisfaction and attrition

# Visualising JobInvolvement Vs PerformanceRating for employees whose attrition is 'Yes'
ggplot(data = emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(x=JobInvolvement, fill = PerformanceRating)) + 
  geom_bar(position = position_dodge(width=1)) + 
  ggtitle('Job Involvement Vs Performance Rating For Employees Who Left The Company') +
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count', position = position_dodge(width = 1))
# Observation -
# Employees with high job involvement and with performance rating as 3 (about 44.7%) tend to leave the company.


# Visualising DistanceFromHome Vs WorkLifeBalance for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(avg_hours, fill = WorkLifeBalance)) + 
  geom_histogram(binwidth = 1) + scale_fill_discrete(labels=c("Bad", "Good", "Better", "Best"))
# Observation -
# There are wide range of employees those have better work life balance and left the company

# Visualising TotalWorkingYears Vs JobSatisfaction for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(TotalWorkingYears, fill = JobSatisfaction)) + 
  geom_histogram(binwidth = 4) + scale_fill_discrete(labels=c("Low", "Medium", "High", "Very High")) +
  ggtitle('Total years of Exp Vs Job Satisfaction For Employees Who Left The Company')
# Observation -
# Employees less than 10 years of experience tend to leave more even though the job satisfaction is high.

# Visualising YearsWithCurrManager Vs JobSatisfaction for employees whose attrition is 'Yes'
ggplot(emp_merge[which(emp_merge$Attrition == 'Yes'),], aes(YearsWithCurrManager, fill = JobSatisfaction)) + 
  geom_histogram(binwidth = 5) + scale_fill_discrete(labels=c("Low", "Medium", "High", "Very High")) +
  ggtitle('Years with Current Manager Vs Job Satisfaction For Employees Who Left The Company')
# Observation -
# Employees who have been with their manager for about 2.5 years with high job satisfaction tend to leave more

#-----------------------------------------------------------------------------#
#   Data Visualisation for continuous variables - emp_merge                   #
#-----------------------------------------------------------------------------#

###NEED TO LOOK AT THE CODE

x_axis_label <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),text = element_text(size=10))
bar_chart_label <- geom_text(aes(y = (..count..),label = scales::percent((..count..)/sum(..count..))), stat="count", vjust = -0.5, position = position_dodge(width = 1), size = 3)
bar_chart_style <- geom_bar(position = position_dodge(width = 1))  
legend_color <- scale_fill_manual("Attrition", values = c("No" = "#90F9B8", "Yes" = "#F9B090"))

# 1. Age
ggplot(emp_merge, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 2. DistanceFromHome
ggplot(emp_merge, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 3. MonthlyIncome
ggplot(emp_merge, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 4. NumCompaniesWorked
ggplot(emp_merge, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 5. PercentSalaryHike
ggplot(emp_merge, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 6. TotalWorkingYears
ggplot(emp_merge, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 7. TrainingTimesLastYear
ggplot(emp_merge, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 8. YearsAtCompany
ggplot(emp_merge, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 9. YearsSinceLastPromotion
ggplot(emp_merge, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 10. YearsWithCurrManager
ggplot(emp_merge, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

# 11. avg_hours
ggplot(emp_merge, aes(x=Attrition,y=avg_hours, fill=Attrition))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none") + legend_color

#Observations - 
# Key factors affecting Employee Attrition -
# Age < 30
# Income < 70,000
# Number of Companies Worked > 4
# Total Working Years < 10
# Average Hours > 8.5
#-----------------------------------------------------------------------------#
#                     Remvoing Outliers - emp_merge                           #
#-----------------------------------------------------------------------------#
boxplot(emp_merge$Age)       # No Outliers
boxplot(emp_merge$DistanceFromHome)    # No Outliers

boxplot(emp_merge$MonthlyIncome)
quantile(emp_merge$MonthlyIncome, seq(0,1,0.01))
emp_merge$MonthlyIncome[which(emp_merge$MonthlyIncome > 152020.0)] <- 152020.0

boxplot(emp_merge$NumCompaniesWorked)
quantile(emp_merge$NumCompaniesWorked, seq(0,1,0.01))
emp_merge$NumCompaniesWorked[which(emp_merge$NumCompaniesWorked > 8)] <- 8

boxplot(emp_merge$PercentSalaryHike)   # No Outliers

boxplot(emp_merge$TotalWorkingYears)
quantile(emp_merge$TotalWorkingYears, seq(0,1,0.01))
emp_merge$TotalWorkingYears[which(emp_merge$TotalWorkingYears > 28)] <- 28

boxplot(emp_merge$TrainingTimesLastYear)
quantile(emp_merge$TrainingTimesLastYear, seq(0,1,0.01))
emp_merge$TrainingTimesLastYear[which(emp_merge$TrainingTimesLastYear > 4)] <- 4
emp_merge$TrainingTimesLastYear[which(emp_merge$TrainingTimesLastYear < 1)] <- 1

boxplot(emp_merge$YearsAtCompany)
quantile(emp_merge$YearsAtCompany, seq(0,1,0.01))
emp_merge$YearsAtCompany[which(emp_merge$YearsAtCompany > 19)] <- 19

boxplot(emp_merge$YearsSinceLastPromotion)
quantile(emp_merge$YearsSinceLastPromotion, seq(0,1,0.01))
emp_merge$YearsSinceLastPromotion[which(emp_merge$YearsSinceLastPromotion > 7)] <- 7

boxplot(emp_merge$YearsWithCurrManager)
quantile(emp_merge$YearsWithCurrManager, seq(0,1,0.01))
emp_merge$YearsWithCurrManager[which(emp_merge$YearsWithCurrManager > 14)] <- 14

boxplot(emp_merge$avg_hours)
quantile(emp_merge$avg_hours, seq(0,1,0.01))
emp_merge$avg_hours[which(emp_merge$avg_hours > 10.90)] <- 10.90

#-----------------------------------------------------------------------------#
#                     Correlation Matrix - emp_merge                          #
#-----------------------------------------------------------------------------#

numeric_columns <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked",
                     "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
                     "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
                     "avg_hours")
emp_merge_continuous <- emp_merge[, numeric_columns]
cor_emp_merge_continuous <- cor(emp_merge_continuous)
corrplot(cor_emp_merge_continuous, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.7, tl.col = 'black',
         order = "hclust", diag = FALSE)
# Observations - 
# 1. YearsSinceLastPromotion and YearsAtCompany are positively correlated
cor(emp_merge$YearsSinceLastPromotion, emp_merge$YearsAtCompany)  #0.551 
# 2. YearsSinceLastPromotion and YearsWithCurrManager are positively correlated
cor(emp_merge$YearsSinceLastPromotion, emp_merge$YearsWithCurrManager)  # 0.518
# 3. YearsAtCompany and YearsWithCurrManager are positively correlated
cor(emp_merge$YearsAtCompany, emp_merge$YearsWithCurrManager)  # 0.76
# 4. YearsAtCompany and TotalWorkingYears are positively correlated
cor(emp_merge$YearsAtCompany, emp_merge$TotalWorkingYears)  # 0.62
# 5. Age and TotalWorkingYears are positively correlated
cor(emp_merge$Age, emp_merge$TotalWorkingYears)  # 0.68

# Visual Representation of YearsAtCompany ~ Years With the current Manager
ggplot(emp_merge, aes(x = YearsAtCompany, y = YearsWithCurrManager, color = Attrition)) +
  geom_smooth(method = 'gam') + geom_point() + ggtitle("YearsAtCompany ~ Years With the current Manager")

# Visual Representation of YearsAtCompany ~ YearsSinceLastPromotion
ggplot(emp_merge, aes(x = YearsAtCompany, y = YearsSinceLastPromotion, color = Attrition)) +
  geom_smooth(method = 'gam') + geom_point() + ggtitle("YearsAtCompany ~ YearsSinceLastPromotion")

# Visual Representation of YearsSinceLastPromotion ~ Years With the current Manager
ggplot(emp_merge, aes(x = YearsSinceLastPromotion, y = YearsWithCurrManager, color = Attrition)) +
  geom_smooth(method = 'gam') + geom_point() + ggtitle("YearsSinceLastPromotion ~ Years With the current Manager")

# Visual Representation of YearsAtCompany ~ TotalWorkingYears
ggplot(emp_merge, aes(x = YearsAtCompany, y = TotalWorkingYears, color = Attrition)) +
  geom_smooth(method = 'gam') + geom_point() + ggtitle("YearsAtCompany ~ TotalWorkingYears")

# Visual Representation of Age ~ TotalWorkingYears
ggplot(emp_merge, aes(x = Age, y = TotalWorkingYears, color = Attrition)) +
  geom_smooth(method = 'gam') + geom_point() + ggtitle("Age ~ TotalWorkingYears")


# Finding the attrition rate
emp_merge$Attrition <- ifelse(emp_merge$Attrition=='Yes',1,0)
attrition_sum <- sum(emp_merge$Attrition)
str(emp_merge$Attrition)
total_sum <- nrow(emp_merge)
attrition_rate <- (attrition_sum*100)/total_sum
attrition_rate      # 16.16279% is the attrition rate for the XYZ company
#emp_merge$Attrition <- as.factor(emp_merge$Attrition)


#-----------------------------------------------------------------------------#
#                     Logistic Regression Model - Feature scaling             #
#-----------------------------------------------------------------------------#


emp_merge$Age <- scale(emp_merge$Age)
emp_merge$DistanceFromHome <- scale(emp_merge$DistanceFromHome)
emp_merge$MonthlyIncome <- scale(emp_merge$MonthlyIncome)
emp_merge$NumCompaniesWorked <- scale(emp_merge$NumCompaniesWorked)
emp_merge$PercentSalaryHike <- scale(emp_merge$PercentSalaryHike)
emp_merge$TotalWorkingYears <- scale(emp_merge$TotalWorkingYears)
emp_merge$TrainingTimesLastYear <- scale(emp_merge$TrainingTimesLastYear)
emp_merge$YearsAtCompany <- scale(emp_merge$YearsAtCompany)
emp_merge$YearsSinceLastPromotion <- scale(emp_merge$YearsSinceLastPromotion)
emp_merge$YearsWithCurrManager <- scale(emp_merge$YearsWithCurrManager)
emp_merge$avg_hours <- scale(emp_merge$avg_hours)


# EmployeeCount
unique(emp_merge$EmployeeCount)    
# The entire data set has only value of EmployeeCount - 1

# Over18
unique(emp_merge$Over18)
# The entire data set has only value of Over18 - Y

# StandardHours
unique(emp_merge$StandardHours)
# The entire data set has only value of StandardHours - 8

# Choosing to remove these three variables as they are not significant
emp_merge$EmployeeCount <- NULL
emp_merge$Over18 <- NULL
emp_merge$StandardHours <- NULL

#-----------------------------------------------------------------------------#
#                     Logistic Regression Model - Dummy variable Creation     #
#-----------------------------------------------------------------------------#

factor_columns_new <- c("BusinessTravel", "Department", "EducationField", 
                        "Gender", "JobLevel", "JobRole", "MaritalStatus", "JobSatisfaction",
                        "JobInvolvement", "PerformanceRating", "WorkLifeBalance", "EnvironmentSatisfaction",
                        "Education", "StockOptionLevel")
emp_merge_categorical <- emp_merge[, factor_columns_new]
emp_merge_non_categorical <- emp_merge[, -which(colnames(emp_merge) %in% factor_columns_new)]

dummies<- data.frame(sapply(emp_merge_categorical, 
                            function(x) data.frame(model.matrix(~x-1,data =emp_merge_categorical))[,-1]))

emp_merge_final <- cbind(emp_merge_non_categorical,dummies) 

#------------------------------------------------------------------------------------#
#   Logistic Regression Model - Setting seed and diving data into train and test     #
#------------------------------------------------------------------------------------#

set.seed(123)
split_flag <- sample.split(emp_merge_final, SplitRatio = 0.7)
employee_data_train <- emp_merge_final[split_flag, ]
employee_data_test <- emp_merge_final[!(split_flag), ]
str(employee_data_train)


#------------------------------------------------------------------------------------#
#   Logistic Regression Model - Model Building                                       #
#------------------------------------------------------------------------------------#


# Function to check last n vif values and p-values along with the variable name
find_vif_pvalue_topN <- function(model, n) {
  
  # To return the two objects
  result <- list()
  vif_model <- data.frame(vif(model))
  colnames(vif_model) <- c("vif")
  adj_r_squared <- summary(model)$adj.r.squared
  summary_p_value <- data.frame(summary(model)$coefficients[-1,4])
  colnames(summary_p_value) <- c("P-value")
  var_vif_pvalue <- cbind(vif_model, summary_p_value)
  var_vif_pvalue <- var_vif_pvalue[ order(-var_vif_pvalue[,1]), ]
  
  # Storing variable along with it's VIF and P-value in dataframe
  # Storing adjusted r squared in variable 
  # Returning the list
  result$varvifpvalue <- head(var_vif_pvalue, n)
  result$adjrsquared <- adj_r_squared
  return(result)
  
}

find_pvalue_topNDecreasing <- function(model, n) {
  # To return the two objects
  result <- list()
  vif_model <- data.frame(vif(model))
  colnames(vif_model) <- c("vif")
  adj_r_squared <- summary(model)$adj.r.squared
  summary_p_value <- data.frame(summary(model)$coefficients[-1,4])
  colnames(summary_p_value) <- c("P-value")
  var_vif_pvalue <- cbind(vif_model, summary_p_value)
  var_vif_pvalue <- var_vif_pvalue[ order(-var_vif_pvalue[,2]), ]
  
  # Storing variable along with it's VIF and P-value in dataframe
  # Storing adjusted r squared in variable 
  # Returning the list
  result$varpvalue <- head(var_vif_pvalue, n)
  result$adjrsquared <- adj_r_squared
  return(result)
}



# Initial model
model_1 = glm(Attrition ~ ., data = employee_data_train[,-1], family = "binomial")
summary(model_1) #AIC 2054.1

# Model 2
model_2<- stepAIC(model_1, direction="both")
summary(model_2) #AIC: 2025.9

## Model 3
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_hours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                WorkLifeBalance.x3 + WorkLifeBalance.x4 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                Education.x5 + StockOptionLevel.x1, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_3) #AIC: 2025.9

variablevifpvalue <- find_vif_pvalue_topN(model_3,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_3, 5)
variablepvalue

#                                 vif    P-value
# JobLevel.x5                   1.068997 0.13351581
## Model 4 - Removing JobLevel.x5
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_hours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                WorkLifeBalance.x3 + WorkLifeBalance.x4 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                Education.x5 + StockOptionLevel.x1, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_4) #AIC: AIC: 2026.3

variablevifpvalue <- find_vif_pvalue_topN(model_4,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_4, 5)
variablepvalue


#                                     vif    P-value
# Education.x5                  1.047306 0.12512982
## Model 5 - Removing Education.x5
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                avg_hours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobLevel.x2 + JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                WorkLifeBalance.x3 + WorkLifeBalance.x4 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               StockOptionLevel.x1, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_5) #AIC: 2026.8

variablevifpvalue <- find_vif_pvalue_topN(model_5,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_5, 5)
variablepvalue


#                                     vif    P-value
# JobLevel.x2                   1.046912 0.057859307
## Model 6 - Removing JobLevel.x2
model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobSatisfaction.x2 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + JobInvolvement.x3 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_6) #AIC: 2038.1

variablevifpvalue <- find_vif_pvalue_topN(model_6,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_6, 5)
variablepvalue

#                               vif    P-value
# MonthlyIncome            1.049457 0.66603683
## Model 7 - Removing MonthlyIncome
model_7 <- glm(formula = Attrition ~ Age+ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobSatisfaction.x2 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + JobInvolvement.x3 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_7) #AIC: 2036.2

variablevifpvalue <- find_vif_pvalue_topN(model_7,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_7, 5)
variablepvalue


#                               vif    P-value
# YearsAtCompany           4.479696 0.37385289
## Model 8 - Removing YearsAtCompany
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear +  
                YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobSatisfaction.x2 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + JobInvolvement.x3 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_8) #AIC: 2035

variablevifpvalue <- find_vif_pvalue_topN(model_8,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_8, 5)
variablepvalue

#                               vif    P-value
# JobRole.xSales.Executive 1.053446 0.32865469
## Model 9 - Removing JobRole.xSales.Executive
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear +  
                YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x5 + JobRole.xManufacturing.Director + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + JobSatisfaction.x2 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + JobInvolvement.x3 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
              data = employee_data_train[-1])
summary(model_9) #AIC: 2034

variablevifpvalue <- find_vif_pvalue_topN(model_9,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_9, 5)
variablepvalue

#                               vif    P-value
# MaritalStatus.xMarried 2.186260 0.283744685
## Model 10 - Removing MaritalStatus.xMarried
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear +  
                YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + JobLevel.x5 + 
                JobRole.xManufacturing.Director +  MaritalStatus.xSingle + JobSatisfaction.x2 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + JobInvolvement.x3 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
              data = employee_data_train[,-1])
summary(model_10) #AIC: 2033.1

variablevifpvalue <- find_vif_pvalue_topN(model_10,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_10, 5)
variablepvalue

#                               vif    P-value
# JobInvolvement.x3     1.034208 0.056342010
## Model 11 - Removing JobInvolvement.x3
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear +  
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + JobLevel.x5 + 
                 JobRole.xManufacturing.Director +  MaritalStatus.xSingle + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + Education.x5, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_11) #AIC: 2034.8

variablevifpvalue <- find_vif_pvalue_topN(model_11,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_11, 5)
variablepvalue

#                               vif    P-value
# Education.x5            1.045769 0.0549072217
## Model 12 - Removing Education.x5
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear +  
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobLevel.x5 + JobRole.xManufacturing.Director +  
                 MaritalStatus.xSingle + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_12) #AIC: 2036.8

variablevifpvalue <- find_vif_pvalue_topN(model_12,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_12, 5)
variablepvalue

#                               vif    P-value
# JobLevel.x5            1.035238 0.0405589930
## Model 13 - Removing JobLevel.x5
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear +  
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director +  MaritalStatus.xSingle + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_13) #AIC: 2039.4

variablevifpvalue <- find_vif_pvalue_topN(model_13,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_13, 5)
variablepvalue


#                               vif    P-value
# TrainingTimesLastYear    1.026574 0.0051539833
## Model 14 - Removing TrainingTimesLastYear
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_14) #AIC: 2045.3

variablevifpvalue <- find_vif_pvalue_topN(model_14,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_14, 5)
variablepvalue


#                               vif    P-value
# JobSatisfaction.x2        1.503238 0.0059936694
## Model 15 - Removing JobSatisfaction.x2
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle +  
                 JobSatisfaction.x3 + JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_15) #AIC: 2050.9

variablevifpvalue <- find_vif_pvalue_topN(model_15,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_15, 5)
variablepvalue

#                               vif    P-value
# JobSatisfaction.x3        1.189594 1.908609e-02
## Model 16 - Removing JobSatisfaction.x3
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 JobRole.xManufacturing.Director +  MaritalStatus.xSingle +  JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_16) #AIC: 2054.5

variablevifpvalue <- find_vif_pvalue_topN(model_16,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_16, 5)
variablepvalue


#                                     vif    P-value
# JobRole.xManufacturing.Director 1.025776 1.800061e-03
## Model 17 - Removing JobRole.xManufacturing.Director
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_hours + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 MaritalStatus.xSingle +  JobSatisfaction.x4 +  
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4, family = "binomial", 
               data = employee_data_train[,-1])
summary(model_17) #AIC: 2063.3

variablevifpvalue <- find_vif_pvalue_topN(model_17,5)
variablevifpvalue

variablepvalue <- find_pvalue_topNDecreasing(model_17, 5)
variablepvalue

## all the variables are highly significant.

#------------------------------------------------------------------------------------#
#   Logistic Regression Model - Model Evaluation  - Determining Cutoff               #
#------------------------------------------------------------------------------------#

emp_test_attrition_pred <- predict(model_17, type = "response", newdata = employee_data_test[,c(-1,-3)])
summary(emp_test_attrition_pred)
employee_data_test$Attrition_Pred <- emp_test_attrition_pred
View(employee_data_test)

# Determining the cut off value
emp_actual_attrition <- factor(ifelse(employee_data_test$Attrition==1,"Yes","No"))
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(emp_test_attrition_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, emp_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.0007608 to 0.8711999 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.87,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

## Generate the plot to show the intersection point of sensitivity, specificity and accuracy
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2,main = "Sensitivity and Specificity Trend")
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(x = 0.7, y = 0.83,col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),
       c("Sensitivity","Specificity","Accuracy"),lty=1:1, cex=0.45)

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #0.1663636

# Let's choose a cutoff value of 0.1663636 for final model

emp_cutoff_attrition <- factor(ifelse(emp_test_attrition_pred >=0.1663636, "Yes", "No"))

conf_final <- confusionMatrix(emp_cutoff_attrition, emp_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc#  Accuracy 0.7154412

sens #Sensitivity 0.7175926

spec #Specificity 0.715035 


#------------------------------------------------------------------------------------#
#   Logistic Regression Model - Model Evaluation  - KS Statistics                    #
#------------------------------------------------------------------------------------#

emp_cutoff_attrition <- ifelse(emp_cutoff_attrition=="Yes",1,0)
emp_actual_attrition <- ifelse(emp_actual_attrition=="Yes",1,0)
predicted_attrition <- prediction(emp_cutoff_attrition, emp_actual_attrition)
performance_measures_test <- performance(predicted_attrition, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.4326276

#------------------------------------------------------------------------------------#
#   Logistic Regression Model - Model Evaluation  - Lift and Gain Chart              #
#------------------------------------------------------------------------------------#

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(emp_actual_attrition, emp_cutoff_attrition, groups = 10)
Attrition_decile

plot(Attrition_decile$bucket, Attrition_decile$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket",main = "Lift")
plot(Attrition_decile$bucket, Attrition_decile$Gain, type="l", ylab="Gain", xlab="Bucket",main = "Gain")

# Interpretation of gain
# 73% of events covered in top 40% of data based on model.


# Interpretation of cumulative lift
# For top two decile, cummulative lift is 2. This means, we can cover 
# 2 times the number of attritors by selecting only 20% of the employees based on the model 
# as compared to 20% employee selection randomly.