#removing any global variables
rm(list = ls())

# install.packages("gmodels")

#importing required libraries
library(plyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(gmodels)
library(corrplot)

#importing the dataset
loan <- read.csv("loan.csv",stringsAsFactors = F,na.strings = c(""," ","-","NA")) #while importing the dataset we can replace certain strings as NAs
#understanding the dataframe structure
str(loan)

#--------------------------------------------------------------------------------#
#                        DATA CLEANING                                           #
#--------------------------------------------------------------------------------#


##Checking how many duplicate records are there in ID and member_ID column
sum(duplicated(loan$id)) # 0, no duplicates, ID is unique.
sum(duplicated(loan$member_id)) # 0 , no duplicates, member_id is unique.

#Converting the loan term to a factor of 36 & 60 months.
loan$term <- as.factor(as.integer(str_replace_all(loan$term," months","")))
levels(loan$term) # shows the levels that are available after factorizing the column

#Converting int_rate to numeric and removing the percentage symbol.
loan$int_rate <- as.numeric(str_replace_all(loan$int_rate,"%",""))

#converting the grade and sub grade columns to factor.
loan$grade <- as.factor(loan$grade)
loan$sub_grade <- as.factor(loan$sub_grade)

# shows the levels that are available after factorizing the column
levels(loan$grade)
levels(loan$sub_grade)

#Converting employee length to a numeric and factor
distinct(loan,emp_length) # it shows the distinc values used in the data set. need to convert the >10+ years
#,< 1year and n/a to right values. Also remove years from the data set so that it can be
# converted into a factor. Since the data is in years users with < 1 year will be treated
# as 0 years experience and 10+ years will be treated as 10
loan$emp_length[which(loan$emp_length == "< 1 year")] <- 0
loan$emp_length[which(loan$emp_length == "10+ years")] <- 10
loan$emp_length[which(loan$emp_length == "n/a")] <- "Not Available" #rather than replacing it with NA we can change it to Not Available
loan$emp_length<- as.factor(str_replace_all(loan$emp_length,pattern = " year.*",replacement = ""))
levels(loan$emp_length)# shows the levels that are available after factorizing the column

#Checking the home_ownership,verification_status,loan_status
#column to identify any case or data issues and converting to factor.
distinct(loan,home_ownership)# no case or data issues, converting to factor.
distinct(loan,verification_status)# no case or data issues, converting to factor.
distinct(loan,loan_status)# combining Verified and source verified, since both are verified income status 

#we will combine it from an analysis perspective, then converting it to factor.
loan$home_ownership<-as.factor(loan$home_ownership)
loan$verification_status[which(loan$verification_status == "Source Verified")] <- "Verified"
loan$verification_status<-as.factor(loan$verification_status)
loan$loan_status<-as.factor(loan$loan_status)

# shows the different levels that are available after facrtorizing the column.
levels(loan$home_ownership) 
levels(loan$verification_status)
levels(loan$loan_status) 

#converting issue_d to R date format.
loan$issue_d<-as.Date(paste('01-',loan$issue_d,sep = ""),format = c("%d-%b-%y"),tz = "Asia/Calcutta")
#creating a derived metric called year and month and converting them into a factor
loan$issue_d_year<-as.factor(year(loan$issue_d))
loan$issue_d_month<-as.factor(month(loan$issue_d))

# shows the different levels that are available after facrtorizing the column.
levels(loan$issue_d_year)
levels(loan$issue_d_month)

#converting pymnt_plan to factor
loan$pymnt_plan <- as.factor(loan$pymnt_plan)
levels(loan$pymnt_plan)# shows the levels that are available after factorizing the column

#converting purpose to factor and also checking if the data can be made consistent
distinct(loan,purpose) # no duplicates however need to be made consistent.
loan$purpose<-str_replace_all(loan$purpose,"_"," ") # removing the underscore from the data.
loan$purpose <- as.factor(str_to_title(loan$purpose,locale = "en")) # changing the case to title case and converting to factor.
levels(loan$purpose)# shows the levels that are available after factorizing the column

#converting zip code, state to factors.
loan$zip_code<-as.factor(str_replace_all(loan$zip_code,"xx","")) #remvoing "xx" from the value and converting to factor
loan$addr_state<-as.factor(loan$addr_state)# converting state to factor.
# shows the levels that are available after factorizing the column
levels(loan$zip_code)
levels(loan$addr_state)

#converting earliest_cr_line to R date format.
loan$earliest_cr_line<-as.Date(paste('01-',loan$earliest_cr_line,sep=""),format = c("%d-%b-%y"),tz = "Asia/Calcutta")
#Based on the output of CrossTable (below) one can see that the number of entries with 4 and above
#inquiries are few hence it makes sense them to combine them as 4+ entries in all dataset.
# 4 inquiries have 0.8% of data, 5 inquiries have 0.4%, 6 is 0.2%, 7 is 0.1% and 8 is almost close to 0
CrossTable(loan$inq_last_6mths)

#loan$inq_last_6mths[which(loan$inq_last_6mths == "4")] <- 4
loan$inq_last_6mths[which(loan$inq_last_6mths>=4)]<- "4+"
loan$inq_last_6mths<-as.factor(loan$inq_last_6mths)
levels(loan$inq_last_6mths)

#removing the percentage symbol from revol_util and making it numeric
loan$revol_util <- as.numeric(str_replace_all(loan$revol_util,"%",""))

#converting last_pymnt_d, next_pymnt_d,last_credit_pull_d to R date format.
loan$last_pymnt_d<-as.Date(paste('01-',loan$last_pymnt_d,sep = ""),format = c("%d-%b-%y"),tz = "Asia/Calcutta")
loan$next_pymnt_d<-as.Date(paste('01-',loan$next_pymnt_d,sep = ""),format = c("%d-%b-%y"),tz = "Asia/Calcutta")
loan$last_credit_pull_d<-as.Date(paste('01-',loan$last_credit_pull_d,sep = ""),format = c("%d-%b-%y"),tz = "Asia/Calcutta")

#converting application_type to factor and also checking if the data can be made consistent
distinct(loan,application_type) # no duplicates because of case hence no consistency issue, converting to a factor
loan$application_type<-as.factor(loan$application_type) # converting to factor
levels(loan$application_type)# shows the levels that are available after factorizing the column

#looking at the rest of the columns using the data set viewer it looks like lot of them just have NAs
#In case the columns just have NAs then remove the column from the data set to reduce the number of columns to work with
#there are 111 column, 39717 rows
sapply(loan,function(x) sum(is.na(x))) # will show you the columns with sum of NAs. whereever the sum = 39717 same as number of rows,
# those column can be deleted from dataset as it doesn't have any relevant data for analysis
#before deleting the entire datset taking a backup of dataset in case we need it for any conversions
loan_backup <- loan
#The below command  will help us remove the columns that are having NAs same as number of rows
loan<-loan[,colSums(is.na(loan)) != nrow(loan)] 
#after removing the columns which were 100% NAs we can remove those columns which have more than 50% NAs as they will not have any
#relevance for our analysis
#which(colMeans(is.na(loan))>0.5)
loan<-loan[,-which(colMeans(is.na(loan))>0.5)]# to identify which columns have more than 50% NAs

#We can also remove those columns which do not have any relevance for analysis even though they have data in them and 
#not complete NA's
#emp_title has many different values so would not be of relevance for our analysis.
#for demographic we can use state rather than zip_code because from a zip_code the relevance will not be clear
#desc and url will not be used for analysis hence removing them.
distinct(loan,initial_list_status) # only one value for all records, not required for analysis.
distinct(loan,pymnt_plan) # only one value for all records, not required for analysis.
distinct(loan,policy_code) # only one value, not being considered for any analysis
distinct(loan,acc_now_delinq) #no delinquent accounts in data set, not usefule for analysis.
distinct(loan,chargeoff_within_12_mths) #No charge of data and moreover won't be available during application type, hence removing from the analysis.
distinct(loan,delinq_amnt) # no data available for this field. hence disregarding from an analysis perspective.
distinct(loan,tax_liens) # no data available for this field. hence disregarding from an analysis perspective.
distinct(loan,collections_12_mths_ex_med) # no data available for this field. hence disregarding from an analysis perspective.
columns_for_deletion <-c("emp_title","pymnt_plan","url","desc","zip_code","initial_list_status","policy_code",
                         "acc_now_delinq","chargeoff_within_12_mths","delinq_amnt","tax_liens","collections_12_mths_ex_med")

#data set below has all the relevant columns required for analysis with all the different loan statuses.
loan_relevant_data <- loan[,-which(names(loan)%in%columns_for_deletion)]


#--------------------------------------------------------------------------------#
#                        DATA UNDERSTANDING                                      #
#--------------------------------------------------------------------------------#

#In order to perform the analysis we have subsetted the data set from the relevant columns data set to have only the data
# with fully paid and charged off and charged off data set alone.
loan_excluding_current <- loan_relevant_data%>%filter(loan_status != "Current")
#creating a derived metrics called loan_status_bool for the loan status as boolean, if fully paid then value = 0, charged off then value = 1
#loan_excluding_current$loan_status_bool <- ifelse(loan_excluding_current$loan_status == "Fully Paid",0,1)
loan_charged_off <- loan_excluding_current%>%filter(loan_status == "Charged Off")



#detaching plyr package before the below analysis because of the contention b/w plyr and dplyr. this is causing
#the group by summary not work correctly for dplyr
detach(package:plyr)

#------------------------------------------------------------------------------------------------#
# Overall count of applicants Vs Loan Status
loan_stat <- loan_relevant_data %>% group_by(loan_status) %>% summarise(Count = n())
loan_stat$Percentage <- round(loan_stat$Count*100/sum(loan_stat$Count),2)

ggplot(loan_stat, aes(x = loan_status, y = Count)) + 
  geom_bar(stat = 'identity', fill="red", col="black", alpha=0.5) + 
  geom_text(aes(label = paste0(Percentage,'%')), color = "blue", vjust=-0.2) + xlab('Loan Status')+
  labs(title="Loan distribution by Loan Status")


(sum(loan_stat$Count[which(loan_stat$loan_status == 'Charged Off')])*100)/sum(loan_stat$Count)

# Observations : Out of 39717 applicants, 5,627 applicants have defaulted. 14.17% have defaulted
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
# Analysis of Loans ststus vs. Issue year 

loan_IssueYear_status_df <- tally(loan_relevant_data %>%group_by(issue_d_year, loan_status))
loan_IssueYear_status_df<-loan_IssueYear_status_df%>%group_by(issue_d_year)%>%mutate(percent = round(n*100/sum(n),0))

plot_Year_loanStatus <- ggplot(loan_IssueYear_status_df, aes(x = issue_d_year, y = percent, fill = loan_status)) + 
  geom_bar(position = "fill",stat="identity", col="black")
plot_Year_loanStatus <- plot_Year_loanStatus + 
  geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,position = position_fill(vjust=0.5))
plot_Year_loanStatus <- plot_Year_loanStatus + 
  scale_fill_discrete(name = "Loan Status") + scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Loans by Issue Year", x = "Issue year", y = "Percent")+
  theme(axis.text.x = element_text(angle=70, hjust=1))
plot_Year_loanStatus

#------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------
# Analysis of Loans status vs. Loan purpose

loan_purpose_status_df <- tally(loan_relevant_data %>%group_by(purpose, loan_status))
loan_purpose_status_df<-loan_purpose_status_df%>%filter(!(loan_status %in% c("Current")))
loan_purpose_status_df<-loan_purpose_status_df%>%group_by(purpose)%>%mutate(percent = round(n*100/sum(n),0))

plot_purpose_loanStatus <- ggplot(loan_purpose_status_df, aes(x = purpose, y = percent, fill = loan_status)) + 
  geom_bar(position = "fill",stat="identity", col="black")
plot_purpose_loanStatus <- plot_purpose_loanStatus + 
  geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,position = position_fill(vjust=0.5))
plot_purpose_loanStatus <- plot_purpose_loanStatus + 
  scale_fill_discrete(name = "Loan Status") + scale_y_continuous(labels = scales::percent) +
  labs(title = "Loan purpose vs. Loan status", x = "Loan purpose", y = "Percent")+
  theme(axis.text.x = element_text(angle=70, hjust=1))
plot_purpose_loanStatus

#------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------
# Analysis of Charged off loans vs. Annual income

plot_Income_ChargedOff <- ggplot(loan_charged_off, aes(x=annual_inc)) + 
  geom_histogram(breaks=seq(10000, 500000, by=30000), col="black", fill="red", alpha=0.5, aes(fill=y))

plot_Income_ChargedOff <- plot_Income_ChargedOff  + 
  labs(title = "Distribution of Charged off vs. Annual Income", x = "Annual Income", y = "Number of loans")
#plot_Income_ChargedOff <- plot_Income_ChargedOff  + xlim(c(10000,450000))

plot_Income_ChargedOff

# Plotting additional plot restricting the range of data 
# since we see that the high number of defaulters are in the range of 10K to 100K

plot_Income_ChargedOff_detail <- ggplot(loan_charged_off, aes(x=annual_inc)) + 
  geom_histogram(breaks=seq(10000, 100000, by=3000), col="black", fill="red", alpha=0.5, aes(fill=y))

plot_Income_ChargedOff_detail <- plot_Income_ChargedOff_detail  + 
  labs(title = "Charged off loans vs. Annual Income (10-100K)", x = "Annual Income", y = "Number of loans")

plot_Income_ChargedOff_detail

#From the plot we see that high volume of defaulters lie in the salary bucket of 25K to 75K

#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#UNDERSTANDING Verification Status

#Performing univariate analysis on verification status on the entire data set
plot_univariate_verification_status <- ggplot(loan_charged_off,aes(x = verification_status))+
  geom_bar(fill = "red", col="black", alpha=0.5)+
  geom_text(stat = "count",aes(label=..count..),vjust=-.3,size = 3)
plot_univariate_verification_status <- plot_univariate_verification_status + 
  labs(title = "Understanding Verification Status For Charged off loans",x =  "Verification Status", y = "Frequency")
plot_univariate_verification_status
#Observations - With the 62% of verified loans have been defaulted which could be attributed other factors 
#however the loan company can still reduce the 38% of defaulted loans by making sure to verify the income 
#of the loan applicants.

#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING home_ownership across products for charged off df

ggplot(loan_products_charged_off_df, aes(home_ownership)) + 
  geom_bar(stat = 'count', color = "black", fill = "red", alpha=0.5) + 
  xlab('Home Ownership') + geom_text(aes(label = ..count..), color = "blue", vjust=-1, stat = 'count') +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
  facet_wrap(~purpose) + scale_y_continuous(limits = c(0,2500))
# Observations : Home improvement loan are taken and defaulted by applicants who have mortage homeownership
#                Other loan products are taken and defaulted by applicants who have rented accomodation
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING home ownership analysis across differnt years of issue loans for all defaulted laons
home_ownership_data_charged_off <- tally(loan_charged_off %>% group_by(issue_d_year,home_ownership))
#removing the OTHER AND NONE ownership from our analysis as their percentage is very less
home_ownership_data_charged_off<-home_ownership_data_charged_off%>%filter(!(home_ownership %in% c("OTHER","NONE")))
#Calculating the percentage of ownership per year for all defaulted laons
home_ownership_data_charged_off<-home_ownership_data_charged_off%>%group_by(issue_d_year)%>%mutate(percent = round(n*100/sum(n),2))

plot_home_ownership_data_charged_off <- ggplot(home_ownership_data_charged_off,aes(x = issue_d_year, y = percent, fill = home_ownership))+geom_bar(position = "fill",stat="identity")
plot_home_ownership_data_charged_off <- plot_home_ownership_data_charged_off + geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,position = position_fill(vjust=0.5))
plot_home_ownership_data_charged_off <- plot_home_ownership_data_charged_off + scale_fill_discrete(name = "Home Ownership") + scale_y_continuous(labels = scales::percent)
plot_home_ownership_data_charged_off <- plot_home_ownership_data_charged_off + labs(title = "Defaulter Across The Years Vs Home Ownership ",x = "Loan Issue Year", y = "Percentage")
plot_home_ownership_data_charged_off
#Observations - Applicants who already OWN a house are less likely to default because they do not have any percentage 
#monthly spend towards mortgage or rent so they can pay back the loan.
#Applicants who have a house Mortgage default lesser than the applicants who RENT a house because they already 
#go through a rigorous credit check and inquiries while applying a loan. So folks with Mortgage will mostly pay 
#back their loans however some folks default because they might not be able to manage their monthly debts.
#Applicants who RENT are the higher defaulters of all the three hence more riskier than the other applicants.
#For such applicants loan company can decrease the amount being funded.
#------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------#
#UNDERSTANDING open_acc

#Performing univariate analysis on number of open accounts on the entire data set
plot_univariate_open_acc <- ggplot(loan_relevant_data,aes(x = factor(open_acc)))+
  geom_bar(stat = 'count', color = "black", fill = "red", alpha=0.5)+
  geom_text(stat = "count",aes(label=..count..),vjust=-0.2,size = 3,color = "blue")
plot_univariate_open_acc <- plot_univariate_open_acc +labs(title = "Understanding Open Account Distribution",x = "Number Of Open Accnts", y = "Frequency")
plot_univariate_open_acc

loan_products_charged_off_df <- loan_charged_off[which(loan_charged_off$purpose %in% c('Debt Consolidation','Credit Card',
                                                                                       'Home Improvement','Major Purchase','Small Business')),]

plot_univariate_open_acc_purpose <- ggplot(loan_products_charged_off_df,aes(x = factor(open_acc)))+
  geom_bar(stat = 'count', color = "black", fill = "red", alpha=0.5)
plot_univariate_open_acc_purpose <- plot_univariate_open_acc_purpose +
  labs(title = "Understanding Open Account Distribution",x = "Number Of Open Accnts", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
plot_univariate_open_acc_purpose + facet_wrap(~purpose)

#Observations - From the trend we can see that a minimum of 2 open accounts is necesscary for getting a loan with the company.
#Overall the distribution of maximum loans have open account b/w 3 and 13
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Loan products
loan_products <- loan_relevant_data %>% group_by(purpose) %>% summarise(Count = n())
loan_products$Percentage <- round(loan_products$Count*100/sum(loan_products$Count),2)

ggplot(loan_products, aes(x = purpose, y = Count)) + 
  geom_bar(stat = 'identity',col="black", fill="red", alpha = 0.5) + 
  geom_text(aes(label = paste0(Percentage,'%')), color = "blue", vjust=-0.2) +
  labs(title = "Overall top 5 loan products", x = "Loan Products", y = "Count")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) 

# Observations : Five popular loan products are : debt_consolidation, credit_card, home_improvement,
#                major_purchase, small_business

#ALERT!!! - THIS PLOT HAS BEEN REPLACED IN THE PPT
#loan_products_status <- loan_excluding_current %>% group_by(purpose,loan_status) %>% summarise(Count = n())
#loan_products_status_graph <- ggplot(loan_products_status, aes(x = purpose, y = Count)) + geom_bar(stat = 'identity', fill="red", col="black", alpha=0.5) + 
#  geom_text(aes(label = Count), color = "blue", vjust=-1) + xlab('Loan Products') +
#  theme(axis.text.x = element_text(angle = 70, hjust = 1))
#loan_products_status_graph + facet_grid(loan_status ~ .) + scale_y_continuous(limits = c(0,17000))

#-------------------------------------------------------------------------------------------------
# ************************* Analysis of Loan status vs. Loan purpose ****************************
#-------------------------------------------------------------------------------------------------

loan_purpose_status_df <- tally(loan_relevant_data %>%group_by(purpose, loan_status))
loan_purpose_status_df<-loan_purpose_status_df%>%filter(!(loan_status %in% c("Current")))
loan_purpose_status_df<-loan_purpose_status_df%>%group_by(purpose)%>%mutate(percent = round(n*100/sum(n),0))

plot_purpose_loanStatus <- ggplot(loan_purpose_status_df, aes(x = purpose, y = percent, fill = loan_status)) + 
  geom_bar(position = "fill",stat="identity", col="black")
plot_purpose_loanStatus <- plot_purpose_loanStatus + 
  geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,position = position_fill(vjust=0.5))
plot_purpose_loanStatus <- plot_purpose_loanStatus + 
  scale_fill_discrete(name = "Loan Status") + scale_y_continuous(labels = scales::percent) +
  labs(title = "Loan purpose vs. Loan status", x = "Loan purpose", y = "Percent")+
  theme(axis.text.x = element_text(angle=70, hjust=1))
plot_purpose_loanStatus
#------------------------------------------------------------------------------------------------#

# Year of funding the loan  
#ALERT!!! - THIS PLOT is NOT IN THE PPT
ggplot(loan_relevant_data, aes(factor(issue_d_year))) + 
  geom_bar(stat = 'count',color = "black", fill = "red", alpha=0.5) + 
  xlab('Year of funding loan') + 
  geom_text(aes(label = ..count..), color = "blue", vjust=-.2, stat = 'count')+
  labs(title = "Loan distribution by Year", x = "Year of issue", y = "Count")
# Observations : There have been gradual increase in the default applicants over the years from 2007 to 2011

#ALERT!!! - THIS PLOT is NOT IN THE PPT
ggplot(loan_excluding_current, aes(factor(issue_d_year), fill = loan_status)) + 
  geom_bar(stat = 'count', position = position_dodge(width = 1)) + 
  xlab('Year of funding loan') + 
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count', position = position_dodge(width = 1))+
  labs(title = "Loan distribution by Year by loan status", x = "Year of issue", y = "Count")
# Observations : There have been gradual increase in the default applicants over the years from 2007 to 2011

#------------------------------------------------------------------------------------------------#

loan_products_df <- loan_excluding_current[which(loan_excluding_current$purpose %in% c('Debt Consolidation','Credit Card',
                                                                                   'Home Improvement','Major Purchase','Small Business')),]
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING loan_amnt across products for charged off df

# loan_amnt : Checking the distribution of loan_amnt in the loan_charged_off data set
options(scipen = 10000)
ggplot(loan_products_charged_off_df, aes(loan_amnt)) + geom_histogram(aes(y=..density..), bins = 20, fill = "white", color = "black") +
  xlab('Loan Amount') + geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(0, 40000, 2000), lim = c(0, 40000)) + 
  labs(title = "Loan amount distribution by top loan products", x = "Loan Amount", y = "Count") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + facet_wrap(~purpose) 

# The graph is right skewed for almost all the loan products. But loan amount taken for Major Purchase
# and defaulting have high density
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING loan_amnt across products for charged off df

# funded_amnt : Checking the distribution of funded_amnt in the loan_charged_off data set
#ALERT!!! - THIS PLOT is NOT IN THE PPT
ggplot(loan_products_charged_off_df, aes(funded_amnt)) + 
  geom_histogram(aes(y=..density..), bins = 20, fill = "white", color = "black") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(0, 40000, 2000), lim = c(0, 40000)) + 
  labs(title = "Funded amount distribution by top loan products", x = "Funded Amount", y = "Count") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + facet_wrap(~purpose)

# The graph is right skewed for almost all the loan products. But funded amount given for Major Purchase
# and defaulting have high density
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING term across products for charged off df
ggplot(loan_products_charged_off_df, aes(term)) + 
  geom_bar(stat = 'count', color = "black", fill = "red", alpha=0.5) + 
  xlab('Term') + geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count') + 
  labs(title = "Charged off loan distribution by loan term", x = "Loan term", y = "Count") +
  facet_wrap(~purpose)  + scale_y_continuous(limits = c(0,1800))
# Observations : 61.25% applicants have defaulted with 36 months of term for debt_consolidation loan
#                product whereas 66.39% applicants have defaulted for 60 months of term for the same product

#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING installment across products for charged off df

#ALERT!!! - THIS PLOT is NOT IN THE PPT
ggplot(loan_products_charged_off_df, aes(installment)) + 
  geom_histogram(aes(y=..density..), bins = 20, fill = "white", color = "black") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(0, 1600, 200), lim = c(0, 1600)) + 
  labs(title = "Installment amount distribution by top loan products", x = "Installment Amount", y = "Count") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + facet_wrap(~purpose)  
# Observations : The applicants taking loan for major_purchase have high density around 0-200
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING emp_length across products for charged off df
ggplot(loan_products_charged_off_df, aes(emp_length)) + 
  geom_bar(stat = 'count', color = "black", fill = "red", alpha=0.5) +
  xlab('Employment Length in Years') + 
  geom_text(aes(label = ..count..), color = "blue", vjust=-0.2, stat = 'count') +
  labs(title = "Charged off loans for Employee length by top loan products", x = "Employment length", y = "Count") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
  facet_wrap(~purpose) + scale_y_continuous(limits = c(0,900))
# Observations : The applicants having 10 or more than 10 years if employment are likely to default.
#                Additionally, there are high number of defaulters who have taken loan for 
#                debt_consolidation 
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING inq_last_6mths

#Performing univariate analysis on last 6 months inquiries distribution on the entire data set
#ALERT!!! - THIS PLOT is NOT IN THE PPT
plot_univariate_inq_last_6mth <- ggplot(loan_relevant_data,aes(x = factor(inq_last_6mths)))+
  geom_bar(fill = "red", alpha=0.5, col="black")+geom_text(stat = "count",aes(label=..count..),vjust=-0.2,size = 3)
plot_univariate_inq_last_6mth <- plot_univariate_inq_last_6mth + 
  labs(title = "Understanding last 6 months Credit Inquiries",x = "6 Months Inquiries", y = "Frequency")
plot_univariate_inq_last_6mth
#Observations - directly using the credit inquiries will not reveal much however lenders with very high number of inquiries
#are considered as risky applicants as it is an evidence for more credit seeking behaviour. In the entire data set with all loan status
# folks with 2 and above inquiries can be identified as risky applicants as they have higher inquiries within a span of 6 months.
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
##last 6 months inquiries against grade for the Charged Off Loans
#subsetting data set by loan grade for charged off loans
inquiries_6mths_all_charged <- tally(loan_charged_off %>% group_by(inq_last_6mths,grade)) 
#calculating the percentage of each loan status grouped by the loan grade
inquiries_6mths_all_charged<-inquiries_6mths_all_charged%>%group_by(grade)%>%mutate(percent = round(n*100/sum(n),2))
p_inq_last_6mths_charged <- ggplot(inquiries_6mths_all_charged,aes(x = grade, y = percent, fill = inq_last_6mths))+geom_bar(position = "fill",stat="identity")
p_inq_last_6mths_charged <- p_inq_last_6mths_charged + geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,position = position_fill(vjust=0.5))
p_inq_last_6mths_charged <- p_inq_last_6mths_charged + scale_fill_discrete(name = "Inquiries in last 6 months") + scale_y_continuous(labels = scales::percent)
p_inq_last_6mths_charged <- p_inq_last_6mths_charged + labs(title = "Percentage of Inquiries in last 6 months Vs Grade For Defaulted Loans",x = "Loan Grade", y = "Percentage")
p_inq_last_6mths_charged
#Observations - Applicants with zero inquiries across different loan grades are risky clients because of their unknown 
#credit seeking behavior. Data shows that majority of folks with 0 inquiries have defaulted more. It is better not to 
#give them loan to avoid defaults or fund them a lesser loan amount.
#Applicants with 1 inquiries are also risky because with one inquiry on their credit file doesn’t reflect their 
#true credit seeking behavior. It is better to be cautious while giving loans to such applicants.
#Percentage of defaulters with 2 inquiries are relatively same across loan grades so they are less riskier applicants.
#Folks with 3 inquiries and above can also be classified as riskier applicants because within a short span to time 
#they have a requirement of more credit and by not giving them loans one can reduce the percentage of default loans.
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING Home ownership
#Performing univariate analysis on home ownershop status on the entire data set
plot_univariate_home_owner <- ggplot(loan_relevant_data,aes(x = home_ownership))+geom_bar(fill = "red", alpha=0.5, col="black")+
  geom_text(stat = "count",aes(label=..count..),vjust=0,size = 3) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) 
plot_univariate_home_owner <- plot_univariate_home_owner + labs(title = "Understanding Home Ownership",x = "By Ownership", y = "Frequency")
plot_univariate_home_owner
#Observation - From the plot one can see that the majority of applicants RENT a house rather than OWN/Mortgage
#------------------------------------------------------------------------------------------------#

#UNDERSTANDING DTI
#DTI (Debt to Income ratio) for all defaulted loans
#range has been created by looking at the min and max value available for this data
loan_charged_off$dti_range <- ifelse(loan_charged_off$dti >=0 & loan_charged_off$dti<10,"0-10",
                                     ifelse(loan_charged_off$dti>=10 & loan_charged_off$dti<20,"10-20","20-30"))
loan_charged_off$dti_range<-as.factor(loan_charged_off$dti_range)
#Making sure the right levels exist.
levels(loan_charged_off$dti_range)
#Subsetting the data to calculate the percentage of applicants who fall in a certain dti range for all defaulted loans
dti_defaulters_rate <- tally(loan_charged_off%>%group_by(dti_range,loan_status))
dti_defaulters_rate <- dti_defaulters_rate%>%group_by(loan_status)%>%mutate(percent = round(n*100/sum(n),2))
plot_dti_defaulters <- ggplot(dti_defaulters_rate,aes(x = dti_range, y = percent))+
  geom_bar(position = "dodge",stat="identity",fill = "red", col="black", alpha=0.5)
plot_dti_defaulters <- plot_dti_defaulters + geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,hjust = 0.5,vjust = -0.2,position = position_dodge(1))
plot_dti_defaulters <- plot_dti_defaulters + scale_y_continuous(labels = scales::percent)
plot_dti_defaulters <- plot_dti_defaulters + labs(title = "Understanding DTI For Charged Off Loans",x = "DTI Range", y = "Percentage")
plot_dti_defaulters
#Observations - As the DTI ratio goes above 10 the percentage of defaulting the loan also increases. 
#If the DTI is available during the loan application processing then in order to avoid loan defaults, 
#loans for applicants with DTI greater than 10 can be rejected or the funded amount can be reduced.

#analysing the dti for the charged off loans across the 5 popular loan products
#Five popular loan products are : debt_consolidation, credit_card, home_improvement,
#major_purchase, small_business
#for the created range calulcated the percentage of purpose across the range
dti_defaulters_rate_loan_purpose_charged_off <- tally(loan_charged_off%>%filter(purpose%in%c('Debt Consolidation','Credit Card',
                                                                                             'Home Improvement','Major Purchase','Small Business'))%>%group_by(dti_range,purpose))
dti_defaulters_rate_loan_purpose_charged_off <- dti_defaulters_rate_loan_purpose_charged_off%>%group_by(dti_range)%>%mutate(percent = round(n*100/sum(n),2))
plot_dti_defaulters_loan_purpose_charged_off <- ggplot(dti_defaulters_rate_loan_purpose_charged_off,aes(x = dti_range, y = percent, fill = purpose))+
  geom_bar(position = "dodge",stat="identity")
plot_dti_defaulters_loan_purpose_charged_off <- plot_dti_defaulters_loan_purpose_charged_off + 
  geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,hjust = 0.5,vjust = -0.5,position = position_dodge(1))
plot_dti_defaulters_loan_purpose_charged_off <- plot_dti_defaulters_loan_purpose_charged_off + 
  scale_fill_discrete(name = "Top 5 Loan Products") 
plot_dti_defaulters_loan_purpose_charged_off <- plot_dti_defaulters_loan_purpose_charged_off+labs(title = "Understanding DTI For Charged Off Loans Across Top 5 Loan Products",x = "DTI Range", y = "Percentage")
plot_dti_defaulters_loan_purpose_charged_off
#Observation -Across the top 5 loan product, debt consolidation with DTI greater than or equal to 10 seems to 
#have higher chances of defaulting. Applicants with DTI >= 10 and loan purpose of DEBT Consolidation are
#likely to default.
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING revol_util

#Creating a range for the revolving utilization percentage.For a good credit score it is always better to have a 
#revolving utlization less than 20%
loan_charged_off$revol_util_range <- ifelse(loan_charged_off$revol_util >=0  & loan_charged_off$revol_util <10,"0-10%",
                                            ifelse(loan_charged_off$revol_util >=10 & loan_charged_off$revol_util <20,"10-20%",
                                                   ifelse(loan_charged_off$revol_util >=20 & loan_charged_off$revol_util <30,"20-30%",
                                                          ifelse(loan_charged_off$revol_util >=30 & loan_charged_off$revol_util <40,"30-40%",">40%"))))
loan_charged_off$revol_util_range<-as.factor(loan_charged_off$revol_util_range)
#Checking if the right levels are being stored.
levels(loan_charged_off$revol_util_range)
#Substteing the defaulted loan dataset to calculate the percentage of revolving utilization range
revol_util_defaulters_rate <- tally(loan_charged_off%>%group_by(revol_util_range,loan_status))
#removing NAs so as not to consider for analysis as the count is very less
revol_util_defaulters_rate<-revol_util_defaulters_rate[-which(is.na(revol_util_defaulters_rate$revol_util_range)),]
revol_util_defaulters_rate <- revol_util_defaulters_rate%>%group_by(loan_status)%>%mutate(percent = round(n*100/sum(n),2))


plot_revol_util_defaulters <- ggplot(revol_util_defaulters_rate,aes(x = revol_util_range, y = percent))+
  geom_bar(position = "dodge",stat="identity",fill = "red", col="black", alpha=0.5)
plot_revol_util_defaulters <- plot_revol_util_defaulters + 
  geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,hjust = 0.5,vjust = -0.2,position = position_dodge(1))
plot_revol_util_defaulters <- plot_revol_util_defaulters
plot_revol_util_defaulters <- plot_revol_util_defaulters + labs(title = "Understanding Revol Util For Charged Off Loans",x = "Revol Util Range", y = "Percentage")
plot_revol_util_defaulters
#Observation - The overall percentage of applicants with revolving utilization greater than 20% who have defaulted 
#loans is about 86%. Such applicants are likely to default on their loans as they are not able to manage 
#their credit line and are risky loan applicants.

#analysing the revol util for the charged off loans across the 5 popular loan products
#Five popular loan products are : debt_consolidation, credit_card, home_improvement,
#major_purchase, small_business
#for the created range calulcated the percentage of purpose across the range for defaulted loans
revol_util_defaulters_rate_loan_purpose_charged_off <- tally(loan_charged_off%>%filter(purpose%in%c('Debt Consolidation','Credit Card',
    'Home Improvement','Major Purchase','Small Business') & !(is.na(revol_util_range))) 
    %>%group_by(revol_util_range,purpose))
revol_util_defaulters_rate_loan_purpose_charged_off <- revol_util_defaulters_rate_loan_purpose_charged_off%>%group_by(revol_util_range)%>%mutate(percent = round(n*100/sum(n),0))
p_revol_util_defaulters_loan_purpose_charged_off <- ggplot(revol_util_defaulters_rate_loan_purpose_charged_off,aes(x = revol_util_range, y = percent, fill = purpose))+geom_bar(position = "dodge",stat="identity")
p_revol_util_defaulters_loan_purpose_charged_off <- p_revol_util_defaulters_loan_purpose_charged_off + geom_text(aes(y=percent,label=paste0(percent,"%")),size = 3,hjust = 0.5,vjust = -0.2,position = position_dodge(1))
p_revol_util_defaulters_loan_purpose_charged_off <- p_revol_util_defaulters_loan_purpose_charged_off + scale_fill_discrete(name = "Top 5 Loan Products")
p_revol_util_defaulters_loan_purpose_charged_off <- p_revol_util_defaulters_loan_purpose_charged_off+labs(title = "Understanding Revol Utilization for Charged Off Loans Across Top 5 Loan Products",x = "Revol Util Range", y = "Percentage")
p_revol_util_defaulters_loan_purpose_charged_off
#Observations - For Debt Consolidation loan purpose, there is a drastic increase in loan defaults where the 
#revolving utilization is greater than 20%.
#Applicants with revolving utilization greater than 20% especially for Debt Consolidation can be deemed as risky
#and likely to default on their loans

#------------------------------------------------------------------------------------------------#

#to understand the open account distirbution better for charged off loans let us create a range and then see how the 
#distribution of this range is.
#before we create the range lets check if there are any NAs. if there are we can eliminate them from calculation.
sum(is.na(loan_charged_off$open_acc))# no NAs
loan_charged_off$open_acc_range <- ifelse(loan_charged_off$open_acc >=0  & loan_charged_off$open_acc <5,"0-5",
                                          ifelse(loan_charged_off$open_acc >=5 & loan_charged_off$open_acc <10,"5-10",
                                                 ifelse(loan_charged_off$open_acc >=10 & loan_charged_off$open_acc <15,"10-15", ">15")))
loan_charged_off$open_acc_range<-as.factor(loan_charged_off$open_acc_range)
#Confirming the levels available.
levels(loan_charged_off$open_acc_range)

#Subsetting the data set to calculate the percentage of open_accounts (range) for all defaulted loans
open_acc_defaulters_rate <- tally(loan_charged_off%>%group_by(open_acc_range,loan_status))
open_acc_defaulters_rate <- open_acc_defaulters_rate%>%group_by(loan_status)%>%mutate(percentage = round(n*100/sum(n),2))

plot_open_acc_defaulters_rate <- ggplot(open_acc_defaulters_rate,aes(x = open_acc_range, y = percentage))+
  geom_bar(position = "dodge",stat="identity",fill = "red", col="black", alpha=0.5)
plot_open_acc_defaulters_rate <- plot_open_acc_defaulters_rate + geom_text(aes(y=percentage,label=paste0(percentage,"%")),size = 3,hjust = 0.5,vjust = -0.5,position = position_dodge(1))
plot_open_acc_defaulters_rate <- plot_open_acc_defaulters_rate + scale_y_continuous(labels = scales::percent)
plot_open_acc_defaulters_rate <- plot_open_acc_defaulters_rate + labs(title = "Understanding Open Acc For Charged Off Loans",x = "Open Acc Range", y = "Percentage")
plot_open_acc_defaulters_rate
#Observations - With the increase in number of Open Account beyond 5 the percentage of defaulting a loan increases drastically.
#It is advisable to give loans to applicants with open accounts up to 5 and reject the loan applications with 
#open accounts above 5.
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#UNDERSTANDING pub_rec
#Performing univariate analysis on number of derogatory public records for the defauled loans
#ALERT!!! - THIS PLOT is NOT IN THE PPT
plot_univariate_pub_rec_charged_off <- ggplot(loan_charged_off,aes(x = factor(pub_rec)))+
  geom_bar(fill = "red", col="black", alpha=0.5)+geom_text(stat = "count",aes(label=..count..),vjust=0,size = 3)
plot_univariate_pub_rec_charged_off <- plot_univariate_pub_rec_charged_off +labs(title = "Understanding Applicant Public Records Distribution For Defaulted Loans",x = "# Of Public Records", y = "Frequency")
plot_univariate_pub_rec_charged_off
#Observation - From the observation majority of the applicants who have defaulted the loans. 
#There is not much of insight if you just look at the distribution.
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
#Peforming univariate segmenting it by verification status
#ALERT!!! - THIS PLOT is NOT IN THE PPT
plot_univariate_pub_rec_charged_off_verification <- ggplot(loan_charged_off,aes(x = factor(pub_rec),fill = verification_status))+geom_bar(position = "stack")+geom_text(stat = "count",aes(label=..count..),size = 2,hjust = 0.5,vjust = -0.5)
plot_univariate_pub_rec_charged_off_verification <- plot_univariate_pub_rec_charged_off_verification +labs(title = "Understanding Applicant Public Records Distribution who have Defaulted And verification status",x = "# Of Public Records", y = "Frequency")
plot_univariate_pub_rec_charged_off_verification <- plot_univariate_pub_rec_charged_off_verification + scale_fill_discrete(name = "Verification Status")
plot_univariate_pub_rec_charged_off_verification
#Observations - Even though the applicants who have zero derogatory remarks have defaulted 
#however good set of them do not have income verified which makes them risky applicants and likely to default. 
#------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------------#
# Checking the correlation between quantitive variables for data frames
numeric_columns <- c('loan_amnt','funded_amnt','funded_amnt_inv','int_rate','installment','annual_inc','dti','open_acc')
loan_relevant_data_numeric <- loan_relevant_data[,colnames(loan_relevant_data) %in% numeric_columns,drop=FALSE]
cor_loan_relevant_data <- cor(loan_relevant_data_numeric)
corrplot(cor_loan_relevant_data, method = "shade")
corrplot(cor_loan_relevant_data, method = "number")
# Observations : There are high positive correlation between loan_amnt and funded_amnt, loan_amnt and funded_amnt_inv,
#                loan_amnt and installment, funded_amnt and installment
#                There is slight negative correlation between dti and annual income which is obvious



