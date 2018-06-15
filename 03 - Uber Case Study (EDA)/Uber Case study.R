# Load file 
Uber_df <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)

# Load lubridate & chrom package for manupilating and conducting operations on dates and times
install.packages("lubridate")
library("lubridate")

install.packages("chron")
library("chron")

# Load other required libraries for plotting and manipulating data for analysis
install.packages("ggplot2")
library("ggplot2")

install.packages("gridExtra")
library("GridExtra")

install.packages("plyr")
library("plyr")

install.packages("dplyr")
library("dplyr")

# Check the structure of Uber dataframe
str(Uber_df)

#___________________________________________________________________________________________________________________#
# *************************** Data cleansing: Start - Request date and Drop date formatting ************************
#___________________________________________________________________________________________________________________#

# Using parse_date_time method from lubridate package to format the Uber Request time
# This is to standardize the data format as they data is provided in different formats
Uber_df$Request.time<-parse_date_time(Uber_df$Request.timestamp,orders = c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"),tz = "Asia/Calcutta")

# Using parse_date_time method from lubridate package to format the Uber drop time
# This is to standardize the data format as they data is provided in different formats
Uber_df$Drop.time<-parse_date_time(Uber_df$Drop.timestamp,orders = c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"),tz = "Asia/Calcutta")

# Using strptime to convert the formatted Request.Time & Drop.Time data so we can derive 
# the time dimensions for Request and drop times
Uber_df$RequestTime <- strptime(Uber_df$Request.time, format = "%Y-%m-%d %H:%M:%S")
Uber_df$DropTime <- strptime(Uber_df$Drop.time, format = "%Y-%m-%d %H:%M:%S")

#___________________________________________________________________________________________________________________#
# *************************** Data cleansing: Start - Request date and Drop date formatting ************************
#___________________________________________________________________________________________________________________#

#___________________________________________________________________________________________________________________#
# ********************************* Univariant Analysis: Request time  *********************************************
#___________________________________________________________________________________________________________________#

# Getting the day of week - to see request frequency by day to look for any patterns
Uber_df_final$RequestDay <- format(Uber_df_final$RequestTime, "%A")

# Creating new data frame with key fields
Uber_df_final <- Uber_df[,c(1,2,3,4,9,10)]

#Derive the request date
Uber_df_final$Request.Date <- format(Uber_df_final$RequestTime, "%d-%m-%Y")

#Derive the request time
Uber_df_final$Request.Time <- format(Uber_df_final$RequestTime, "%H:%M:%S")

#Derive the drop date
Uber_df_final$Drop.Date <- format(Uber_df_final$DropTime, "%d-%m-%Y")

#Derive the drop time
Uber_df_final$Drop.Time <- format(Uber_df_final$DropTime, "%H:%M:%S")

#Creating new data frame reordering the columns
Uber_df_final2 <- Uber_df_final[,c(1,2,3,4,5,7,8,11,6,9,10)]

#Get travel time to look for any patterns
Uber_df_final2$Travel.Time <- Uber_df_final2$DropTime - Uber_df_final2$RequestTime

#Splitting Request time into slots - to conduct analysis on number of trip requests by time slots
#LN(Late Night - 12 AM to 3 AM)
#EM(Early Morning - 3 AM to 6 AM)
#M(Morning - 6 AM to 9 AM)
#LM(Late Morning - 9 AM to 12 PM)
#A(Afternoon - 12 PM to 3 PM)
#LA(Late Afternoon - 3 PM to 6 PM)
#E(Evening - 6 PM to 9 PM)
#N(Night - 9 PM to 12 AM
Uber_df_final2$Req_Time_Slots<-cut(times(format(Uber_df_final2$RequestTime,"%H:%M:%S")),breaks = times(c("00:00:00","03:00:00","06:00:00","09:00:00","12:00:00","15:00:00","18:00:00","21:00:00","23:59:59")),labels = c("LN","EM","M","LM","A","E","LE","N"),include.lowest = TRUE,right = FALSE)

#Creating trip status column to analyze the trip completion success 
Uber_df_final2$Trip.Status <- ifelse(Uber_df_final2$Status == "Trip Completed", "Y", "N")

#Converting day of week to factor for plotting frequency histogram
Uber_df_final2$Request.Day <- as.factor(Uber_df_final2$Request.Day)

#ggplot to see requests by day of week - trying to identify if there are any patterns
plot_Request_by_day <- ggplot(Uber_df_final2, aes(x = Uber_df_final2$Request.Day)) + geom_bar(fill = "blue", position = "dodge") + labs(title="Requests by day of week", x="Request Day", y="Count of Requests", caption = "Based on data from Uber India") 

plot_Request_by_day

#***********************************************************************
#Anylsys result: 
#1. The Uber requests seem to be more or less constant during the week
#2. There is no weekend data in the dataset provided
#***********************************************************************

# to remove the request date, time, drop date, time
Uber_df_final2 <- Uber_df_final2[,-6]
Uber_df_final2 <- Uber_df_final2[,-6]
Uber_df_final2 <- Uber_df_final2[,-8]
Uber_df_final2 <- Uber_df_final2[,-8]

#___________________________________________________________________________________________________________________#
# ********************************** Assessing Airport vs City request issues **************************************
#___________________________________________________________________________________________________________________#

#Uber Airport dataset - irrespective of status
uber_Airport <- tally(Uber_df_final2%>%filter(Pickup.point == "Airport")%>%group_by(Pickup.point,Req_Time_Slots,Status))

#Uber Airport dataset - only missed trips (where status is "Cancelled" or "No cars available")
uber_Airport_Unavailable <-tally(Uber_df_final2%>%filter(Pickup.point == "Airport" & (Status =="Cancelled" | Status =="No Cars Available"))%>%group_by(Pickup.point,Req_Time_Slots,Status))

#Plot showing Number of requests that are "Cancelled" or "No cars available" status by timeslot of day from Airport
plot_Airport_unavailable <- ggplot(uber_Airport_Unavailable,aes(x = Req_Time_Slots,y = n, fill = Status))+geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=n, y = n + 0.05),position = position_dodge(0.9), vjust = 0)+labs(title = "Airport to City",x = "By Time Slots", y = "Requests",caption = "LN(Late Night: 12 AM-3 AM)\nEM(Early Morning: 3 AM-6 AM)\nM(Morning: 6 AM-9 AM)\nLM(Late Morning: 9 AM-12 PM)\nA(Afternoon: 12 PM-3 PM)\nE(Evening: 3 PM-6 PM)\nLE(Late Evening: 6 PM-9 PM)\nN(Night: 9 PM-12 AM)")

#***********************************************************************
#Anylsys result: 
#1. We can see that during Late Evening 
#2. There is no weekend data in the dataset provided
#***********************************************************************

#Uber city - irrespective of status
uber_City <- tally(Uber_df_final2%>%filter(Pickup.point == "City")%>%group_by(Pickup.point,Req_Time_Slots,Status))

#Uber City dataset - only missed trips (where status is "Cancelled" or "No cars available")
uber_City_Unavailable <-tally(Uber_df_final2%>%filter(Pickup.point == "City" & (Status =="Cancelled" | Status =="No Cars Available"))%>%group_by(Pickup.point,Req_Time_Slots,Status))

#Plot showing Number of requests that are "Cancelled" or "No cars available" status by timeslot of day from City
plot_City_unavailable <- ggplot(uber_City_Unavailable,aes(x = Req_Time_Slots,y = n, fill = Status))+geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=n, y = n + 0.05),position = position_dodge(0.9), vjust = 0)+labs(title = "City to Airport",x = "By Time Slots", y = "Requests",caption = "LN(Late Night: 12 AM-3 AM)\nEM(Early Morning: 3 AM-6 AM)\nM(Morning: 6 AM-9 AM)\nLM(Late Morning: 9 AM-12 PM)\nA(Afternoon: 12 PM-3 PM)\nE(Evening: 3 PM-6 PM)\nLE(Late Evening: 6 PM-9 PM)\nN(Night: 9 PM-12 AM)")
                        
#Display the plots of Cancelled and no cabs available from Airport and City
grid.arrange(plot_Airport_unavailable, plot_City_unavailable, nrow=1, ncol = 2)

#____________________________________________________________________________________________________________________#

# ************************************** Assessing Supply vs. Demand issues ****************************************

#Getting the count of demand in a demand dataset
uber_Demand <-tally(Uber_df_final2%>%group_by(Pickup.point,Req_Time_Slots))
colnames(uber_Demand)[3]<-"Demand"

#Getting the count of supply in a supply dataset
uber_Supply <-tally(Uber_df_final2%>%filter(Status == "Trip Completed")%>%group_by(Pickup.point,Req_Time_Slots))
colnames(uber_Supply)[3]<-"Supply"

#Merging the Demand vs. Supply for combination of Pickup point & Time slots
Uber_Supply_Demand <- merge(x=uber_Supply,y=uber_Demand, by.x=c("Pickup.point","Req_Time_Slots"), by.y=c("Pickup.point","Req_Time_Slots"))

Uber_Supply_Demand$Gap <- Uber_Supply_Demand$Demand - Uber_Supply_Demand$Supply

#Demand plot *********************************************
plot_Demand <- ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,y = Demand, fill=Pickup.point))+geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=Demand, y = Demand + 0.05),position = position_dodge(0.9), vjust = 0)+labs(title = "Supply by pickup point",x = "By Time Slots", y = "Supply",caption = "LN(Late Night: 12 AM-3 AM)\nEM(Early Morning: 3 AM-6 AM)\nM(Morning: 6 AM-9 AM)\nLM(Late Morning: 9 AM-12 PM)\nA(Afternoon: 12 PM-3 PM)\nE(Evening: 3 PM-6 PM)\nLE(Late Evening: 6 PM-9 PM)\nN(Night: 9 PM-12 AM)")

#Supply plot *********************************************
plot_Supply <- ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,y = Supply, fill=Pickup.point))+geom_bar(stat = "identity",position = "dodge")+geom_text(aes(label=Supply, y = Supply + 0.05),position = position_dodge(0.9), vjust = 0)+labs(title = "Demand by pickup point",x = "By Time Slots", y = "Demand",caption = "LN(Late Night: 12 AM-3 AM)\nEM(Early Morning: 3 AM-6 AM)\nM(Morning: 6 AM-9 AM)\nLM(Late Morning: 9 AM-12 PM)\nA(Afternoon: 12 PM-3 PM)\nE(Evening: 3 PM-6 PM)\nLE(Late Evening: 6 PM-9 PM)\nN(Night: 9 PM-12 AM)")

grid.arrange(plot_Demand, plot_Supply, nrow=1, ncol = 2)

# ****************************************************************************************************************************
# ******************************************************** Extra code ******************************************************** 
# ****************************************************************************************************************************
plot_supply_demand <- ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,group = Pickup.point))
+geom_bar(stat = "identity",position = "dodge")
+geom_bar(aes(label=Demand, y = Demand + 0.05),position = position_dodge(0.9), vjust = 0)
+geom_bar(aes(label=Supply, y = Supply + 0.05),position = position_dodge(0.9), vjust = 0)
+facet_wrap(~ Pickup.point)

ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,group = Pickup.point))+geom_bar(stat = "identity",position = "dodge")+geom_bar(aes(label=Demand, y = Demand + 0.05),position = position_dodge(0.9), vjust = 0)+geom_bar(aes(label=Supply, y = Supply + 0.05),position = position_dodge(0.9), vjust = 0)+facet_wrap(~ Pickup.point)

plot_supply_demand <- ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,group = Pickup.point))+geom_bar(aes(y = n, color = "Airport"))+geom_bar(aes(y = n, color = "City"))+facet_wrap(~ Pickup.point)

plot_supply_demand <- ggplot(Uber_Supply_Demand,aes(x = Req_Time_Slots,group = Pickup.point))+geom_line(aes(y = Demand, color = "Demand"))+geom_line(aes(y = Supply, color = "Supply"))+facet_wrap(~ Pickup.point)+scale_color_manual(values = c("black","yellow"))

write.csv(Uber_df_final2, "uber_final_f2.csv")

write.csv(Uber_Supply_Demand, "Uber_Supply_Demand.csv")


# ****************************************************************************************************************************
# ******************************************************** Extra code ******************************************************** 
# ****************************************************************************************************************************