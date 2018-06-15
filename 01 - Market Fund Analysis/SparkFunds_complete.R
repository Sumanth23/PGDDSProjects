# Clearing the workspace
rm(list = ls())

# Loading Libraries
library(dplyr)
library(reshape2)

#------------------------------------------------------------------------------------------------#
#                               CHECK POINT 1 - Data Cleaning                                    #          
#------------------------------------------------------------------------------------------------#
# Loading companies data and rounds2 data
companies <- read.csv('companies.txt', sep = "\t",fill = TRUE, header = TRUE, stringsAsFactors = FALSE)
rounds2 <- read.csv('rounds2.csv', stringsAsFactors = FALSE)

# How many unique companies are present in rounds2?
# Changing the values of company_permalink into lower case so that data can be uniform
rounds2$company_permalink <- tolower(rounds2$company_permalink)
length(unique(rounds2$company_permalink))     # Ans - 66368

# How many unique companies are present in companies?
# Changing the values of permalink into lower case so that the data can be uniform
companies$permalink <- tolower(companies$permalink)
length(unique(companies$permalink))    # Ans - 66368

#In the companies data frame, which column can be used as the unique key for each company? 
# In order to find the unique key present, we can use sapply to find out that which
# column have the same number of rows as the total number of rows of the companies df
# If the length(unique(<<each column>>)) equals nrow(comapnies), that column can be treated as 
# a unique key

sapply(companies, function(x) length(unique(x)))    #Ans - permalink

#permalink    name        homepage_url category_list        status  country_code    state_code    region 
#66368         66038         61192         27297             4           138           312          1093 
#city    founded_at 
#5112          3979 
# Only permalink column have same number of unique rows as the nrow of companies df i.e. 66368

# Are there any companies in the rounds2 file which are not present in companies? 
# In order to  find the companies that are there in rounds2 data frame and not in companies, 
# we can use the setdiff command
# setdiff(a$x, b$y) means element which is in a$x but not in b$y

round2_companies_not_present_in_companies <- as.data.frame(setdiff(rounds2$company_permalink, companies$permalink))
nrow(round2_companies_not_present_in_companies)
# Since the above gives result as 0, there are no companies in rounds2 which is not there in companies df

# Merge the two data frames so that all variables (columns) in the companies frame 
# are added to the rounds2 data frame. 
# Name the merged frame master_frame. How many observations are present in master_frame?
# In order to merge the companies df in rounds2 df, we will use merge command. by.x and by.y is used because
# matching variable have different name
master_frame <- merge(x = rounds2, y = companies, by.x = "company_permalink", by.y = "permalink", all = TRUE)

# Master frame has nrow(master_frame) = 114949

#------------------------------------------------------------------------------------------------#
#                               CHECK POINT 2 - Investment Type Analysis                         #          
#------------------------------------------------------------------------------------------------#

# Checking for any NA value in funding_round_type column
sum(is.na(master_frame$funding_round_type))    # The result comes as 0
# we can also check by using any(is.na()) OR anyNA() - not to get a count just to check for NA's
any(is.na(master_frame$funding_round_type))
anyNA(master_frame$funding_round_type)

# Converting funding_round_type as factors because it is a categorical variable
master_frame$funding_round_type <- as.factor(master_frame$funding_round_type)  
funding_types <- unique(master_frame$funding_round_type)
funding_types
# There are 14 funding types listed in the master frame namely -
# venture, seed, undisclosed, equity_crowdfunding, convertible_note, private_equity, debt_financing,
# angel, grant, secondary_market, post_ipo_equity, post_ipo_debt, product_crowdfunding and non_equity_assistance


# 1. Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) 
master_frame$funding_round_type <- tolower(master_frame$funding_round_type) 
venture_investment_amount <- as.data.frame(master_frame[which(master_frame$funding_round_type == 'venture'),c('funding_round_type','raised_amount_usd')])
mean_venture <- mean(venture_investment_amount$raised_amount_usd, na.rm = TRUE)
mean_venture <- round(mean_venture,2)

#11748949.13

angel_investment_amount <- as.data.frame(master_frame[which(master_frame$funding_round_type == 'angel'),c('funding_round_type','raised_amount_usd')])
mean_angel <- mean(angel_investment_amount$raised_amount_usd, na.rm = TRUE)
mean_angel <- round(mean_angel,2)

#958694.47

seed_investment_amount <- as.data.frame(master_frame[which(master_frame$funding_round_type == 'seed'),c('funding_round_type','raised_amount_usd')])
mean_seed <- mean(seed_investment_amount$raised_amount_usd, na.rm = TRUE)
mean_seed <- round(mean_seed,2)

#719818

private_equity_investment_amount <- as.data.frame(master_frame[which(master_frame$funding_round_type == 'private_equity'),c('funding_round_type','raised_amount_usd')])
mean_private_equity <- mean(private_equity_investment_amount$raised_amount_usd, na.rm = TRUE)
mean_private_equity <- round(mean_private_equity,2)

#73308593.03

investment_type <- c("venture","angel","seed","private equity")
mean_investment_type <- c(mean_venture, mean_angel, mean_seed, mean_private_equity)

average_investment_funding_type <- data.frame(investment_type, mean_investment_type)
colnames(average_investment_funding_type) <- c("Funding_Type", "Average_Funding_Amount")

# Filtering based on Average Funding Amount constraint to find out the suitable investment type

suitable_investment_type <- filter(average_investment_funding_type, (Average_Funding_Amount>=5000000 & 
                                     Average_Funding_Amount<=15000000 ))


# 2. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for it? 
# Based on the analysis obtained from the above, Venture type of investment is most suitable for Sparks Fund

#------------------------------------------------------------------------------------------------#
#                               CHECK POINT 3 - Country Analysis                                 #          
#------------------------------------------------------------------------------------------------#

venture_master_frame <- master_frame[which(master_frame$funding_round_type == 'venture'),]

# Creating a data frame containing the country name, country code and English Speaking status

Country_Code <- c('IND' , 'USA' , 'CHN' , 'HKG' , 'CAN' , 'CHL' , 'GBR' , 'FRA' , 'AUS' , 'DNK' , 'ROM' , 'AUT' , 'KOR' , 'NLD' , 'SWE' , 'JPN' , 'RUS' , 'SGP' , 'NOR' , 'COL' , 'ESP' , 'BEL' , 'IRL' , 'ITA' , 'ISR' , 'NZL' , 'CZE' , 'DEU' , 'CHE' , 'BRA' , 'BGR' , 'SVN' , 'JOR' , 'HUN' , 'BWA' , 'NGA' , 'FIN' , 'TUR' , 'IDN' , 'ARE' , 'CYP' , 'POL' , 'CRI' , 'PRT' , 'ARG' , 'TWN' , 'KHM' , 'THA' , 'SVK' , 'UKR' , 'LTU' , 'ISL' , 'ZAF' , 'MEX' , 'VEN' , 'URY' , 'MUS' , 'KEN' , 'PHL' , 'MNE' , 'VNM' , 'GHA' , 'PSE' , 'MYS' , 'PER' , 'EGY' , 'PAN' , 'LVA' , 'GGY' , 'ALB' , 'UGA' , 'HRV' , 'EST' , 'LBN' , 'GRC' , 'PAK' , 'NPL' , 'LUX' , 'SAU' , 'IRN' , 'GTM' , 'BAH' , 'ARM' , 'BGD' , 'MDA' , 'SRB' , 'TUN' , 'ZWE' , 'BRB' , 'NIC' , 'TAN' , 'PRI' , 'TTO' , 'BHR' , 'CMR' , 'BLR' , 'CYM' , 'BRN' , 'ECU' , 'SLV' , 'MLT' , 'ZMB' , 'MKD' , 'GIB' , 'TGO' , 'LAO' , 'BMU' , 'HND' , 'MCO' , 'MMR' , 'KAZ' , 'QAT' , 'DOM' , 'MAR' , 'LIE' , 'GEO' , 'MOZ' , 'DZA' , 'JAM' , 'KNA' , 'AZE' , 'SEN' , 'RWA' , 'DMA' , 'UZB' , 'BLZ' , 'OMN' , 'JEY' , 'KWT' , 'CIV' , 'BLM' , 'GRD' , 'LKA' , 'SOM' , 'SYC' , 'PRY' , 'MAF')
Country_Name <- c('INDIA' , 'UNITED STATES' , 'CHINA' , 'HONG KONG' , 'CANADA' , 'CHILE' , 'UNITED KINGDOM' , 'FRANCE' , 'AUSTRALIA' , 'DENMARK' , 'ROM' , 'AUSTRIA' , 'SOUTH KOREA' , 'NETHERLANDS' , 'SWEDEN' , 'JAPAN' , 'RUSSIA' , 'SINGAPORE' , 'NORWAY' , 'COLOMBIA' , 'SPAIN' , 'BELGIUM' , 'IRELAND' , 'ITALY' , 'ISRAEL' , 'NEW ZEALAND' , 'CZECH REBUPLIC' , 'GERMANY' , 'SWITZERLAND' , 'BRAZIL' , 'BULGARIA' , 'SLOVENIA' , 'JORDAN' , 'HUNGARY' , 'BOTSWANA' , 'NIGERIA' , 'FINLAND' , 'TURKEY' , 'INDONESIA' , 'UNITED ARAB EMIRATES' , 'CYPRUS' , 'POLAND' , 'COSTA RICA' , 'PORTUGAL' , 'ARGENTINA' , 'TAIWAN' , 'CAMBODIA' , 'THAILAND' , 'SLOVAKIA' , 'UKRAIN' , 'LITHUANIA' , 'ICELAND' , 'SOUTH AFRICA' , 'MEXICO' , 'VENEZUELA' , 'URUGUAY' , 'MAURITIUS' , 'KENYA' , 'PHILIPPINES' , 'MONTENEGRO' , 'VIETNAM' , 'GHANA' , 'PALESTINE' , 'MALAYSIA' , 'PERU' , 'EGYPT' , 'PANAMA' , 'LATVIA' , 'GUERNSEY' , 'ALBANIA' , 'UGANDA' , 'CROATIA' , 'ESTONIA' , 'LEBANON' , 'GREECE' , 'PAKISTAN' , 'NEPAL' , 'LUXEMBOURG' , 'SAUDI ARABIA' , 'IRAN' , 'GUATEMALA' , 'BAHRAIN' , 'ARMENIA' , 'BANGADESH' , 'MOLDOVA' , 'SERBIA' , 'TUNISIA' , 'ZIMBABWE' , 'BARBADOS' , 'NICARAGUA' , 'TANZANIA' , 'PUERTO RICO' , 'TRINIDAD AND TOBAGO' , 'BAHRAIN' , 'CAMEROON' , 'BELARUS' , 'CAYMAN ISLANDS' , 'BRUNEI' , 'ECUADOR' , 'EL SALVADOR' , 'MALTA' , 'ZAMBIA' , 'MACEDONIA' , 'GIBRALTAR' , 'TOGO' , 'LAOS' , 'BERMUDA' , 'HONDURAS' , 'MONACO' , 'MYANMAR' , 'KAZAKHSTAN' , 'QATAR' , 'DOMINICAN REPUBLIC' , 'MOROCCO' , 'LIECHTENSTEIN' , 'GEORGIA' , 'MOZAMBIQUE' , 'ALGERIA' , 'JAMAICA' , 'SAINT KITTS AND NEVIS' , 'AZERBAIJAN' , 'SENEGAL' , 'RWANDA' , 'DOMINICA' , 'UZBEKISTAN' , 'BELIZE' , 'OMAN' , 'JERSEY' , 'KUWAIT' , 'IVORY COAST' , 'SAINT BARTHELEMY' , 'GRENADA' , 'SRI LANKA' , 'SOMALIA' , 'SEYCHELLES' , 'PARAGUAY' , 'SAINT MARTIN')
English_Speaking <- c('YES' , 'YES' , 'NO' , 'NO' , 'YES' , 'NO' , 'YES' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'YES' , 'YES' , 'YES' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'YES' , 'NO' , 'YES' , 'NO' , 'YES' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'YES' , 'NO' , 'NO' , 'YES' , 'YES' , 'NO' , 'YES' , 'NO' , 'NO' , 'NO' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO' , 'YES' , 'NO' , 'NO')
country_and_codes <- data.frame(Country_Code, Country_Name, English_Speaking)

venture_master_frame <- merge(x = venture_master_frame, y = country_and_codes, 
                              by.x = 'country_code', by.y = 'Country_Code',
                              all.x = TRUE)


country_name_raised_amount <- venture_master_frame[which(venture_master_frame$English_Speaking == 'YES'),] %>% group_by(Country_Name) %>% summarise(raised_amount_usd = sum(raised_amount_usd, na.rm = TRUE))

# Sorting in descending order by raised_amount_usd
country_name_raised_amount <- country_name_raised_amount[order(-country_name_raised_amount$raised_amount_usd),]
top9 <- head(country_name_raised_amount, 9)

# Based on the result obtained above, top three English Speaking Countries are - United States, United Kingdom and India


#------------------------------------------------------------------------------------------------#
#                               CHECK POINT 4 - Sector Analysis 1                                #          
#------------------------------------------------------------------------------------------------#

mapping <- read.csv('mapping.csv', stringsAsFactors = FALSE)

# Removing row no 1 as it corresponds to "Blanks" sector
mapping <- mapping[-1,]

# Converting the category_list and bringing them all
# to lowercase to maintain uniformity
mapping$category_list <- tolower(mapping$category_list)
master_frame$category_list <- tolower(master_frame$category_list)

# Data cleaning for category_list column in mapping.csv
# "na" in category_list is replaced by 0. Finding all the values that have 0 in the column value
# and converting them to na except enterprise 2.0 as it is a correct data

mapping$category_list <- gsub('0','na',mapping$category_list)
mapping$category_list[which(mapping$category_list == 'enterprise 2.na')] <- 'enterprise 2.0'

# Creating a primary_sector column in companies data frame to store the primary sector from category_list column
master_frame$primary_sector <- lapply(master_frame$category_list, function(x) strsplit(x, "[|]")[[1]][1])
master_frame$primary_sector <- tolower(master_frame$primary_sector)

# Converting category_list column to factor
mapping$category_list <- as.factor(mapping$category_list)

# Converting mapping dataframe from wide format to long format and choosing only those rows 
# where the value is 1
mapping_long_format <- melt(mapping, id.vars = c("category_list"))
names(mapping_long_format) <- c("category_list","main_sector","value")
mapping_long_format <- mapping_long_format[which(mapping_long_format$value == 1),]


# Merge the master_frame and mapping_long_format data frame to get the main_sector in master_frame df
master_frame <- merge(x = master_frame, y = mapping_long_format, by.x = "primary_sector", by.y = "category_list", all.x = TRUE)

# Removing the last column "value"
master_frame$value <- NULL
master_frame$main_sector <- as.factor(tolower(master_frame$main_sector))


#------------------------------------------------------------------------------------------------#
#                               CHECK POINT 5 - Sector Analysis 2                                #          
#------------------------------------------------------------------------------------------------#

# All the columns of the master_frame along with the primary sector and the main sector

D1 <- filter(master_frame,funding_round_type =="venture" & country_code == 'USA' & 
             (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 ))
D2 <- filter(master_frame,funding_round_type =="venture" & country_code == 'GBR' & 
             (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 ))
D3 <- filter(master_frame,funding_round_type =="venture" & country_code == 'IND' & 
             (raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000 ))

#The total number (or count) of investments and total amount invested for each main sector in a separate column

temp_D1 <- D1 %>% group_by(main_sector) %>% summarise(count_of_investment = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))
temp_D2 <- D2 %>% group_by(main_sector) %>% summarise(count_of_investment = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))
temp_D3 <- D3 %>% group_by(main_sector) %>% summarise(count_of_investment = n(), total_amount_invested = sum(raised_amount_usd, na.rm = T))

D1 <- merge(D1, temp_D1)
D2 <- merge(D2, temp_D2)
D3 <- merge(D3, temp_D3)

#------------------------------------------------------------------------------------------------#
#                               Data Analysis for Sector                                         #          
#------------------------------------------------------------------------------------------------#


# 1. Total number of Investments (count)
D1_count_of_investment <- sum(unique(D1$count_of_investment))   # Ans - 12150
D2_count_of_investment <- sum(unique(D2$count_of_investment))   # Ans - 628
D3_count_of_investment <- sum(unique(D3$count_of_investment))   # Ans - 330

# 2. Total amount of investment (USD)
D1_amount_of_investment <- sum(unique(D1$total_amount_invested))   # Ans - 108531347515 
D2_amount_of_investment <- sum(unique(D2$total_amount_invested))   # Ans - 5436843539
D3_amount_of_investment <- sum(unique(D3$total_amount_invested))   # Ans - 2976543602

# A function to find out the top n sectors based on the count of investment
# Arguments : [1] data frame and [2] numeric value to be passed based on top n required

find_top_n_sectors <- function(df, n) {
  df_temp <- unique(df)
  top_n_df <- head(df_temp[order(df_temp[["count_of_investment"]], decreasing= T),], n = n)
  return(top_n_df)
}


# 3,4,5,6,7,8 - collated to give the desired output
# Top, second and third sector names along with number of investments 

# for C1
C1_top_3_sectors <- find_top_n_sectors(D1[,c("main_sector", "count_of_investment")], 3)
C1_top_3_sectors

# for C2
C2_top_3_sectors <- find_top_n_sectors(D2[,c("main_sector", "count_of_investment")], 3)
C2_top_3_sectors

# for C3
C3_top_3_sectors <- find_top_n_sectors(D3[,c("main_sector", "count_of_investment")], 3)
C3_top_3_sectors

# To convert from exponential form to numeric for very large column values
options(scipen = 999)

# 9. For point 3 (top sector count-wise), which company received the highest investment?
# C1
D1_highest_investment_others <- D1[which(D1$main_sector == 'others'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D1_highest_investment_others <- D1_highest_investment_others[order(-D1_highest_investment_others$investment_amount),]  
# Ans - Virtustream

# C2
D2_highest_investment_others <- D2[which(D2$main_sector == 'others'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D2_highest_investment_others <- D2_highest_investment_others[order(-D2_highest_investment_others$investment_amount),]  
# Ans - Electric Cloud

# C3
D3_highest_investment_others <- D3[which(D3$main_sector == 'others'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D3_highest_investment_others <- D3_highest_investment_others[order(-D3_highest_investment_others$investment_amount),]  
# Ans - FirstCry.com

# 10. For point 4 (second best sector count-wise), which company received the highest investment?
# C1
D1_highest_investment_second <- D1[which(D1$main_sector == 'social..finance..analytics..advertising'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D1_highest_investment_second <- D1_highest_investment_second[order(-D1_highest_investment_second$investment_amount),]  
# Ans - SST Inc. (Formerly ShotSpotter)

# C2
D2_highest_investment_second <- D2[which(D2$main_sector == 'social..finance..analytics..advertising'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D2_highest_investment_second <- D2_highest_investment_second[order(-D2_highest_investment_second$investment_amount),]  
# Ans - Celltick Technologies

# C3
D3_highest_investment_second <- D3[which(D3$main_sector == 'social..finance..analytics..advertising'),] %>% group_by(name) %>% summarise(investment_amount = sum(raised_amount_usd))
D3_highest_investment_second <- D3_highest_investment_second[order(-D3_highest_investment_second$investment_amount),]  
# Ans - Manthan Systems
