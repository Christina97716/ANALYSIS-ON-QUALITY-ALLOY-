###############################
# Read File and Merge Compatable Tables
###############################

library(readxl)
library(psych)
library(ggplot2)
library(data.table)

visits <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet='Weekly Visits',range='A5:H71')
financials <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet='Financials',range='A5:E71')
table1 <-merge(visits,financials, sort=FALSE)
View(table1)

###############################
# Creating New Columns
###############################

# Number of New Visits
table1$new_visits <- c()
table1$new_visits <- round(table1$Visits * table1$`% New Visits`)

# Cost
table1$cost <- c()
table1$cost <- round(table1$Revenue - table1$Profit)

###############################
# Segment Data Based on Period
###############################

Initial <- table1[1:14,]

Pre_Promotion <- table1[15:35,]
  
Promotion <- table1[36:52,]

Post_Promotion <- table1[53:66,]

###############################
# Initial Bar Charts
###############################

# Unique Visits Over Time
Unique_Visits_Chart <- ggplot(table1, aes(`Week (2008-2009)`, `Unique Visits`)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Unique Visits by Week") +
  theme(axis.text.x = element_text(angle=90))
Unique_Visits_Chart

# Revenue Over Time
Revenue_Chart <- ggplot(table1, aes(`Week (2008-2009)`, `Revenue`)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Revenue by Week") +
  theme(axis.text.x = element_text(angle=90))
Revenue_Chart

# Profit Over Time
Profit_Chart <- ggplot(table1, aes(`Week (2008-2009)`, `Profit`)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Profit by Week") +
  theme(axis.text.x = element_text(angle=90))
Profit_Chart

# Lbs. Sold Over Time
lbs_Chart <- ggplot(table1, aes(`Week (2008-2009)`, `Lbs. Sold`)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Lbs. Sold by Week") +
  theme(axis.text.x = element_text(angle=90))
lbs_Chart

###############################
# Summary Table on Specified Columns
###############################

#funciton for SUMMARY MEASURES
mefunc<-function(df){
  col<-colnames(table1)
  v1<-c(2,3,9,10,11)
  v2<-c('vars','n','mean','sd','median','trimmed','mad','min','max','range','skew','kurtosis','se')
  rname<-c()
  des<-as.data.frame(matrix(nrow=5,ncol=13))
  for(i in 1:5){des[i,]<-describe(df[,v1[i]])
  rname[i]<-col[v1[i]]
  }
  row.names(des)<-rname
  names(des)<-v2
  return(des)
}

Initial_Summary <- mefunc(df=Initial)
Initial_Summary

Pre_Promotion_Summary <-mefunc(df=Pre_Promotion)
Pre_Promotion_Summary

Promotion_Summary <-mefunc(df=Promotion)
Promotion_Summary

Post_Promotion_Summary <-mefunc(df=Post_Promotion)
Post_Promotion_Summary

###############################
# Full Summary of Each Period to show column summaries
###############################

summary(Initial)
summary(Pre_Promotion)
summary(Promotion)
summary(Post_Promotion)

###############################
# Create tables for mean of different variables through each period
###############################

# Focus on Mean of Visits for Each Period
Visits_Mean <- c(mean(Initial$Visits), mean(Pre_Promotion$Visits), mean(Promotion$Visits), mean(Post_Promotion$Visits))

# Focus on Mean of Unique Visits for Each Period
Unique_Visits_Mean <- c(mean(Initial$`Unique Visits`), mean(Pre_Promotion$`Unique Visits`), mean(Promotion$`Unique Visits`), mean(Post_Promotion$`Unique Visits`))

# Focus on Mean of Revenue for Each Period
Revenue_Mean <- c(mean(Initial$Revenue), mean(Pre_Promotion$Revenue), mean(Promotion$Revenue), mean(Post_Promotion$Revenue))

# Focus on Mean of Profit for Each Period
Profit_Mean <- c(mean(Initial$Profit), mean(Pre_Promotion$Profit), mean(Promotion$Profit), mean(Post_Promotion$Profit))

# Focus on Mean of Lbs. Sold for Each Period
lbs_Sold_Mean <- c(mean(Initial$`Lbs. Sold`), mean(Pre_Promotion$`Lbs. Sold`), mean(Promotion$`Lbs. Sold`), mean(Post_Promotion$`Lbs. Sold`))

# Create Table
Period_Table <- data.table(Period = c("Initial", "Pre-Promotion", "Promotion", "Post-Promotion"),
                           Visits = Visits_Mean,
                           Unique_Visits = Unique_Visits_Mean,
                           Revenue = Revenue_Mean,
                           Profit = Profit_Mean,
                           Lbs_Sold = lbs_Sold_Mean, stringsAsFactors = FALSE)

# Chart for Visits
Visits_Period_Chart <- ggplot(Period_Table, aes(x=Period_Table$Period, y=Period_Table$Visits)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Visits by Period", x = "Period", y = "Average Visits") +
  scale_x_discrete(limits = Period_Table$Period)
Visits_Period_Chart 

# Chart for Unique Visits
Unique_Visits_Period_Chart <- ggplot(Period_Table, aes(x=Period_Table$Period, y=Period_Table$Unique_Visits)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Unique Visits by Period", x = "Period", y = "Average Unique Visits") +
  scale_x_discrete(limits = Period_Table$Period)
Unique_Visits_Period_Chart 

# Chart for Revenue
Revenue_Period_Chart <- ggplot(Period_Table, aes(x=Period_Table$Period, y=Period_Table$Revenue)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Revenue by Period", x = "Period", y = "Average Revenue") +
  scale_x_discrete(limits = Period_Table$Period)
Revenue_Period_Chart

# Chart for Profit
Profit_Period_Chart <- ggplot(Period_Table, aes(x=Period_Table$Period, y=Period_Table$Profit)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Profit by Period", x = "Period", y = "Average Profit") +
  scale_x_discrete(limits = Period_Table$Period)
Profit_Period_Chart 

# Chart for Lbs. Sold
lbs_Sold_Period_Chart <- ggplot(Period_Table, aes(x=Period_Table$Period, y=Period_Table$Lbs_Sold)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Lbs.Sold by Period", x = "Period", y = "Average Lbs. Sold") +
  scale_x_discrete(limits = Period_Table$Period)
lbs_Sold_Period_Chart 

##############################

# Focus on Mean of New_Visits for Each Period
new_visits_Mean <- c(mean(Initial$new_visits), mean(Pre_Promotion$new_visits), mean(Promotion$new_visits), mean(Post_Promotion$new_visits))

# Focus on Mean of Cost for Each Period
cost_Mean <- c(mean(Initial$cost), mean(Pre_Promotion$cost), mean(Promotion$cost), mean(Post_Promotion$cost))

# Focus on Mean of Bounce Rate for Each Period
Bounce_Mean <- c(mean(Initial$`Bounce Rate`), mean(Pre_Promotion$`Bounce Rate`), mean(Promotion$`Bounce Rate`), mean(Post_Promotion$`Bounce Rate`))

# Focus on Mean of Inquiries for Each Period
Inquiries_Mean <- c(mean(Initial$Inquiries), mean(Pre_Promotion$Inquiries), mean(Promotion$Inquiries), mean(Post_Promotion$Inquiries))


# Create New Table with More Variables
New_Period_Table <- data.table(Period = c("Initial", "Pre-Promotion", "Promotion", "Post-Promotion"),
                           New_Visits = new_visits_Mean,
                           Cost = cost_Mean,
                           Bounce_Rate = Bounce_Mean,
                           Inquiries = Inquiries_Mean, stringsAsFactors = FALSE)

# Chart for New Visits
new_visits_Period_Chart <- ggplot(New_Period_Table, aes(x=New_Period_Table$Period, y=New_Period_Table$New_Visits)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average New Visits by Period", x = "Period", y = "Average New Visits") +
  scale_x_discrete(limits = New_Period_Table$Period)
new_visits_Period_Chart

# Chart for Cost
Cost_Period_Chart <- ggplot(New_Period_Table, aes(x=New_Period_Table$Period, y=New_Period_Table$Cost)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Cost by Period", x = "Period", y = "Cost") +
  scale_x_discrete(limits = New_Period_Table$Period)
Cost_Period_Chart

# Chart for Bounce Rate
Bounce_Period_Chart <- ggplot(New_Period_Table, aes(x=New_Period_Table$Period, y=New_Period_Table$Bounce_Rate)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Bounce Rate by Period", x = "Period", y = "Average Bounce Rate") +
  scale_x_discrete(limits = New_Period_Table$Period)
Bounce_Period_Chart

# Chart for Inquiries
Inquiries_Period_Chart <- ggplot(New_Period_Table, aes(x=New_Period_Table$Period, y=New_Period_Table$Inquiries)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="Average Inquiries by Period", x = "Period", y = "Average Inquiries") +
  scale_x_discrete(limits = New_Period_Table$Period)
Inquiries_Period_Chart 


###############################
# Plotting Revenue vs. Lbs. Sold
###############################

Rev_Lbs_Chart <- ggplot(table1, aes(x=table1$`Lbs. Sold`, y=table1$Revenue, color = table1$Visits)) +
  geom_point() +
  ggtitle("Revenue vs. Lbs. Sold") + 
  xlab("Lbs. Sold") + 
  ylab("Revenue")
Rev_Lbs_Chart

# Correlation between Lbs.Sold and Revenue
Rev_Lbs_corr <- cor(table1$`Lbs. Sold`,table1$Revenue)

###############################
# Plotting Revenue vs. Unique Visits
###############################

Rev_U_Visits_Chart <- ggplot(table1, aes(x=table1$`Unique Visits`, y=table1$Revenue)) +
  geom_point() +
  ggtitle("Revenue vs. Unique Visits") + 
  xlab("Unique Visits") + 
  ylab("Revenue")
Rev_U_Visits_Chart

# Correlation between Lbs.Sold and Revenue
Rev_U_Visits_corr <- cor(table1$`Unique Visits`,table1$Revenue)

###############################
# Plotting Revenue vs. Bounce Rate
###############################

Rev_Bounce_Chart <- ggplot(table1, aes(x=table1$`Bounce Rate`, y=table1$Revenue)) +
  geom_point() +
  ggtitle("Revenue vs. Bounce Rate") + 
  xlab("Bounce Rate") + 
  ylab("Revenue")
Rev_Bounce_Chart

# Correlation between Lbs.Sold and Revenue
Rev_Bounce_corr <- cor(table1$`Bounce Rate`,table1$Revenue)

###############################
# Plotting New Visits vs. Inquiries
###############################

NewVis_Inquiries_Chart <- ggplot(table1, aes(x=table1$new_visits, y=table1$Inquiries)) +
  geom_point() +
  ggtitle("New Visits vs. Inquiries") + 
  xlab("New Visits") + 
  ylab("Inquiries")
NewVis_Inquiries_Chart

# Correlation between Lbs.Sold and Revenue
NewVis_Inquiries_corr <- cor(table1$new_visits,table1$Inquiries)

###############################
# Histograms
###############################

# Skewness and Kurtosis (Have to install moments package)
library(moments)

skewness(table1$Visits)

kurtosis(table1$Visits)

# Histogram for full data
hist(table1$Visits, 
     main="Histogram for Daily Visits", 
     border="black", 
     col="cyan", 
     las=1)

# Histogram for Initial data
hist(Initial$Visits, 
     main="Histogram for Daily Visits - Initial Period", 
     border="black", 
     col="cyan", 
     las=1)

# Histogram for Pre-Promotion data
hist(Pre_Promotion$Visits, 
     main="Histogram for Daily Visits - Pre-Promotion Period", 
     border="black", 
     col="cyan", 
     las=1)

# Histogram for Promotion data
hist(Promotion$Visits, 
     main="Histogram for Daily Visits - Promotion Period", 
     border="black", 
     col="cyan", 
     las=1)

# Histogram for Post-Promotion data
hist(Post_Promotion$Visits, 
     main="Histogram for Daily Visits - Post-Promotion Period", 
     border="black", 
     col="cyan", 
     las=1)


###############################
# Junjie Linear Regression
###############################

regressionfunc<-function(df){
  df1<-df
  df1<-df1[,-1]
  df1<-df1[,-8]
  df1<-df1[,-10]
  df1<-df1[,-11]
  regre<-lm(Profit~.,data=df1)
  return(summary(regre))
} 
regressionfunc(table1)
