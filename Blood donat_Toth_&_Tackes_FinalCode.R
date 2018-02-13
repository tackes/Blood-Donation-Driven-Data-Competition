library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(PerformanceAnalytics)
library(ggvis)
library(class)
library(gmodels)
library(plyr)

# load the data
#setwd("C:/Users/nm49027/Documents/Personal/PREDICT 454/Blood Donation/")
#setwd("C:/Users/jeffrey.tackes/Desktop/Predict 454 Data/")
#training <- read.csv("Training Data.csv") # load the "training.csv" file
training <- read.csv("/Users/gellerttoth/Desktop/PREDICT 495/training.csv") # load the "training.csv" file
training$designation<-"training"
dim(training)
ls(training)
head(training)
count('training$Made.Donation.in.March.2007')
138/(438+138)
1-138/(438+138)


#test <- read.csv("Test Data.csv") # load the "test.csv" file
test <- read.csv("/Users/gellerttoth/Desktop/PREDICT 495/test.csv") # load the "test.csv" file
test$designation<-"test"
test$Made.Donation.in.March.2007<-NA
dim(test)
ls(test)
head(test)

#merge training and test
total <- rbind(training,test)
dim(total)
write.csv(total,"total.csv")
#write.csv(total,"/Users/gellerttoth/Desktop/PREDICT 495/total.csv")
total <- read.csv("total.csv") # load the "training.csv" file



##########################################################################
##################### Exploratory Data Analysis ##########################
##########################################################################

########  Made.Donation.in.March.2007  ################################
madedonation<-ggplot(data = total) + geom_bar(mapping = aes(x =Made.Donation.in.March.2007))
print(madedonation + ggtitle("Made Donation in March 2007"))
require(dplyr)
total%>%
count('Made.Donation.in.March.2007')




#####################################################################################
#################   1. Months.since.First.Donation   ################################
#####################################################################################

#original variable
months_since_first<-ggplot(data = total) + geom_bar(mapping = aes(x = Months.since.First.Donation ))
print(months_since_first + ggtitle("Months Since First Donation"))


require(dplyr)
total%>%
count('Months.since.First.Donation')

#create yearly categories from monthly data
#1 year intervals 
attach(total)
total$first_don_cat1[Months.since.First.Donation <= 12] <- "< 1 year"
total$first_don_cat1[Months.since.First.Donation > 12 & Months.since.First.Donation <= 24] <- "1-2 Yrs"
total$first_don_cat1[Months.since.First.Donation> 24 & Months.since.First.Donation <= 36] <- "2-3 Yrs"
total$first_don_cat1[Months.since.First.Donation> 36 & Months.since.First.Donation <= 48] <- "3-4 Yrs"
total$first_don_cat1[Months.since.First.Donation > 48 & Months.since.First.Donation <= 60] <- "4-5 Yrs"
total$first_don_cat1[Months.since.First.Donation> 60 & Months.since.First.Donation <= 72] <- "5-6 Yrs"
total$first_don_cat1[Months.since.First.Donation> 72 & Months.since.First.Donation <= 84] <- "6-7 Yrs"
total$first_don_cat1[Months.since.First.Donation> 84] <- "7+ Yrs"
detach(total)
years_since_first_donation<-ggplot(data = total) + geom_bar(mapping = aes(x = first_don_cat1))
print(years_since_first_donation + ggtitle("Years Since Last Donation"))


boxplot1<-ggplot(data = total, mapping = aes(x = first_don_cat1, y =Months.since.First.Donation)) + geom_boxplot()
print(boxplot1 + ggtitle("Months Since First Donation by Yearly Category"))

#create a more normal distribution
#first year is quarterly, all others are yearly
attach(total)
total$first_don_cat2[Months.since.First.Donation<= 3] <- "< 3 mos "
total$first_don_cat2[Months.since.First.Donation > 3 & Months.since.First.Donation <= 6] <- "3-6 mos"
total$first_don_cat2[Months.since.First.Donation > 6 & Months.since.First.Donation <= 12] <- "6-12 mos"
total$first_don_cat2[Months.since.First.Donation> 12 & Months.since.First.Donation <= 24] <- "1-2 Yrs"
total$first_don_cat2[Months.since.First.Donation > 24 & Months.since.First.Donation <= 36] <- "2-3 Yrs"
total$first_don_cat2[Months.since.First.Donation> 36 & Months.since.First.Donation <= 48] <- "3-4 Yrs"
total$first_don_cat2[Months.since.First.Donation> 48 & Months.since.First.Donation <= 60] <- "4-5 Yrs"
total$first_don_cat2[Months.since.First.Donation> 60 & Months.since.First.Donation <= 72] <- "5-6 Yrs"
total$first_don_cat2[Months.since.First.Donation> 72 & Months.since.First.Donation <= 84] <- "6-7 Yrs"
total$first_don_cat2[Months.since.First.Donation > 84] <- "7+ Yrs"
detach(total)
years_since_first_donation2<-ggplot(data = total) + geom_bar(mapping = aes(x = first_don_cat2))
print(years_since_first_donation2 + ggtitle("Time Since First Donation"))

#create binary variable
#less than 12 months since 1st donation vs. more than 12 months since 1st donation
attach(total)
total$first_don_cat3[Months.since.First.Donation<= 12] <- "< 12 mos "
total$first_don_cat3 [Months.since.First.Donation > 12] <- "12+ mos"
detach(total)
years_since_first_donation3<-ggplot(data = total) + geom_bar(mapping = aes(x = first_don_cat3))
print(years_since_first_donation3 + ggtitle("New Donors (1st Dination within Most Recent Year) vs. Old Donors"))


#########################################################################
########   Months.since.Last.Donation  ##################################
#########################################################################

months_since_last<-ggplot(data = total) + geom_bar(mapping = aes(x = Months.since.Last.Donation))
print(months_since_last + ggtitle("Months Since Last Donation"))


total%>%
count('Months.since.Last.Donation')

#create yearly  categories: Recency of Last Donation in Years
#More than 12 months could be inactive
#More than 24 months may be impossible to reactivate
attach(total)
total$last_don_cat1[Months.since.Last.Donation <= 12] <- "< 1 year"
total$last_don_cat1[Months.since.Last.Donation > 12 & Months.since.Last.Donation <= 24] <- "1-2 Yrs"
total$last_don_cat1[Months.since.Last.Donation  > 24] <- "2+ Yrs"
detach(total)
years_since_last_donation<-ggplot(data = total) + geom_bar(mapping = aes(x = last_don_cat1))
print(years_since_last_donation + ggtitle("Years Since Last Donation"))
boxplot2<-ggplot(data = total, mapping = aes(x = last_don_cat1, y =Months.since.Last.Donation)) + geom_boxplot()
print(boxplot2 + ggtitle("Months Since Last Donation by Yearly Category"))

#create quarterly  categories: Recency of Last Donation in Quarters
attach(total)
total$last_don_cat2[Months.since.Last.Donation  <= 3] <- "< 1"
total$last_don_cat2[Months.since.Last.Donation > 3 & Months.since.Last.Donation <= 6] <- "2-3"
total$last_don_cat2[Months.since.Last.Donation  > 6 & Months.since.Last.Donation <= 9] <- "3-4"
total$last_don_cat2[Months.since.Last.Donation  > 9 & Months.since.Last.Donation <= 12] <- "4-5"
total$last_don_cat2[Months.since.Last.Donation  > 12 & Months.since.Last.Donation <= 16] <- "5-6"
total$last_don_cat2[Months.since.Last.Donation  > 16] <- "6+"
detach(total)
qtrs_since_last_donation2<-ggplot(data = total) + geom_bar(mapping = aes(x = last_don_cat2))
print(qtrs_since_last_donation2 + ggtitle("Quarters Since Last Donation"))

boxplot3<-ggplot(data = total, mapping = aes(x = last_don_cat2, y = Months.since.Last.Donation  )) + geom_boxplot()
print(boxplot3 + ggtitle("Quarters Since Last Donation by Yearly Category"))


######################################################################
########  Number.of.Donations   ######################################
######################################################################
num_donat<-ggplot(data = total) + geom_bar(mapping = aes(x =Number.of.Donations))
print(num_donat + ggtitle("Number of Donations"))

total%>%
count('Number.of.Donations')


#create  categories: Number of Donations
attach(total)
total$num_donat[Number.of.Donations <= 2] <- "< 1-2"
total$num_donat[Number.of.Donations > 2 & Number.of.Donations <= 4] <- "2-4"
total$num_donat[Number.of.Donations > 4 & Number.of.Donations <= 6] <- "4-6"
total$num_donat[Number.of.Donations > 6 & Number.of.Donations <= 8] <- "6-8"
total$num_donat[Number.of.Donations> 8 & Number.of.Donations <= 10] <- "8-10"
total$num_donat[Number.of.Donations > 10 & Number.of.Donations <= 12] <- "10-12"
total$num_donat[Number.of.Donations > 12] <- "12+"
detach(total)
number_of_donations<-ggplot(data = total) + geom_bar(mapping = aes(x = num_donat))
print(number_of_donations + ggtitle("Number of Donations"))

#create  binary categories: donated only once vs. more than once
attach(total)
total$num_donat2[Number.of.Donations = 1] <- "Only 1"
total$num_donat2[Number.of.Donations  > 1] <- "More than 1"
detach(total)
number_of_donations2<-ggplot(data = total) + geom_bar(mapping = aes(x = num_donat2))
print(number_of_donations2 + ggtitle("Number of Donations"))

#create  categories: donated 1x,2-3X or 3+X
attach(total)
total$num_donat3[Number.of.Donations <= 1] <- "1"
total$num_donat3[Number.of.Donations > 1 & Number.of.Donations <= 2] <- "1-2"
total$num_donat3[Number.of.Donations  > 2 & Number.of.Donations <= 3] <- "2-3"
total$num_donat3[Number.of.Donations > 3] <- "3+"
detach(total)
number_of_donations3<-ggplot(data = total) + geom_bar(mapping = aes(x = num_donat3))
print(number_of_donations3 + ggtitle("Number of Donations"))


#####################################################################
########   Total.Volume.Donated..c.c..  #############################
#####################################################################

vol_donated<-ggplot(data = total) + geom_bar(mapping = aes(x = Total.Volume.Donated..c.c.. ))
print(vol_donated + ggtitle("Volume of Blood Donated (c.c.)"))

total%>%
count('Total.Volume.Donated..c.c..')

#create  volume categories in 1000 increments
attach(total)
total$cc[Total.Volume.Donated..c.c.. <= 1000] <- "< 1000"
total$cc[Total.Volume.Donated..c.c..  > 1000 & Total.Volume.Donated..c.c.. <= 2000] <- "1000-2000"
total$cc[Total.Volume.Donated..c.c..  > 2000 & Total.Volume.Donated..c.c.. <= 3000] <- "2000-3000"
total$cc[Total.Volume.Donated..c.c..  > 3000 & Total.Volume.Donated..c.c.. <= 4000] <- "3000-4000"
total$cc[Total.Volume.Donated..c.c..  > 4000 & Total.Volume.Donated..c.c.. <= 5000] <- "4000-5000"
total$cc[Total.Volume.Donated..c.c..  > 5000] <- "5000+"
detach(total)
donated_cc<-ggplot(data = total) + geom_bar(mapping = aes(x = cc))
print(donated_cc + ggtitle("Total Volume Donated"))


#create  categories: break down smaller volume witn 1000 cc
attach(total)
total$cc2[Total.Volume.Donated..c.c..  <= 250] <- "< 250"
total$cc2[Total.Volume.Donated..c.c..  > 250 & Total.Volume.Donated..c.c.. <= 500] <- "250-500"
total$cc2 [Total.Volume.Donated..c.c.. > 500 & Total.Volume.Donated..c.c.. <= 750] <- "500-750"
total$cc2[Total.Volume.Donated..c.c..  > 750 & Total.Volume.Donated..c.c.. <= 1000] <- "750-1000"
total$cc2[Total.Volume.Donated..c.c..  > 1000 & Total.Volume.Donated..c.c.. <= 1250] <- "1000-1250"
total$cc2[Total.Volume.Donated..c.c..  > 1250 & Total.Volume.Donated..c.c.. <= 1500] <- "1250-1500"
total$cc2[Total.Volume.Donated..c.c..  > 1500 & Total.Volume.Donated..c.c.. <= 1750] <- "1500-1750"
total$cc2[Total.Volume.Donated..c.c..  > 1750 & Total.Volume.Donated..c.c.. <= 2000] <- "1750-2000"
total$cc2[Total.Volume.Donated..c.c..  > 2000 & Total.Volume.Donated..c.c.. <= 3000] <- "2000-3000"
total$cc2[Total.Volume.Donated..c.c..  > 3000 & Total.Volume.Donated..c.c.. <= 4000] <- "3000-4000"
total$cc2[Total.Volume.Donated..c.c..  > 4000 & Total.Volume.Donated..c.c.. <= 5000] <- "4000-5000"
total$cc2[Total.Volume.Donated..c.c..  > 5000] <- "5000+"
detach(total)
donated_cc2<-ggplot(data = total) + geom_bar(mapping = aes(x = cc2))
print(donated_cc2 + ggtitle("Total Volume Donated"))


##################################################################
######   CREATE NEW VARIABLES FROM TWO VARIABLES: COMPOSITE ######   
##################################################################

################################################################################
#longest time active
#create new variables: active months
#zero means donor who just started in the system
total$active.months<-total$Months.since.First.Donation-total$Months.since.Last.Donation
active_months<-ggplot(data = total) + geom_bar(mapping = aes(x = active.months))
print(active_months + ggtitle("Active Months: Difference Between Most Recent and Earliest Donation"))


total%>%
count('active.months')

#active months in categories/buckets
attach(total)
total$active.cat2[active.months <= 1] <- "1"
total$active.cat2[active.months > 1 & active.months <=10] <-"2-10" 
total$active.cat2[active.months > 10 & active.months <=20] <-"10-20" 
total$active.cat2[active.months > 20 & active.months <=30] <-"20-30" 
total$active.cat2[active.months > 30 & active.months <=40] <-"30-40"  
total$active.cat2[active.months > 40 & active.months <=50] <-"40-50" 
total$active.cat2[active.months > 50 & active.months <=60] <-"50-60" 
total$active.cat2[active.months > 60 & active.months <=70] <-"60-70" 
total$active.cat2[active.months > 70] <- "70+"
detach(total)

active.cat2 <- ggplot(data = total) + geom_bar(mapping = aes(x = active.cat2))
print(active.cat2 + ggtitle("Active Months"))

#########################################################################################################
#Number of donations per month
total$donations_per_month<-total$Number.of.Donations /total$ Months.since.First.Donation
donate_per_month<-ggplot(data = total) + geom_bar(mapping = aes(x = donations_per_month))
print(donate_per_month + ggtitle("Average No. of Donations/Month"))

total%>%
count('donations_per_month')
summary(total$donations_per_month)

#create categories
attach(total)
total$Month_donate_cat[donations_per_month <= .1] <- ".1"
total$Month_donate_cat[donations_per_month > .1 & donations_per_month <=.2] <-".1-.2" 
total$Month_donate_cat[donations_per_month > .2 & donations_per_month <=.3] <-".2-.3" 
total$Month_donate_cat[donations_per_month > .3 & donations_per_month <=.4] <-".3-.4" 
total$Month_donate_cat[donations_per_month > .4 & donations_per_month <=.5] <-".4-.5"  
total$Month_donate_cat[donations_per_month > .5 & donations_per_month <=.6] <-".5-.6" 
total$Month_donate_cat[donations_per_month > .6 & donations_per_month <=.7] <-".6-.7" 
total$Month_donate_cat[donations_per_month > .7 & donations_per_month <=.8] <-".7-.8" 
total$Month_donate_cat[donations_per_month > .8 & donations_per_month <=.9] <-".8-.9" 
total$Month_donate_cat[donations_per_month > .9] <- ".9+"
detach(total)

donate_per_month<-ggplot(data = total) + geom_bar(mapping = aes(x = Month_donate_cat))
print(donate_per_month + ggtitle("Average No. of Donations/Month"))

######################################################################################
#recent donor by activity category

#subset data for those donated recently (e.g. donations were made in the most recent quarter)
#flag those who donated witin each activity category

attach(total)
total$since_last_cat2[Months.since.First.Donation <= 1 & Months.since.Last.Donation<=2] <- "1"
total$since_last_cat2[Months.since.First.Donation > 1 & Months.since.First.Donation <=10 & Months.since.Last.Donation<=2] <-"2-10" 
total$since_last_cat2[Months.since.First.Donation > 10 & Months.since.First.Donation <=20 & Months.since.Last.Donation<=2 ] <-"10-20" 
total$since_last_cat2[Months.since.First.Donation > 20 & Months.since.First.Donation <=30 & Months.since.Last.Donation<=2 ] <-"20-30" 
total$since_last_cat2[Months.since.First.Donation > 30 & Months.since.First.Donation <=40 & Months.since.Last.Donation<=2 ] <-"30-40"  
total$since_last_cat2[Months.since.First.Donation > 40 & Months.since.First.Donation <=50 & Months.since.Last.Donation<=2 ] <-"40-50" 
total$since_last_cat2[Months.since.First.Donation > 50 & Months.since.First.Donation <=60 & Months.since.Last.Donation<=2 ] <-"50-60" 
total$since_last_cat2[Months.since.First.Donation > 60 & Months.since.First.Donation <=70 & Months.since.Last.Donation<=2 ] <-"60-70" 
total$since_last_cat2[Months.since.First.Donation > 70 & Months.since.Last.Donation<=2 ] <- "70+"
total$since_last_cat2[Months.since.Last.Donation>2] <- "0" #0 are those who did not donate
detach(total)

recent_donor<-ggplot(data = total) + geom_bar(mapping = aes(x = since_last_cat2))
print(recent_donor + ggtitle("No. of Donors within Each 'Active' Category"))



##########################################################################################
##################### RELATONSHIP BETWEEN PREDICTORS AND TARGET ##########################
##########################################################################################

### Correlations ####
require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)

attach(total)
cor(Made.Donation.in.March.2007, Months.since.First.Donation, method = c("pearson", "kendall", "spearman"))
cor.test(Made.Donation.in.March.2007, Months.since.First.Donation, method=c("pearson", "kendall", "spearman"))

cor(Made.Donation.in.March.2007, Months.since.Last.Donation, method = c("pearson", "kendall", "spearman"))
cor.test(Made.Donation.in.March.2007, Months.since.Last.Donation, method=c("pearson", "kendall", "spearman"))

cor(Made.Donation.in.March.2007, Number.of.Donations, method = c("pearson", "kendall", "spearman"))
cor.test(Made.Donation.in.March.2007, Number.of.Donations, method=c("pearson", "kendall", "spearman"))

cor(Made.Donation.in.March.2007, Total.Volume.Donated..c.c.., method = c("pearson", "kendall", "spearman"))
cor.test(Made.Donation.in.March.2007, Total.Volume.Donated..c.c.., method=c("pearson", "kendall", "spearman"))

##### Run interesting plots #####

BloodDonorPlot1 <- ggplot(total, aes(x= Months.since.Last.Donation,Total.Volume.Donated..c.c.., colour=Made.Donation.in.March.2007)) + geom_point() 
print(BloodDonorPlot1 + ggtitle("Distribution of Donors") + labs(colour = "Made.Donation.in.March.2007"))  #add title and legend title

BloodDonorPlot2 <- ggplot(total, aes(x= Months.since.Last.Donation,Number.of.Donations, colour=Made.Donation.in.March.2007)) + geom_point() 
print(BloodDonorPlot2 + ggtitle("Distribution of Donors") + labs(colour = "Made.Donation.in.March.2007"))  #add title and legend title

BloodDonorPlot3 <- ggplot(total, aes(x= Months.since.First.Donation,Number.of.Donations, colour=Made.Donation.in.March.2007)) + geom_point() 
print(BloodDonorPlot3 + ggtitle("Distribution of Donors") + labs(colour = "Made.Donation.in.March.2007"))  #add title and legend title

BloodDonorPlot4 <- ggplot(total, aes(x= Months.since.First.Donation,Months.since.Last.Donation, colour=Made.Donation.in.March.2007)) + geom_point() 
print(BloodDonorPlot4 + ggtitle("Distribution of Donors") + labs(colour = "Made.Donation.in.March.2007"))  #add title and legend title



#histogram of all numeric data
total %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") + 
  geom_histogram()

# View relationship betweem first & last donation and March
total %>% 
  ggvis(~Months.since.Last.Donation, ~Months.since.First.Donation, fill = ~Made.Donation.in.March.2007 ) %>% 
  layer_points()

total %>% 
  ggvis(~Number.of.Donations, ~Months.since.First.Donation, fill = ~Made.Donation.in.March.2007 ) %>% 
  layer_points()

# Scatter PLot Matrix
pairs(~Made.Donation.in.March.2007 + 
        Number.of.Donations + 
        Months.since.First.Donation + 
        Total.Volume.Donated..c.c..+
        Months.since.Last.Donation, 
      data = total, main = 'Scatterplot Matrix' )

# Correlation Matrix + Scatter Plots
#total_df_originals <- total[,3:6]
#chart.Correlation(total_df_originals, histogram=TRUE, pch=19)  
total %>%
  keep(is.numeric) %>% 
  chart.Correlation(histogram=TRUE, pch=19)  #################################THIS DOES NOT RUN




#############################
#                           #
#      Outlier Detection    #
#                           # 
#############################

summary(total)
#last_don_num$Months.since.Last.Donation <- as.numeric(blood_df$Months.since.Last.Donation)



############################################
###       MONTHS SINCE LAST DONATION     ###
############################################


### DATA LOOKS TO BE TRIMODAL - NEED TO DO FURTHER ANALYSIS ON HOW TO HANDLE

# Calculate 1.5 IQR to be used in determining outliers
total$Last.Don.Mod = total$Months.since.Last.Donation

Q1 = quantile(total$Months.since.Last.Donation, probs =0.25)
Q3 = quantile(total$Months.since.Last.Donation, probs =0.75)
IQR = Q3 - Q1
avg_last_don = mean(total$Months.since.Last.Donation)
upper_limit_last_don = Q3 + 1.5 * IQR
lower_limit_last_don = Q1 - 1.5 * IQR

high_outliers <- sum(total$Months.since.Last.Donation > upper_limit_last_don)
low_outliers <- sum(total$Months.since.Last.Donation < lower_limit_last_don)
print(paste0("Number of High Outliers in Last Donation: ", high_outliers))
print(paste0("Number of Low Outliers in Last Donation: ", low_outliers))


# Create a duplicate of last donation column with imputed outliers
#total$Last.Don.Mod[total$Last.Don.Mod > upper_limit_last_don] <- avg_last_don
#total$Last.Don.Mod[total$Last.Don.Mod < lower_limit_last_don] <- avg_last_don
total$Last.Don.Mod[total$Last.Don.Mod > upper_limit_last_don] <- upper_limit_last_don
total$Last.Don.Mod[total$Last.Don.Mod < lower_limit_last_don] <- lower_limit_last_don

# Create a transformation column on Last Donation with outliers removed
total$Last.Don.bimod.trans <- abs(total$Months.since.Last.Donation - mean(total$Months.since.Last.Donation))
Q1_tran = quantile(total$Last.Don.bimod.trans, probs =0.25)
Q3_tran = quantile(total$Last.Don.bimod.trans, probs =0.75)
IQR_tran = Q3_tran - Q1_tran
avg_last_don_tran = mean(total$Last.Don.bimod.trans)
upper_limit_last_don_tran = Q3_tran + 1.5 * IQR_tran
lower_limit_last_don_tran = Q1_tran - 1.5 * IQR_tran

#total$Last.Don.bimod.trans[total$Last.Don.bimod.trans > upper_limit_last_don_tran] <- avg_last_don_tran
#total$Last.Don.bimod.trans[total$Last.Don.bimod.trans < lower_limit_last_don_tran] <- avg_last_don_tran
total$Last.Don.bimod.trans[total$Last.Don.bimod.trans > upper_limit_last_don_tran] <- upper_limit_last_don_tran
total$Last.Don.bimod.trans[total$Last.Don.bimod.trans < lower_limit_last_don_tran] <- lower_limit_last_don_tran

print(paste0("Number of High Outliers in Transformed Last Donation: ", high_outliers))
print(paste0("Number of Low Outliers in Transformed Last Donation: ", low_outliers))


# Plot All Last Donation Graphs

par(mfrow=c(3,4))
par(mar=c(1,1,5,1))

hist(total$Months.since.Last.Donation, breaks=20, col = rgb(0,0,1,0.5), main = 'Hist Last Donation', xlab = 'Months')
boxplot(total$Months.since.Last.Donation, col = rgb(0,0,1,0.5), main = "Boxplot of Last Donation")
shapiro.test(total$Months.since.Last.Donation)
qqnorm(total$Months.since.Last.Donation, main="Norm QQ Last Donation")
qqline(total$Months.since.Last.Donation, col = "red")
plot(density(total$Months.since.Last.Donation))

hist(total$Last.Don.Mod, breaks = 5, col = rgb(0,0,1,0.5), main = 'Last Donation No Outliers', xlab = 'Months')
boxplot(total$Last.Don.Mod, col = rgb(0,0,1,0.5), main = "Last Donation No Outlier")
shapiro.test(total$Last.Don.Mod)
qqnorm(total$Last.Don.Mod, main="Last Donation No Outliers")
qqline(total$Last.Don.Mod, col = "red")
plot(density(total$Last.Don.Mod))

hist(total$Last.Don.bimod.trans, breaks = 5, col = rgb(0,0,1,0.5), main = 'Last Donation No Outliers', xlab = 'Months')
boxplot(total$Last.Don.bimod.trans, col = rgb(0,0,1,0.5), main = "Last Donation No Outlier")
shapiro.test(total$Last.Don.bimod.trans)
qqnorm(total$Last.Don.bimod.trans, main="Last Donation No Outliers")
qqline(total$Last.Don.bimod.trans, col = "red")
plot(density(total$Last.Don.bimod.trans))

title("Months Since Last Donation Analysis", outer=TRUE, line = -1)



############################################
###      MONTHS SINCE FIRST DONATION     ###
############################################


# Calculate 1.5 IQR
total$First.Don.Mod = total$Months.since.First.Donation

Q1_First = quantile(total$Months.since.First.Donation, probs =0.25)
Q3_First = quantile(total$Months.since.First.Donation, probs =0.75)
IQR_First = Q3_First - Q1_First
avg_first_don = mean(total$Months.since.First.Donation)
upper_limit_first_don = Q3_First + 1.5 * IQR_First
lower_limit_first_don = Q1_First - 1.5 * IQR_First

# Print the number of outliers
high_outliers_First <- sum(total$Months.since.First.Donation > upper_limit_first_don)
low_outliers_First <- sum(total$Months.since.First.Donation < lower_limit_first_don)
print(paste0("Number of High Outliers in First Donation: ", high_outliers_First))
print(paste0("Number of Low Outliers in First Donation: ", low_outliers_First))


# Create a duplicate of first donation column with imputed outliers  
#total$First.Don.Mod[total$First.Don.Mod > upper_limit_first_don] <- avg_first_don
#total$First.Don.Mod[total$First.Don.Mod < lower_limit_first_don] <- avg_first_don
total$First.Don.Mod[total$First.Don.Mod > upper_limit_first_don] <- upper_limit_first_don
total$First.Don.Mod[total$First.Don.Mod < lower_limit_first_don] <- lower_limit_first_don

# Apply a transformation on the first donation column
total$First.Don.bimod.trans <- sqrt(total$Months.since.First.Donation)
Q1_first_tran = quantile(total$First.Don.bimod.trans, probs =0.25)
Q3_first_tran = quantile(total$First.Don.bimod.trans, probs =0.75)
IQR_first_tran = Q3_first_tran - Q1_first_tran
avg_first_don_tran = mean(total$First.Don.bimod.trans)
upper_limit_first_don_tran = Q3_tran + 1.5 * IQR_first_tran
lower_limit_first_don_tran = Q1_tran - 1.5 * IQR_first_tran

# Print the number of outliers
high_outliers_First_tran <- sum(total$First.Don.bimod.trans > upper_limit_first_don_tran)
low_outliers_First_tran <- sum(total$First.Don.bimod.trans < lower_limit_first_don_tran)
print(paste0("Number of High Outliers in Transformed First Donation: ", high_outliers_First_tran))
print(paste0("Number of Low Outliers in Transformed First Donation: ", low_outliers_First_tran))

# Impute the average to all outliers
#total$First.Don.bimod.trans[total$First.Don.bimod.trans > upper_limit_first_don_tran] <- avg_first_don_tran
#total$First.Don.bimod.trans[total$First.Don.bimod.trans < lower_limit_first_don_tran] <- avg_first_don_tran
total$First.Don.bimod.trans[total$First.Don.bimod.trans > upper_limit_first_don_tran] <- upper_limit_first_don_tran
total$First.Don.bimod.trans[total$First.Don.bimod.trans < lower_limit_first_don_tran] <- lower_limit_first_don_tran



# Plot All First Donation Graphs

par(mfrow=c(3,4))
par(mar=c(1,1,5,1))

hist(total$Months.since.First.Donation, breaks=20, col = rgb(0,0,1,0.5), main = 'Hist First Donation', xlab = 'Months')
boxplot(total$Months.since.First.Donation, col = rgb(0,0,1,0.5), main = "Boxplot of First Donation")
shapiro.test(total$Months.since.First.Donation)
qqnorm(total$Months.since.First.Donation, main="Norm QQ First Donation")
qqline(total$Months.since.First.Donation, col = "red")
plot(density(total$Months.since.First.Donation))

hist(total$First.Don.Mod, breaks = 5, col = rgb(0,0,1,0.5), main = 'First Donation No Outliers', xlab = 'Months') 
boxplot(total$First.Don.Mod, col = rgb(0,0,1,0.5), main = "First Donation No Outlier")
shapiro.test(total$First.Don.Mod)
qqnorm(total$First.Don.Mod, main="First Donation No Outliers")
qqline(total$First.Don.Mod, col = "red")
plot(density(total$First.Don.Mod))

hist(total$First.Don.bimod.trans, breaks = 5, col = rgb(0,0,1,0.5), main = 'First Donation Sqrt Trans', xlab = 'Months')
boxplot(total$First.Don.bimod.trans, col = rgb(0,0,1,0.5), main = "First Donation Sqrt Trans")
shapiro.test(total$First.Don.bimod.trans)
qqnorm(total$First.Don.bimod.trans, main="First Donation Sqrt Trans")
qqline(total$First.Don.bimod.trans, col = "red")
plot(density(total$First.Don.bimod.trans))

title("Months Since First Donation Analysis", outer=TRUE, line = -1)


############################################
###           NUMBER OF Donations        ###
############################################

# Calculate 1.5 IQR
total$Number.of.Donations.Mod = total$Number.of.Donations

Q1 = quantile(total$Number.of.Donations, probs =0.25)
Q3 = quantile(total$Number.of.Donations, probs =0.75)
IQR = Q3 - Q1
avg_num_don = mean(total$Number.of.Donations)
upper_limit_num_don = Q3 + 1.5 * IQR
lower_limit_num_don = Q1 - 1.5 * IQR


high_outliers_num <- sum(total$Number.of.Donations > upper_limit_num_don)
low_outliers_num <- sum(total$Number.of.Donations < lower_limit_num_don)
print(paste0("Number of High Outliers in Number of Donations: ", high_outliers_num))
print(paste0("Number of Low Outliers in Number of Donations: ", low_outliers_num))


#total$Number.of.Donations.Mod[total$Number.of.Donations.Mod > upper_limit_num_don] <- avg_num_don
#total$Number.of.Donations.Mod[total$Number.of.Donations.Mod < lower_limit_num_don] <- avg_num_don
total$Number.of.Donations.Mod[total$Number.of.Donations.Mod > upper_limit_num_don] <- upper_limit_num_don
total$Number.of.Donations.Mod[total$Number.of.Donations.Mod < lower_limit_num_don] <- lower_limit_num_don



# Transformation on the number of donations variable
total$Num.Don.trans <- log(total$Number.of.Donations)
Q1_num_tran = quantile(total$Num.Don.trans, probs =0.25)
Q3_num_tran = quantile(total$Num.Don.trans, probs =0.75)
IQR_num_tran = Q3_num_tran - Q1_num_tran
avg_num_don_tran = mean(total$Num.Don.trans)
upper_limit_num_don_tran = Q3_num_tran + 1.5 * IQR_num_tran
lower_limit_num_don_tran = Q1_num_tran - 1.5 * IQR_num_tran

#total$Num.Don.trans[total$Num.Don.trans > upper_limit_num_don_tran] <- avg_num_don_tran
#total$Num.Don.trans[total$Num.Don.trans < lower_limit_num_don_tran] <- avg_num_don_tran
total$Num.Don.trans[total$Num.Don.trans > upper_limit_num_don_tran] <- upper_limit_num_don_tran
total$Num.Don.trans[total$Num.Don.trans < lower_limit_num_don_tran] <- lower_limit_num_don_tran

high_outliers_num <- sum(total$Num.Don.trans > upper_limit_num_don_tran)
low_outliers_num <- sum(total$Num.Don.trans < lower_limit_num_don_tran)
print(paste0("Number of High Outliers in Transformed Number of Donations: ", high_outliers_num))
print(paste0("Number of Low Outliers in Transformed Number of Donations: ", low_outliers_num))



# Plot All Number of Donation graphs

par(mfrow=c(3,4))
par(mar=c(1,1,5,1))


hist(total$Num.Don.trans, breaks=20, col = rgb(0,0,1,0.5), main = 'Hist Number Donations', xlab = 'Months')
boxplot(total$Num.Don.trans, col = rgb(0,0,1,0.5), main = "Boxplot of Number Donations")
shapiro.test(total$Num.Don.trans)
qqnorm(total$Num.Don.trans, main="Norm QQ Number Donations")
qqline(total$Num.Don.trans, col = "red")
plot(density(total$Num.Don.trans))

hist(total$Number.of.Donations.Mod , breaks = 5, col = rgb(0,0,1,0.5), main = 'Number Donations No Outliers', xlab = 'Months')
boxplot(total$Number.of.Donations.Mod , col = rgb(0,0,1,0.5), main = "Number Donations No Outlier")
shapiro.test(total$Number.of.Donations.Mod )
qqnorm(total$Number.of.Donations.Mod , main="Number Donations No Outliers")
qqline(total$Number.of.Donations.Mod , col = "red")
plot(density(total$Number.of.Donations.Mod ))

hist(total$Num.Don.trans, breaks = 5, col = rgb(0,0,1,0.5), main = 'Number Donations Log Trans', xlab = 'Months')
boxplot(total$Num.Don.trans, col = rgb(0,0,1,0.5), main = "Number Donations Log Trans")
shapiro.test(total$Num.Don.trans)
qqnorm(total$Num.Don.trans, main="Number Donations Log Trans")
qqline(total$Num.Don.trans, col = "red")
plot(density(total$Num.Don.trans))

title("Number of Donations Analysis", outer=TRUE, line = -1)



############################################
###           VOLUMN OF Donations        ###
############################################

# Calculate 1.5 IQR
total$Tot_Vol_Mod = total$Total.Volume.Donated..c.c..

Q1 = quantile(total$Total.Volume.Donated..c.c.., probs =0.25)
Q3 = quantile(total$Total.Volume.Donated..c.c.., probs =0.75)
IQR = Q3 - Q1
avg_vol_don = median(total$Total.Volume.Donated..c.c..)
upper_limit_vol_don = Q3 + 1.5 * IQR
lower_limit_vol_don = Q1 - 1.5 * IQR


high_outliers_vol <- sum(total$Total.Volume.Donated..c.c.. > upper_limit_vol_don)
low_outliers_vol <- sum(total$Total.Volume.Donated..c.c.. < lower_limit_vol_don)
print(paste0('Number of High Outliers in Volumn of Donation: ' , high_outliers_vol))
print(paste0('Number of Low Outliers in Volumn of Donation: ' , low_outliers_vol))


#total$Tot_Vol_Mod[total$Tot_Vol_Mod > upper_limit_vol_don] <- avg_vol_don
#total$Tot_Vol_Mod[total$Tot_Vol_Mod < lower_limit_vol_don] <- avg_vol_don
total$Tot_Vol_Mod[total$Tot_Vol_Mod > upper_limit_vol_don] <- upper_limit_vol_don
total$Tot_Vol_Mod[total$Tot_Vol_Mod < lower_limit_vol_don] <- lower_limit_vol_don

total$Vol.Don.trans <- sqrt(total$Tot_Vol_Mod)
Q1_vol_tran = quantile(total$Vol.Don.trans, probs =0.25)
Q3_vol_tran = quantile(total$Vol.Don.trans, probs =0.75)
IQR_vol_tran = Q3_vol_tran - Q1_vol_tran
avg_vol_don_tran = mean(total$Vol.Don.trans)

upper_limit_vol_don_tran = Q3_vol_tran + 1.5 * IQR_vol_tran
lower_limit_vol_don_tran = Q1_vol_tran - 1.5 * IQR_vol_tran

#total$Vol.Don.trans[total$Vol.Don.trans > upper_limit_vol_don_tran] <- avg_vol_don_tran
#total$Vol.Don.trans[total$Vol.Don.trans < lower_limit_vol_don_tran] <- avg_vol_don_tran
total$Vol.Don.trans[total$Vol.Don.trans > upper_limit_vol_don_tran] <- upper_limit_vol_don_tran
total$Vol.Don.trans[total$Vol.Don.trans < lower_limit_vol_don_tran] <- lower_limit_vol_don_tran

high_outliers_vol <- sum(total$Vol.Don.trans > upper_limit_vol_don_tran)
low_outliers_vol <- sum(total$Vol.Don.trans < lower_limit_vol_don_tran)
print(paste0('Number of High Outliers in Transformed Volumn of Donation: ' , high_outliers_vol))
print(paste0('Number of Low Outliers in Transformed Volumn of Donation: ' , low_outliers_vol))



# Plot All Volumn of Donation graphs

par(mfrow=c(3,4))
par(mar=c(1,1,5,1))

hist(total$Total.Volume.Donated..c.c.., breaks=20, col = rgb(0,0,1,0.5), main = 'Hist Volumn Donations', xlab = 'Months')
boxplot(total$Total.Volume.Donated..c.c.., col = rgb(0,0,1,0.5), main = "Boxplot of Volumn Donations")
shapiro.test(total$Total.Volume.Donated..c.c..)
qqnorm(total$Total.Volume.Donated..c.c.., main="Norm QQ Volumn Donations")
qqline(total$Total.Volume.Donated..c.c.., col = "red")
plot(density(total$Total.Volume.Donated..c.c..))

hist(total$Tot_Vol_Mod , breaks = 5, col = rgb(0,0,1,0.5), main = 'Volumn Donations No Outliers', xlab = 'Months')
boxplot(total$Tot_Vol_Mod , col = rgb(0,0,1,0.5), main = "Volumn Donations No Outlier")
shapiro.test(total$Tot_Vol_Mod )
qqnorm(total$Tot_Vol_Mod , main="Volumn Donations No Outliers")
qqline(total$Tot_Vol_Mod , col = "red")
plot(density(total$Tot_Vol_Mod ))

hist(total$Vol.Don.trans, breaks = 5, col = rgb(0,0,1,0.5), main = 'Volumn Donations Log Trans', xlab = 'Months')
boxplot(total$Vol.Don.trans, col = rgb(0,0,1,0.5), main = "Volumn Donations Log Trans")
shapiro.test(total$Vol.Don.trans)
qqnorm(total$Vol.Don.trans, main="Volumn Donations Log Trans")
qqline(total$Vol.Don.trans, col = "red")
plot(density(total$Vol.Don.trans))

title("Volumn of Donations Analysis", outer=TRUE, line = -1)

# Create Ratio Variable
total <-mutate(total, ratio = sqrt(Months.since.Last.Donation/Months.since.First.Donation))
Q1_ratio = quantile(total$ratio, probs =0.25)
Q3_ratio = quantile(total$ratio, probs =0.75)
IQR_ratio = Q3_ratio - Q1_ratio
upper_limit_ratio = Q3_ratio + 1.5 * IQR_ratio
lower_limit_ratio = Q1_ratio - 1.5 * IQR_ratio
total$ratio[total$ratio > upper_limit_ratio] <- upper_limit_ratio
total$ratio[total$ratio < lower_limit_ratio] <- lower_limit_ratio


############### ############### ############### ############
############### Set up data for analysis ###### ############
############### ############### ############### ############

attach(total)
data.train1 <- total[total$designation=="training",]
data.validation <- total[total$designation=="test",] #this is the original 200 obeservations called test dataset for prediction

#partition the traiing data into true training and test datasets
smp_size<-floor(0.8*nrow(data.train1)) #80% will be training and 20% will be test

set.seed(123)
train_ind<-sample(seq_len(nrow(data.train1)),size=smp_size)

data.train<-data.train1[train_ind,] #this is the dataset to train the model on
data.test<-data.train1[-train_ind,] #this is the dataset to check model accuracy on 

count('data.train')
#### Standardize the following variables: 

#Last.Don.bimod.trans
#First.Don.bimod.trans
#Num.Don.trans
#Vol.Don.trans
#active.months
#donations_per_month



#means:
m1<-mean(First.Don.bimod.trans)
m2<-mean(Last.Don.bimod.trans)
m3<-mean(Num.Don.trans)
m4<-mean(Vol.Don.trans)
m5<-mean(active.months)
m6<-mean(donations_per_month)

#standard deviations
sd1<-sd(First.Don.bimod.trans)
sd2<-sd(Last.Don.bimod.trans)
sd3<-sd(Num.Don.trans)
sd4<-sd(Vol.Don.trans)
sd5<-sd(active.months)
sd6<-sd(donations_per_month)

#standardize train data (use transformed data):
data.train$stand.Months.since.First.Donation <- t(t(data.train$First.Don.bimod.trans-m1)/sd1)
data.train$stand.Months.since.Last.Donation <- t(t(data.train$Last.Don.bimod.trans-m2)/sd2)
data.train$stand.Number.of.Donations <- t(t(data.train$Num.Don.trans-m3)/sd3)
data.train$stand.Total.Volume.Donated..c.c.. <- t(t(data.train$Vol.Don.trans-m4)/sd4)
data.train$stand.active.months <- t(t(data.train$active.months-m5)/sd5)
data.train$stand.donations_per_month <- t(t(data.train$donations_per_month-m6)/sd6)

#standardize test data (using training mean and sd):
data.test$stand.Months.since.First.Donation <- t(t(data.test$First.Don.bimod.trans-m1)/sd1)
data.test$stand.Months.since.Last.Donation <- t(t(data.test$Last.Don.bimod.trans-m2)/sd2)
data.test$stand.Number.of.Donations <- t(t(data.test$Num.Don.trans-m3)/sd3)
data.test$stand.Total.Volume.Donated..c.c.. <- t(t(data.test$Vol.Don.trans-m4)/sd4)
data.test$stand.active.months <- t(t(data.test$active.months-m5)/sd5)
data.test$stand.donations_per_month <- t(t(data.test$donations_per_month-m6)/sd6)

#standardize vaidation data (using training mean and sd):
data.validation$stand.Months.since.First.Donation <- t(t(data.validation$First.Don.bimod.trans-m1)/sd1)
data.validation$stand.Months.since.Last.Donation <- t(t(data.validation$Last.Don.bimod.trans-m2)/sd2)
data.validation$stand.Number.of.Donations <- t(t(data.validation$Number.of.Donations-m3)/sd3)
data.validation$stand.Total.Volume.Donated..c.c.. <- t(t(data.validation$Vol.Don.trans-m4)/sd4)
data.validation$stand.active.months <- t(t(data.validation$active.months-m5)/sd5)
data.validation$stand.donations_per_month <- t(t(data.validation$donations_per_month-m6)/sd6)

#Final datsets for model creation
#drop original unstandardized variable
names(data.train)
data.train.sd <- data.train[c(-1,-2,-3,-4,-5,-6,-8,-19, -21, -24,-25,-26,-27,-28,-29,-30,-31)]
data.test.sd <- data.train[c(-1,-2,-3,-4,-5,-6,-8,-19, -21, -24,-25,-26,-27,-28,-29,-30,-31)]
data.validation.sd <- data.train[c(-1,-2,-3,-4,-5,-6,-8,-19, -21, -24,-25,-26,-27,-28,-29,-30,-31)]

####################################################################################################################################
##### BUILD MODELS
####################################################################################################################################

set.seed(1234)


blood.training <- data.train.sd[which(names(data.train.sd) %in% c("Made.Donation.in.March.2007"
                                                                  ,"Last.Don.bimod.trans"
                                                                  ,"First.Don.bimod.trans"
                                                                  ,"Num.Don.trans"
                                                                  ,"Vol.Don.trans"
                                                                  ,"stand.Months.since.First.Donation"
                                                                  ,"stand.Months.since.Last.Donation"
                                                                  ,"stand.Number.of.Donations"
                                                                  ,"stand.Total.Volume.Donated..c.c.."
                                                                  ,"stand.active.months"
                                                                  ,"stand.donations_per_month"
                                                                  ,'ratio'
)
)] # Train Set Predictors

blood.test <- data.test.sd[which(names(data.train.sd) %in% c("Made.Donation.in.March.2007"
                                                             ,"Last.Don.bimod.trans"
                                                             ,"First.Don.bimod.trans"
                                                             ,"Num.Don.trans"
                                                             ,"Vol.Don.trans"
                                                             ,"stand.Months.since.First.Donation"
                                                             ,"stand.Months.since.Last.Donation"
                                                             ,"stand.Number.of.Donations"
                                                             ,"stand.Total.Volume.Donated..c.c.."
                                                             ,"stand.active.months"
                                                             ,"stand.donations_per_month"
                                                             ,'ratio'
)
)] # Test Set Predictors



blood.trainlabels <- data.train.sd[1] # Train Set Outcome
blood.testlabels <- data.test.sd[1] # Test Set Outcome



#############################
#                           #
#   CARET PACKAGE ML        #
#                           # 
#############################

require(caret)

blood.training$Made.Donation.in.March.2007 = as.factor(blood.training$Made.Donation.in.March.2007)
blood.test$Made.Donation.in.March.2007 = as.factor(blood.test$Made.Donation.in.March.2007)

outcomeName<-'Made.Donation.in.March.2007'
predictors<-names(blood.training)[!names(blood.training) %in% outcomeName]

###########################
# Testing for best parameters
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
Blood_Pred_Profile <- rfe(blood.training[,predictors], blood.training[,outcomeName],
                          rfeControl = control)
Blood_Pred_Profile



predictors = c('stand.Months.since.Last.Donation'
               , 'stand.donations_per_month'
               , 'stand.Months.since.First.Donation'
               , 'ratio'
               )

require(fastAdaboost)
require(elmNN)
require(deepnet)
require(bst)
require(earth)

fitControl <- trainControl(
  method = "cv"
  ,number = 5
  #,repeats = 5
  ,classProbs=TRUE
  , summaryFunction=mnLogLoss)

levels(blood.training$Made.Donation.in.March.2007) <- make.names(levels(factor(blood.training$Made.Donation.in.March.2007)))

model_gbm<-train(blood.training[,predictors],blood.training[,outcomeName],method='gbm',metric="logLoss", trControl=fitControl,preProcess = c("center", "scale"))
model_rf<-train(blood.training[,predictors],blood.training[,outcomeName],method='rf',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale"))
model_nnet<-train(blood.training[,predictors],blood.training[,outcomeName],method='nnet',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale"))
model_glm<-train(blood.training[,predictors],blood.training[,outcomeName],method='glm',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale"))
model_knn<-train(blood.training[,predictors],blood.training[,outcomeName],method='knn',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale"))
model_dnn<-train(blood.training[,predictors],blood.training[,outcomeName],method='dnn',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale"))
model_gam<-train(blood.training[,predictors],blood.training[,outcomeName],method='gam',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale")) # Generalized Additive Model using Splines
model_earth<-train(blood.training[,predictors],blood.training[,outcomeName],method='earth',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale")) # Multivariate Adaptive Regression Spline
model_lda<-train(blood.training[,predictors],blood.training[,outcomeName],method='lda',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale")) # LDA linear Discriminant Analysis
model_qda<-train(blood.training[,predictors],blood.training[,outcomeName],method='qda',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale")) # QDA QUADRATIC DISCRIMINANT ANALYSIS 
model_logitboost<-train(blood.training[,predictors],blood.training[,outcomeName],method='LogitBoost',metric="logLoss",trControl=fitControl,preProcess = c("center", "scale")) # Boosted Logistic Regression 
model_avNNet<-train(blood.training[,predictors],blood.training[,outcomeName],method='avNNet',metric="logLoss",trControl=fitControl, preProcess = c("center", "scale"))
model_nb<-train(blood.training[,predictors],blood.training[,outcomeName],method='nb',metric="logLoss",trControl=fitControl, preProcess = c("center", "scale"))

model_gbm$finalModel
model_rf$finalModel
model_nnet$finalModel
model_glm$finalModel
model_knn$finalModel
model_dnn$finalModel
model_gam$finalModel
model_earth$finalModel
model_lda$finalModel
model_qda$finalModel
model_logitboost$finalModel
model_avNNET$finalModel
model_nb$finalModel






#model_polr<-train(blood.training[,predictors],blood.training[,outcomeName],method='polr',metric="logLoss",trControl=fitControl) #Polynomial Regression 
#model_elm<-train(blood.training[,predictors],blood.training[,outcomeName],method='elm',metric="logLoss",trControl=fitControl)
#model_gaussprLinear<-train(blood.training[,predictors],blood.training[,outcomeName],method='gaussprLinear',metric="logLoss",trControl=fitControl) # Gaussian Process Linear
#model_bstSm<-train(blood.training[,predictors],blood.training[,outcomeName],method='bstSm',metric="logLoss",trControl=fitControl) # Boosted Smoothing Spline
#model_bam<-train(blood.training[,predictors],blood.training[,outcomeName],method='bam',trControl=fitControl) # Generalized Additive Model using Splines


results <- resamples(list(RF=model_rf
                          , GBM=model_gbm
                          , NN=model_nnet
                          , GLM =model_glm
                          , KNN = model_knn
                          , DNN = model_dnn
                          , GAM = model_gam
                          , EAR = model_earth
                          , LDA =model_lda
                          , QDA =model_qda
                          , BGLM = model_bayesglm
                          , BLR = model_logitboost
                          , AVNNET = model_avNNet
                          #, ELM = model_elm
                          #, GSL = model_gaussprLinear
                          #, ADA = model_adaboost
                          #, BAM = model_bam
                          #, BSS = model_bstSm
                          #, POLR = model_polr  
                          , NB = model_nb
))


######################
#  RESULTS           #
######################

summary(results)
bwplot(results)
dotplot(results)



#PREPARE OUTPUT FOR COMPETITION

test_pred <- predict(model_avNNet, newdata=data.validation, type="prob")

### TRYING DIFFERENT THINGS
# Trial of ENSEMBLE METHOD
test_pred1 <- predict(model_gam, newdata=data.validation, type="prob")
test_pred2 <- predict(model_avNNet, newdata=data.validation, type="prob")
ensemble <- data.frame(test_pred1, test_pred2)
ensemble <- ensemble[c(2,4)]
ensemble$mean <- (ensemble$X1 + ensemble$X1.1)/2
# Defaulting to 0 if <5 donations and more than 24 months since last donation
#merged_df$edit <- ifelse(data.validation$Months.since.Last.Donation>24 & data.validation$Number.of.Donations<5,0,'')
#merged_df$x1 <- ifelse(merged_df$edit == 0,0,merged_df[,3])
#head(merged_df,100)


# OUTPUT FINAL RESULT
bloodTestLabels <- data.frame(test_pred)
merged_df <- data.frame(data.validation$X, bloodTestLabels)
merged_df <- merged_df[,c(1,3)]
names(merged_df) <- c("","Made Donation in March 2007")
#output
write.csv(format(merged_df, nsmall=2), file = "NW Data Science Classifiers AVNNET.csv",row.names=FALSE)


#############################################################
###### TRY DIFFERENT METHODS ################################


require(caret)

blood.training$Made.Donation.in.March.2007 = as.factor(blood.training$Made.Donation.in.March.2007)
blood.test$Made.Donation.in.March.2007 = as.factor(blood.test$Made.Donation.in.March.2007)

# set Control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(1234)

knn_fit <- train(Made.Donation.in.March.2007 ~., data = blood.training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

print(knn_fit)
plot(knn_fit)

test_pred <- predict(knn_fit, newdata=blood.test)
confusionMatrix(test_pred, blood.test$Made.Donation.in.March.2007)


bloodTestLabels <- data.frame(test_pred)
merged_df <- data.frame(blood.test$Made.Donation.in.March.2007, blood.testlabels)
names(merged_df) <- c("Predicted Donations","Observed Donations")

CrossTable(x = blood.testlabels$Made.Donation.in.March.2007, y=test_pred, prop.chisq=TRUE)


################################
# LINEAR DISCRIMINANT ANALYSIS #

library(MASS)
model.lda1 <- lda(Made.Donation.in.March.2007 ~ stand.Months.since.First.Donation + stand.Months.since.Last.Donation + 
                    stand.Number.of.Donations + active.cat2 + Month_donate_cat + num_donat + last_don_cat1,
                  data=data.train.sd) 
model.lda1
lda.predicted<- predict(model.lda1, data.test.sd) 
names(lda.predicted)

lda.class=lda.predicted$class
table(lda.class, data.train.sd$Made.Donation.in.March.2007)

sum(lda.predicted$posterior[,1]>=.5)
sum(lda.predicted$posterior[,1]<.5)

mean(lda.class==data.train.sd$Made.Donation.in.March.2007)
#The accuracy is 83%

####################################
# QUADRATIC DISCRIMINANT ANALYSIS ##

model.qda1 <- qda(Made.Donation.in.March.2007 ~ stand.Months.since.First.Donation + stand.Months.since.Last.Donation + stand.Number.of.Donations,
                  data=data.train.sd) 
model.qda1
qda.predicted<- predict(model.qda1, data.test.sd)$class
table(qda.predicted, data.train.sd$Made.Donation.in.March.2007)
mean(qda.predicted==data.train.sd$Made.Donation.in.March.2007)
#The accuracy is 82%



#################################################
### Non-linear models: Polynomial Regression  ###
#################################################

library(ISLR)
# In performing polynomial regression, a decision needs to be made on the degree of the polynomial:  use hypothesis tests.

# Fit models ranging from linear to a 4th degree polynomial for each variable and find the most parsomonius model 
# Use ANOVA (F-test) to test Ho vs. Ha
# Ho = Model M1 is sufficient to explain the data
# Ha = Model M2 (more complex model) is sufficient
fit.1 <- lm(Made.Donation.in.March.2007 ~ stand.Months.since.Last.Donation, data = data.train.sd)
fit.2 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.Last.Donation, 2, raw=TRUE), data = data.train.sd)
fit.3 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.Last.Donation, 3, raw=TRUE), data = data.train.sd)
fit.4 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.Last.Donation, 4, raw=TRUE), data = data.train.sd)
anova(fit.1, fit.2, fit.3, fit.4)
#third degree polynomial is best

fit.1 <- lm(Made.Donation.in.March.2007 ~ stand.Months.since.First.Donation, data = data.train.sd)
fit.2 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.First.Donation, 2, raw=TRUE), data = data.train.sd)
fit.3 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.First.Donation, 3, raw=TRUE), data = data.train.sd)
fit.4 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Months.since.First.Donation, 4, raw=TRUE), data = data.train.sd)
anova(fit.1, fit.2, fit.3, fit.4)
#use linear

fit.1 <- lm(Made.Donation.in.March.2007 ~ stand.Number.of.Donations, data = data.train.sd)
fit.2 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Number.of.Donations, 2, raw=TRUE), data = data.train.sd)
fit.3 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Number.of.Donations, 3, raw=TRUE), data = data.train.sd)
fit.4 <- lm(Made.Donation.in.March.2007 ~ poly(stand.Number.of.Donations, 4, raw=TRUE), data = data.train.sd)
anova(fit.1, fit.2, fit.3, fit.4)
#use linear


#fit model
#must drop polynonomal to 2nd degree (3rd degree not significant)
lm.fit=lm(Made.Donation.in.March.2007 ~ I(stand.Months.since.Last.Donation^2)+stand.Months.since.First.Donation + stand.Number.of.Donations 
          
          , data = data.train.sd)
summary(lm.fit)

library(car)
vif(lm.fit)
plot(fitted(lm.fit),residuals(lm.fit))

preds=predict(lm.fit, newdata=data.test.sd, type="response",se=T)
names(preds)

lm.class=preds$fit
table(I(lm.class>0.4), data.test.sd$Made.Donation.in.March.2007)

mean(I(lm.class>0.4)==data.test.sd$Made.Donation.in.March.2007)
#The accuracy is 80%

######################################
### Generalized Additive Models (GAMs)
library(gam)


# use natural spline
fit.natural_spline<-lm(Made.Donation.in.March.2007 ~ ns(stand.Months.since.Last.Donation, 5)+ 
                         ns(stand.Months.since.First.Donation, 3) + ns(stand.Number.of.Donations, 5) + first_don_cat3 + num_donat , data=data.train.sd)
summary(fit.natural_spline)

preds1 <- predict(fit.natural_spline, newdata = data.test.sd)

table(I(preds1>0.4), data.test.sd$Made.Donation.in.March.2007)

mean(I(preds1>0.4)==data.test.sd$Made.Donation.in.March.2007)
#The accuracy is 83%









