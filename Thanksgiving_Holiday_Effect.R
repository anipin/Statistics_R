#################################################################################################
# Project           : SIEO W 4150 Final Project Report                          
# Author            : Anita Pinto < UNI : AP3650 >                              
# Date              : Jan 15 2018                                               
# Purpose           : Analysis of Holiday(Thanksgiving) effect on Retail Stocks 
#                   : This code performs the following Tests
#                   : 1. Test for Randomness ( runs test)
#                   : 2. Plot Histograms for Log-Returns 
#                   : 3. Plot Q-Q Normality Plots
#                   : 4. Calculate Mean, Variance and respective 95% Confidence Interval
#                   : 5. Linear Regression (lm) Log-Returns vs Time, with plots for residuals 
#                   : 6. Test (t.test) comparing Pre-Holiday Mean vs Post-Holiday Mean 
#                   : 7. Linear Regression betwen Stock 1 Log-Return vs Stock 2 Log Return
#                   : 8. Test(t.test) comparing Stock 1 Log-Return vs Stock 2 Log Return
##################################################################################################

# All print statements are captured into AP3650_report.txt in the current working directory


#install.packages("quantmod")
#install.packages("dplyr")
#install.packages("forecast")
#install.packages("randtests")

#library(quantmod)
#library(dplyr)
#library(forecast)
#library(ggplot2)
#library(randtests)

packages = c("quantmod","dplyr", "randtests")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
# ( source : http://www.vikram-baliga.com)

package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})

stock_list = c("KSS","WMT","M","TGT","AMZN","JCP")

## Adding this to remove warnings, comment to check the warnings if required
## These warnings are for getSymbols5.0
options("getSymbols.yahoo.warning"=FALSE)
options("getSymbols.warning4.0"=FALSE)

print("Check AP3650_Report.txt for Report and AP3650_Plots.pdf for plots")

sink("AP3650_Report.txt")

print("#################################################################################")
print("# Project           : SIEO W 4150 Final Project Report                           ")
print("# Purpose           : Analysis of Holiday(Thanksgiving) effect on Retail Stocks  ")
print("#################################################################################")

######################################################################################################
# get_stock_df() function gets the stock symbol and returns a dataframe for the stock from 1998 to 2017
# The function also adds Day, Month, Log Return Data fields to the data frame
######################################################################################################
get_stock_df <- function(stock_name) {
    
    #<debug> print(paste("Getting Stock Data for ",stock_name))
    stock_df =  as.data.frame(getSymbols(stock_name, src="yahoo", from = "1998-01-01", to = "2017-12-31", env = NULL, warnings=FALSE ))
    
    # Using Row Names to get Date Field
    stock_df$Date <- row.names(stock_df)
    # Type casting Date Field value into R Date Datatype
    stock_df$Date <- as.Date(stock_df$Date, format = "%Y-%m-%d")
    
    #Add Colums for Day/Month Field of Date
    stock_df$Day <- weekdays(stock_df$Date)
    stock_df$Month <- months(stock_df$Date)
    
    #Re-name Open and Close Column Names
    colnames(stock_df)[1] <-"Open"
    colnames(stock_df)[4] <-"Close"
    
    stock_df$PrevClose <- lag(stock_df$Close)
    stock_df <- stock_df[-1,]
    stock_df$LogRet <- log(stock_df$Close/stock_df$PrevClose)
    
    #print(head(stock_df))  # DBG MSG
    return(stock_df)
}

######################################################################################################
# Calling get_stock_df() fn  to create stock dataframe for corresponding stock symbol
######################################################################################################
kss_df <- get_stock_df("KSS") # KOHL
tgt_df <- get_stock_df("TGT") # TARGET
jcp_df <- get_stock_df("JCP") # JCP
wmt_df <- get_stock_df("WMT") # WALMART
macy_df <- get_stock_df("M")  # MACY
amzn_df <- get_stock_df("AMZN") # AMAZON


print("-----------------------------------------------------------------------")
print("                 Runs Test for checking Randomness                     ")
print("-----------------------------------------------------------------------")

######################################################################################################
# check_random_sample() fn takes in the stock data frame and performs a runs test on the stock Log Returns
# The fn returns the p value of the runs test
# p-value > 0.05, fails to reject Null Hypothesis i.e it is a random sample
# p-value < 0.05, rejects Null Hypothesis, non-random sample
######################################################################################################


check_random_sample <- function (stock_name, stock_df) {
    runs_p_val <- runs.test(stock_df$LogRet)$p.value
    print(paste("Runs Test p_value for", stock_name, ":", runs_p_val))
    if (runs_p_val > 0.05) {
        print(paste("P_value > 0.05, Log Return of", stock_name,"consistent with Random Sample"))
    } else {
        print(paste("P_value <= 0.05, Log Return of", stock_name," not consistent with Random Sample"))
        
    }
}


######################################################################################################
# Calling and Printing the P value for each Stock Data Frame to check Randomness
######################################################################################################
check_random_sample("Kohls", kss_df)
check_random_sample("Target", tgt_df)
check_random_sample("JCP",jcp_df)
check_random_sample("Walmart",wmt_df)
check_random_sample("Macys",macy_df)
check_random_sample("Amazon", amzn_df)

pdf("AP3650_Plots.pdf")

######################################################################################################
# Calling hist fn to build Histograms for stock log returns
######################################################################################################

hist(kss_df$LogRet,main=" Histogram of Kohls Log Return",col="darkgreen",xlab="KSS Log Return")
hist(tgt_df$LogRet,main=" Histogram of TARGET Log Return",col="darkgreen",xlab="TGT Log Return")
hist(jcp_df$LogRet,main=" Histogram of JCP Log Return",col="darkgreen",xlab="JCP Log Return")
hist(wmt_df$LogRet,main=" Histogram of WALMART Log Return",col="darkgreen",xlab="WMT Log Return")
hist(macy_df$LogRet,main=" Histogram of Macys Log Return",col="darkgreen",xlab="M Log Return")
hist(amzn_df$LogRet,main=" Histogram of AMAZON Log Return",col="darkgreen",xlab="AMZN Log Return")

######################################################################################################
#  QQNORM function to draw Normal Probability Plots for each of the stocks
######################################################################################################
qqnorm(kss_df$LogRet,main = "Normal Q-Q Plot for Kohls LogReturn")
qqline(kss_df$LogRet)
qqnorm(tgt_df$LogRet,main = "Normal Q-Q Plot for TARGET LogReturn")
qqline(tgt_df$LogRet)
qqnorm(jcp_df$LogRet,main = "Normal Q-Q Plot for JCP LogReturn")
qqline(jcp_df$LogRet)
qqnorm(wmt_df$LogRet,main = "Normal Q-Q Plot for WALMART LogReturn")
qqline(wmt_df$LogRet)
qqnorm(macy_df$LogRet,main = "Normal Q-Q Plot for Macys LogReturn")
qqline(macy_df$LogRet)
qqnorm(amzn_df$LogRet,main = "Normal Q-Q Plot for AMAZON LogReturn")
qqline(amzn_df$LogRet)


print("-----------------------------------------------------------------------")
print("                 Mean of  Each Stocks' Log Returns                           ")
print("-----------------------------------------------------------------------")

######################################################################################################
# Calculatig Mean of LogRet of each of the Stock Log returns
######################################################################################################
kss_mean <- mean(kss_df$LogRet)
tgt_mean <- mean(tgt_df$LogRet)
jcp_mean <- mean(jcp_df$LogRet)
wmt_mean <- mean(wmt_df$LogRet)
macy_mean <- mean(macy_df$LogRet)
amzn_mean <- mean(amzn_df$LogRet)

print(paste(" Mean Log return of Kohls is",kss_mean))
print(paste(" Mean Log return of Target is",tgt_mean))
print(paste(" Mean Log return of JCP is",jcp_mean))
print(paste(" Mean Log return of Walmart is",wmt_mean))
print(paste(" Mean Log return of Macy's is",macy_mean))
print(paste(" Mean Log return of Amazon is",amzn_mean))

print("-----------------------------------------------------------------------")
print("         Variance of  Each Stocks' Log Returns                           ")
print("-----------------------------------------------------------------------")

######################################################################################################
# Calculatig Variance of LogRet of each of the Stock Log returns
######################################################################################################
kss_var <- (sd(kss_df$LogRet))^2
tgt_var <- (sd(tgt_df$LogRet))^2
jcp_var <- (sd(jcp_df$LogRet))^2
wmt_var <- (sd(wmt_df$LogRet))^2
macy_var <-(sd(macy_df$LogRet))^2
amzn_var <-(sd(amzn_df$LogRet))^2

print(paste(" Variance of Log return of Kohls is",kss_var))
print(paste(" Variance of Log return of Target is",tgt_var))
print(paste(" Variance of Log return of JCP is",jcp_var))
print(paste(" Variance of Log return of Walmart is",wmt_var))
print(paste(" Variance of Log return of Macy's is",macy_var))
print(paste(" Variance of Log return of Amazon is",amzn_var))

print("-----------------------------------------------------------------------------")
print("   95% Confidence Interval for Mean and Variance of  Each Stocks' Log Returns                           ")
print("-----------------------------------------------------------------------------")

######################################################################################################
# Calculating 95% Confidence Intervals for Mean
# Sample Mean, Sample Variance Data Available, so using T-Value for Confidence Interval calc for Mean
# Chi-Square Value for Confidence Interval for Variance 
# 95% Confidence Interval used here, Alpha = 0.05 (1-0.95)
######################################################################################################
calc_95_intr <- function(stock_name,stock_df) {
    stock_mean <- mean(stock_df$LogRet)
    stock_sd <- sd(stock_df$LogRet)
    stock_variance <- stock_sd^2
    stock_size <- length(stock_df$LogRet)
    std_err <- stock_variance/sqrt(stock_size)
    err_margin <- std_err*qt(0.975,df=stock_size-1)
    lower_int_mean <- stock_mean - err_margin
    upper_int_mean <- stock_mean + err_margin
    print(paste("95% Confidence Interval for ",stock_name,"Log Return Mean is : [",lower_int_mean,",",upper_int_mean,"]"))
    lower_int_variance <- (stock_size-1)*stock_variance/qchisq(0.025,stock_size-1)
    upper_int_variance <- (stock_size-1)*stock_variance/qchisq(0.975,stock_size-1)
    print(paste("95% Confidence Interval for ",stock_name,"Log Return Variance is : [",lower_int_variance,",",upper_int_variance,"]"))
}

calc_95_intr("Kohls",kss_df)
calc_95_intr("target",tgt_df)
calc_95_intr("JCP",jcp_df)
calc_95_intr("Walmart",wmt_df)
calc_95_intr("Macys",macy_df)
calc_95_intr("Amazon",amzn_df)

 
print("-----------------------------------------------------------------------------")
print("   Regression of Each Stock's Log Return on Time                             ")
######################################################################################################
# Regression of Log-Return on Time
# lm() command performs the regression with variables, LogRet and Date 
# Null-Hypothesis is Time has no effect on Log Return ( p > 0.05 )
# Alternate Hypothesis is Time has effect on Log Return ( p < 0.05 ) 
######################################################################################################

reg_logret_time <- function (stock_name, stock_df) {
    
    print("-----------------------------------------------------------------------------")
    print(paste("///////////// Linear Regression of",stock_name,"LogRet On Time ///////////////"))
    print("-----------------------------------------------------------------------------")
    
    #lm() Linear Regression between LogRet and Date 
    lin_reg <- lm(LogRet~Date, data = stock_df)
    p_val <- summary(lin_reg)$coefficients[2,4]
    r_sqr <- summary(lin_reg)$r.squared 
    
    #<Debug> print(summary(lin_reg))
    
    print(paste("Intercept and Co-efficient of",stock_name, "LogRet Regression on Time are"))
    print(lin_reg$coefficients)
    
    
    print(paste("R-Squared Value is", r_sqr ))
    print(paste("p-value is",p_val))
    
    if (p_val >= 0.05){
        print(paste("p_val is > 0.05, No sufficient evidence to reject Null-Hypothesis, Time has no effect on Log-Return for", stock_name,"Stock Data"))
    } else {
        print(paste("p_val is < 0.05, Rejecting Null-Hypothesis, Time has effect on Log-Return for", stock_name,"Stock Data"))
    }
    
    plot(stock_df$Date, stock_df$LogRet, xlab = "Date(1998-2017)", ylab = paste(stock_name ,"LogReturn"), col= "#a54c15", main = paste("LogRetun of", stock_name," vs Time(Date)"))
    abline(lin_reg)
    
    plot(stock_df$Date, lin_reg$residuals, xlab = "Date(1998-2017)", ylab = "Residuals",col="#6f1c82", main= paste("Residual Plot -", stock_name))  
    abline(lin_reg)
}

######################################################################################################
# Calling Function for regression on Time for each stock
######################################################################################################
reg_logret_time("Kohls",kss_df)
reg_logret_time("Target",tgt_df)
reg_logret_time("JCP",jcp_df)
reg_logret_time("Walmart",wmt_df)
reg_logret_time("Macys",macy_df)
reg_logret_time("Amazon",amzn_df)   


######################################################################################################
# Extracting Stock Data only for Thankgiving Period
# November 1st week to December 8th for each year
######################################################################################################
get_holiday_stock <- function(stock_df) {
    holiday_df <-  stock_df[which((stock_df$Month == "November") | ( (stock_df$Month == "December") & (as.POSIXlt(stock_df$Date)$mday < 9))),c(1,4,7,10,11)]
    return(holiday_df)
    
}

kss_hol_df <- get_holiday_stock(kss_df)
tgt_hol_df <- get_holiday_stock(tgt_df)
jcp_hol_df <- get_holiday_stock(jcp_df)
wmt_hol_df <- get_holiday_stock(wmt_df)
macy_hol_df <- get_holiday_stock(macy_df)
amzn_hol_df <- get_holiday_stock(amzn_df)

print("-----------------------------------------------------------------------")
print(" Runs Test for checking Randomnes of Stock Data during Holiday Period  ")
print("-----------------------------------------------------------------------")
######################################################################################################
# Calling and Printing the P value for each Stock Holiday Data Frame to check Randomness
######################################################################################################
check_random_sample("Kohls", kss_hol_df)
check_random_sample("Target", tgt_hol_df)
check_random_sample("JCP",jcp_hol_df)
check_random_sample("Walmart",wmt_hol_df)
check_random_sample("Macys",macy_hol_df)
check_random_sample("Amazon", amzn_hol_df)

######################################################################################################
# Calling hist fn to build Histograms for stock log returns During Holiday Period
######################################################################################################

hist(kss_hol_df$LogRet,main=" Histogram of Kohls Log Return - Holiday Period",col="darkgreen",xlab="KSS Log Return")
hist(tgt_hol_df$LogRet,main=" Histogram of TARGET Log Return - Holiday Period",col="darkgreen",xlab="TGT Log Return")
hist(jcp_hol_df$LogRet,main=" Histogram of JCP Log Return - Holiday Period",col="darkgreen",xlab="JCP Log Return")
hist(wmt_hol_df$LogRet,main=" Histogram of WALMART Log Return - Holiday Period",col="darkgreen",xlab="WMT Log Return")
hist(macy_hol_df$LogRet,main=" Histogram of Macys Log Return - Holiday Period",col="darkgreen",xlab="M Log Return")
hist(amzn_hol_df$LogRet,main=" Histogram of AMAZON Log Return - Holiday Period",col="darkgreen",xlab="AMZN Log Return")

######################################################################################################
#  QQNORM function to draw Normal Probability Plots for each of the stocks During Holiday Period
######################################################################################################
qqnorm(kss_hol_df$LogRet,main = "Normal Q-Q Plot for Kohls LogReturn - Holiday Period")
qqline(kss_hol_df$LogRet)
qqnorm(tgt_hol_df$LogRet,main = "Normal Q-Q Plot for TARGET LogReturn - Holiday Period")
qqline(tgt_hol_df$LogRet)
qqnorm(jcp_hol_df$LogRet,main = "Normal Q-Q Plot for JCP LogReturn - Holiday Period")
qqline(jcp_hol_df$LogRet)
qqnorm(wmt_hol_df$LogRet,main = "Normal Q-Q Plot for WALMART LogReturn - Holiday Period")
qqline(wmt_hol_df$LogRet)
qqnorm(macy_hol_df$LogRet,main = "Normal Q-Q Plot for Macys LogReturn - Holiday Period")
qqline(macy_hol_df$LogRet)
qqnorm(amzn_hol_df$LogRet,main = "Normal Q-Q Plot for AMAZON LogReturn - Holiday Period")
qqline(amzn_hol_df$LogRet)

print("-----------------------------------------------------------------------------")
print("   95% Confidence Interval for Mean and Variance of  Holiday Log Returns                           ")
print("-----------------------------------------------------------------------------")
calc_95_intr("Kohls",kss_hol_df)
calc_95_intr("target",tgt_hol_df)
calc_95_intr("JCP",jcp_hol_df)
calc_95_intr("Walmart",wmt_hol_df)
calc_95_intr("Macys",macy_hol_df)
calc_95_intr("Amazon",amzn_hol_df)

######################################################################################################
# Calling Function for regression on Time for each stock
######################################################################################################
reg_logret_time("Kohls Holiday",kss_hol_df)
reg_logret_time("Target Holiday",tgt_hol_df)
reg_logret_time("JCP Holiday",jcp_hol_df)
reg_logret_time("Walmart Holiday",wmt_hol_df)
reg_logret_time("Macys Holiday",macy_hol_df)
reg_logret_time("Amazon Holiday",amzn_hol_df)   

######################################################################################################
#Populating all Thanksgiving dates for last 20 years into a list 
######################################################################################################
tg_dates <- as.Date(c("1998-11-26","1999-11-25","2000-11-23","2001-11-22","2002-11-28",
                          "2003-11-27","2004-11-25","2005-11-24","2006-11-23","2007-11-22",
                          "2008-11-27","2009-11-26","2010-11-25","2011-11-24","2012-11-22",
                          "2013-11-28","2014-11-27","2015-11-26","2016-11-24","2017-11-23"))

print("--------------------------------------------------------------------------------")
print("   T Test Analysis of Stock Holiday LogRet, is Pre_Hol LogRet > Post-Hol LogRet ")
######################################################################################################
# Holiday Effect on Stock Data
# pre_hol_df <- Data frame having stock data for 5 days before Thanksgiving 
# post_hol_df <- Data frame having stock data for 5 days after Thanksgiving 
######################################################################################################

holiday_effect <- function(stock_name, holiday_df){
    
    print("-----------------------------------------------------------------------------")
    print(paste("///////////// T Test of Pre-Holiday Log Returns and Post-Holiday Log Returns of",stock_name,"///////////////"))
    
    ## Deleting contents of Pre Holiday Dataframe
    pre_hol_df <- NULL
    # Fetching for 5 Days Before Thanksgiving
    for (tg in tg_dates) {
        
        row_tg <- which(holiday_df$Date==tg-1)
        temp_pre_hol <- holiday_df[(row_tg-4):row_tg,]
        pre_hol_df <- rbind(pre_hol_df,temp_pre_hol)
    }
    
    ## Deleting contents of post Holiday Dataframe
    post_hol_df <- NULL
    # Fetching for 5 Days Aftrer Thanksgiving
    for (tg in tg_dates) {
        
        row_tg <- which(holiday_df$Date==tg+1)
        temp_post_hol <- holiday_df[row_tg:(row_tg+4),]
        post_hol_df <- rbind(post_hol_df,temp_post_hol)
    }
    
    
    # Null Hypothesis H0 : Pre_Hol_Mean = Post_Hol_Mean
    # Test Statistic TS = Mean(X)-Mean(Y)/SQRT(Var(X)/N+ VAR(Y)/N)
    # t.test Does the same Test for Checking Means of 2 Sample Mean with unknown and Unequal Variance
    
    # Commenting the below code for Test Statistic Calculation as it returned same value as t.test
    pre_hol_mean <- mean(pre_hol_df$LogRet)
    # pre_hol_var <- (sd(pre_hol_df$LogRet))^2
    # pre_hol_size <- length(pre_hol_df$LogRet)
    # #<Debug> print(pre_hol_mean)
    post_hol_mean <- mean(post_hol_df$LogRet)
    # post_hol_var <- (sd(post_hol_df$LogRet))^2
    # post_hol_size <- length(post_hol_df$LogRet)
    # #<Debug> print(post_hol_mean)
    # TS_Nr <- pre_hol_mean - post_hol_mean
    # TS_Dr <- ((pre_hol_var/pre_hol_size) + (post_hol_var/post_hol_size)) ^ 0.5
    # TS = TS_Nr/TS_Dr
    # #<Debug> print(TS)
    
    ##------------------------------------------------------------------------------------
    ## Using t.test to get Test Statistic , default 95% confidence Interval
    ## Null Hypothesis = H0: Pre Holiday Mean == Post Holiday Mean
    ## using attribute atlternative="greater" which means mean1-mean2 > 0 for alternate Hypothesis 
    ## If Test Statistic (TS)>  1.96, reject H0
    ##------------------------------------------------------------------------------------
    
    #<Debug> print(t.test(pre_hol_df$LogRet,post_hol_df$LogRet, alternative="greater")) # 1-sided
    TS <- t.test(pre_hol_df$LogRet,post_hol_df$LogRet)$statistic
    p_val <- t.test(pre_hol_df$LogRet,post_hol_df$LogRet)$p.value
    print(paste("Pre-Holiday Mean : ",pre_hol_mean, "| Post-Holiday Mean : ", post_hol_mean))
    print(paste("Test Statistic : ", TS))
    print(paste("Test p_value   : ", p_val))
    
    if (TS < 1.96) {
        print(" Test Statistic < 1.96, No sufficient Evidence to reject Null Hypothesis") #95% 2 sided
        #print(" Test Statistic < 1.645 No sufficient Evidence to reject Null Hypothesis")  #95% 1-sided
        print("Pre Holiday Mean Equal to Post Holiday Mean")
    } else {
        print(" Rejecting Null Hypothesis that the Means are Equal")
    }
    
    
}

##########################################################################################
# Calling Function holiday_effect to check Holiday effect on each of the Stocks
##########################################################################################

holiday_effect("Kohls",kss_hol_df)
holiday_effect("Target",tgt_hol_df)
holiday_effect("JCP",jcp_hol_df)
holiday_effect("Walmart",wmt_hol_df)
holiday_effect("Macys",macy_hol_df)
holiday_effect("Amazon",amzn_hol_df)


##################################################################################
# get_hol_df function returns the holiday data frame for an input stock name
##################################################################################
get_hol_df <- function(stock_name){
    
    if(stock_name == "Kohls") {
        return(kss_hol_df)
    } else if (stock_name == "Target") {
        return(tgt_hol_df)
    } else if (stock_name == "JCP") {
        return(jcp_hol_df)
    } else if (stock_name == "Walmart") {
        return(wmt_hol_df)
    } else if (stock_name == "Macy") {
        return(macy_hol_df)
    } else if (stock_name == "Amazon") {
        return(amzn_hol_df)
    } else {
        print("Invalid Retail Name ")
    }
}

print("--------------------------------------------------------------------------------")
print("    Regression between  Two Stock Log Returns                         ")
##################################################################################
# two_stock_regression() does Regression between 2 Stock inputs Log Return 
# P-Value and R-Sqaured Value is analysed to check for correlation
##################################################################################
two_stock_regression <- function(stock1, stock2) {
    
    print("--------------------------------------------------------------------------------")
    print(paste("/// Regression Between", stock1, "and", stock2))
    
    stock1_df <- get_hol_df(stock1) 
    stock2_df <- get_hol_df(stock2) 
    
    two_reg <- lm(stock1_df$LogRet~stock2_df$LogRet)
    
    p_val <- summary(two_reg)$coefficients[2,4]
    r_sqr <- summary(two_reg)$r.squared 
    
    #<Debug> print(summary(two_reg))
    
    print(paste("Intercept and Co-efficient of",stock1, "Regression LogRet on",stock2,"are"))
    print(two_reg$coefficients)
    
    
    print(paste("R-Squared Value is", r_sqr ))
    print(paste("p-value is",p_val))
    
    #if (p_val > 0.05){
    #    print(paste("p_val is > 0.05, No sufficient evidence to reject Null-Hypothesis, No Correlation between stock Log returns" ))
    #} else {
    #    print(paste("p_val is < 0.05, Rejecting Null-Hypothesis, Correlation Between Stock Log Returns exist" ))
    #}
    
    # Plotting Stock 1 returns VS Stock 2 Returns
    plot(stock1_df$LogRet, stock2_df$LogRet, xlab=paste(stock1,"Log Returns"), ylab = paste(stock2,"Log Returns"), col="#256da8", main = paste(stock2, "vs", stock1, "Regression"))
    abline(two_reg)
    
    ## Plotting Residuals VS Stock 1 LogReturn
    plot(stock1_df$LogRet,two_reg$residuals, xlab=paste(stock1,"Log Returns"), ylab = "Residuals",col="#3d933b", main = paste("Residuals vs ",stock1,"Log Returns"))
    abline(two_reg)
    
}

stock_name_list <- c("Kohls", "Target", "JCP", "Walmart", "Macy", "Amazon")

#############################################################################################
## Calling two_stock_regression() function to perform regression between 2 stocks each in iteration
#############################################################################################
for (i in  stock_name_list) {
    for (j in stock_name_list) {
        if ( i != j ) {
            two_stock_regression(i, j)
        }
    }
}

print("--------------------------------------------------------------------------------")
print("    T-Test between 2 Stock LogRet , to check if Means are Equal  ")
#############################################################################################
# This function gets the stock name as input arguments and fetches the holiday Stock Dataframe
# t,test() funtion is used to check if Stock 1 Mean == Stock 2 Mean
# Null Hypothesis = H0: Stock1 Mean == Stock2 Mean
# using attribute atlternative="greater" which means mean1-mean2 > 0 for alternate Hypothesis 
# If Test Statistic > TS > 1.96, reject H0 ( two sided 95% confidence interval)
#############################################################################################
two_t_test <- function (stock1, stock2){
    
    stock1_df <- get_hol_df(stock1) 
    stock2_df <- get_hol_df(stock2) 
    
    stock1_mean <- mean(stock1_df$LogRet)
    stock2_mean <- mean(stock2_df$LogRet)
    
print("--------------------------------------------------------------------------------")
print(paste("T Test Betwen", stock1, "and", stock2))
    ## Using t.test to get Test Statistic , default 95% confidence Interval
    ## Null Hypothesis = H0: Stock 1 Mean == Stock 2 Mean
    ## using attribute atlternative="greater" which means mean1-mean2 > 0 for alternate Hypothesis, in case 1-sided 
    ## If Test Statistic  TS > 1.96, reject H0
    ##------------------------------------------------------------------------------------
    
    #<Debug> print(t.test(pre_hol_df$LogRet,post_hol_df$LogRet, alternative="greater"))
    TS <- t.test(stock1_df$LogRet,stock2_df$LogRet)$statistic
    p_val <- t.test(stock1_df$LogRet,stock2_df$LogRet)$p.value  
    print(paste(stock1,"LogRet Mean : ",stock1_mean, "|", stock2, "LogRet Mean : ", stock2_mean))
    print(paste("Test Statistic : ", TS))
    print(paste("Test p_value   : ", p_val))
    
    if (TS < 1.96) {
        print(" Test Statistic < 1.96, No sufficient Evidence to reject Null Hypothesis")
        print(paste(stock1," Mean Equal to", stock2,"Mean"))
    } else {
        print(" Rejecting Null Hypothesis that the Means are Equal")
    }
    
}

#############################################################################################
## Calling two_t_test() function to perform t.test between 2 stocks each in iteration
#############################################################################################
for (i in  stock_name_list) {
    for (j in stock_name_list) {
        if ( i != j ) {
            two_t_test(i, j)
        }
    }
}

sink()




dev.off()

