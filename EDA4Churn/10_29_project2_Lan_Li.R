data = read.csv('downloads/Member Dataset.csv')
data = as.data.frame(data)
#change the format of date data to date type
data$ClosedDate =  as.Date(data$ClosedDate,format="%m/%d/%Y")
data$EarliestMemAcctDate = as.Date(data$EarliestMemAcctDate,format="%m/%d/%Y")

#seperate closed account before july from the original data
closed_mask = is.na(as.character(data$ClosedDate))==FALSE & data$ClosedDate<as.Date("2018-07-01")
closed_user= data[closed_mask,c(1,2,3,4,5)]
open_user = data[!closed_mask,]

#reorder the columns
open_user = open_user[,c(1:6,39,7:17,40,18:28,41,29:38)]

addzero<-function(x){
  x<-sprintf("%05d", x)
}
#replace the invalid ZIPCODE data to the mode of ZIPCODE
open_user$ZipCode_Validated = as.character(open_user$ZipCode_Validated)
open_user[is.na(as.numeric(open_user$ZipCode_Validated)),"ZipCode_Validated"] = 0 
open_user$ZipCode_Validated = as.numeric(open_user$ZipCode_Validated)
newzip = apply(as.matrix(open_user$ZipCode_Validated),1,addzero)
open_user$ZipCode_Validated = newzip

#change column names to differentiate balance data by month
col_name_list = read.table('downloads/colnames.list.txt')
dimnames(open_user)[2] = col_name_list


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

zip_mode = getmode(open_user$ZipCode_Validated) 
open_user[open_user$ZipCode_Validated=="00000","ZipCode_Validated"] = zip_mode

#replace invalid age with mean of valid ages
valid_age_mask = !is.na(open_user$Age)
age_mean = mean(open_user[valid_age_mask,"Age"])
open_user[!valid_age_mask,"Age"] = age_mean


# for values in July.trans, Aug.trans, and Sept.trans, 
# set them to 0s.
# then check for closed date
# if closed date < July, replace 0s with NAs 
# if July < closed date < a month,
#             closed month 0,
#            later month NAs

open_user$July.Trans[is.na(open_user$July.Trans)] = 0
open_user$August.Trans[is.na(open_user$August.Trans)] = 0
open_user$September.Trans[is.na(open_user$September.Trans)] = 0

cutOffJuly = as.Date('2018-07-31')
cutOffAug = as.Date('2018-08-31')

open_user$August.Trans[(open_user$ClosedDate < cutOffJuly) %in% TRUE] = NA
open_user$September.Trans[(open_user$ClosedDate < cutOffAug) %in% TRUE] = NA

dim(open_user)
str(open_user)


# EDA question: correlation between when the customer closes the account and features
# precision: by month, ie. July, Aug or September
open_user$closed = 'Active'
open_user$closed[(open_user$ClosedDate < as.Date('2018-07-01')) %in% TRUE] = 'Before July'
open_user$closed[(open_user$ClosedDate > as.Date('2018-06-30')) & (open_user$ClosedDate <= as.Date('2018-07-31')) %in% TRUE] = 'July'
open_user$closed[(open_user$ClosedDate > as.Date('2018-07-31')) & (open_user$ClosedDate <= as.Date('2018-08-31')) %in% TRUE] = 'August'
open_user$closed[(open_user$ClosedDate > as.Date('2018-08-31')) & (open_user$ClosedDate <= as.Date('2018-09-30')) %in% TRUE] = 'September'
open_user$closed[(open_user$ClosedDate > as.Date('2018-09-30')) & (open_user$ClosedDate <= as.Date('2018-10-31')) %in% TRUE] = 'October'


sum(open_user$closed == 3)

library(ISLR)
typeof(open_user$closed)
open_user$closed =  as.factor(open_user$closed)
class(open_user$closed)
glm.fit = glm(closed~., data = open_user, family=binomial)

str()


# 10 features: Trans, Visa, Mortgage, Home Equality, Other Loan, Vehicle, Checking, Savings, CD, IRA, Money Market
# create a label with dates


# merge features
# merge_Trans=rbind(open_user$July.Trans,open_user$August.Trans,open_user$September.Trans)
dfnew = data.frame(open_user$July.Trans, open_user$Visa.Jul,open_user$Money.Market.Jul,open_user$Home.Equity.Jul,open_user$Other.Loan.Jul,open_user$Vehicle.Jul,open_user$Checking.Jul,open_user$Savings.Jul,open_user$CD.Jul,open_user$IRA.Jul,open_user$Money.Market.Jul)
colnames(dfnew) = c("Transaction","Visa","Mortgage","Home.Equity","Other.Loan","Vehicle","Checking","Savings","CD","IRA","Money.Market")
dfnew$month=as.factor(7)

dfnew1 = data.frame(open_user$August.Trans, open_user$Visa.Aug,open_user$Money.Market.Aug,open_user$Home.Equity.Aug,open_user$Other.Loan.Aug,open_user$Vehicle.Aug,open_user$Checking.Aug,open_user$Savings.Aug,open_user$CD.Aug,open_user$IRA.Aug,open_user$Money.Market.Aug)
colnames(dfnew1) = c("Transaction","Visa","Mortgage","Home.Equity","Other.Loan","Vehicle","Checking","Savings","CD","IRA","Money.Market")
dfnew1$month=as.factor(8)

dfnew2 = data.frame(open_user$September.Trans, open_user$Visa.Sept,open_user$Money.Market.Sept,open_user$Home.Equity.Sept,open_user$Other.Loan.Sept,open_user$Vehicle.Sept,open_user$Checking.Sept,open_user$Savings.Sept,open_user$CD.Sept,open_user$IRA.Sept,open_user$Money.Market.Sept)
colnames(dfnew2) = c("Transaction","Visa","Mortgage","Home.Equity","Other.Loan","Vehicle","Checking","Savings","CD","IRA","Money.Market")
dfnew2$month=as.factor(9)

merge_df=rbind(dfnew,dfnew1,dfnew2)

library(ggplot2)

trans=ggplot(merge_df,aes(x=Transaction,fill=month))+geom_histogram(bins=35)
visa=ggplot(merge_df,aes(x=Visa,fill=month))+geom_histogram(bins=35)
mort=ggplot(merge_df,aes(x=Mortgage,fill=month))+geom_histogram(bins=35)
homee=ggplot(merge_df,aes(x=Home.Equity,fill=month))+geom_histogram(bins=35)
otherloan=ggplot(merge_df,aes(x=Other.Loan,fill=month))+geom_histogram(bins=35)
vehicle=ggplot(merge_df,aes(x=Vehicle,fill=month))+geom_histogram(bins=35)
checking=ggplot(merge_df,aes(x=Checking,fill=month))+geom_histogram(bins=35)
saving=ggplot(merge_df,aes(x=Savings,fill=month))+geom_histogram(bins=35)
cd=ggplot(merge_df,aes(x=CD,fill=month))+geom_histogram(bins=35)
ira=ggplot(merge_df,aes(x=IRA,fill=month))+geom_histogram(bins=35)
moneym=ggplot(merge_df,aes(x=Money.Market,fill=month))+geom_histogram(bins=35)

ggsave(filename="transaction.png", plot=trans)
ggsave(filename="visa.png", plot=visa)
ggsave(filename="Mortgage.png", plot=mort)
ggsave(filename="HomeE.png", plot=homee)
ggsave(filename="Other_Loan.png", plot=otherloan)
ggsave(filename="Vehicle.png", plot=vehicle)
ggsave(filename="Checking.png", plot=checking)
ggsave(filename="Saving.png", plot=saving)
ggsave(filename="CD.png", plot=cd)
ggsave(filename="IRA.png", plot=ira)
ggsave(filename="moneym.png", plot=moneym)



## Correlation Plot
install.packages("corrplot")
library('corrplot')
open_user[,5]<-as.numeric(open_user[,5])

## Calculate Active Period
open_user$CloseDate_node <- open_user$ClosedDate
open_user$CloseDate_node[is.na(open_user$CloseDate_node)]<- Sys.Date()

open_user$Active_period <- as.numeric(open_user$CloseDate_node - open_user$EarliestMemAcctDate)

## Plot for July
M<-cor(x = (open_user[,c(48,2,5,7:17,42:45)]),use = 'pairwise.complete.obs')
M
corrplot(M, method="circle")

## Plot for Aug
M<-cor(x = (open_user[,c(48,2,5,19:29,43:45)]),use = 'pairwise.complete.obs')
M
corrplot(M, method="circle")

## Plot for Sept
M<-cor(x = (open_user[,c(48,2,5,31:41,44:45)]),use = 'pairwise.complete.obs')
M
corrplot(M, method="circle")


