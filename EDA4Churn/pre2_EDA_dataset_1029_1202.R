data = read.csv('Member Dataset.csv')
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
col_name_list = read.table('colnames.list.txt')
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

# EDA question: correlation between when the customer closes the account and features
# precision: by month, ie. July, Aug or September
open_user$Jul.closed = 0
open_user$Aug.closed = 0
open_user$Sept.closed = 0
open_user$Oct.closed = 0
open_user$Open = 0
open_user$Jul.closed[(open_user$ClosedDate > as.Date('2018-06-30')) & (open_user$ClosedDate <= as.Date('2018-07-31')) %in% TRUE] = 1
open_user$Aug.closed[(open_user$ClosedDate > as.Date('2018-07-31')) & (open_user$ClosedDate <= as.Date('2018-08-31')) %in% TRUE] = 1
open_user$Sept.closed[(open_user$ClosedDate > as.Date('2018-08-31')) & (open_user$ClosedDate <= as.Date('2018-09-30')) %in% TRUE] = 1
open_user$Oct.closed[(open_user$ClosedDate > as.Date('2018-09-30')) & (open_user$ClosedDate <= as.Date('2018-10-31')) %in% TRUE] = 1
open_user$Open[is.na(open_user$ClosedDate) %in% TRUE] = 1



library(ISLR)
typeof(open_user$closed)
open_user$closed =  as.factor(open_user$closed)
class(open_user$closed)
glm.fit = glm(closed~., data = open_user, family=binomial)

str()

