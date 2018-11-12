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

active_user = open_user[open_user$Open==1,]
unactive_user = open_user[!open_user$Open==1,]
active_user_train = active_user[sample(nrow(active_user),size = nrow(unactive_user)),]
origin = rbind(active_user_train,unactive_user)
origin= origin[,-c(42,43,44,45)]
origin = origin[,-c(1,3,4)]
origin[is.na(origin)]<-0

#split train and test set
smp_size <- floor(0.75 * nrow(origin))
set.seed(123)
train_index <- sample(seq_len(nrow(origin)), size = smp_size)
train <- origin[train_index, ]
test <- origin[-train_index, ]
train$ZipCode_Validated = as.factor(train$ZipCode_Validated)
test$ZipCode_Validated = as.factor(test$ZipCode_Validated)


train_x = data.matrix(train[,-39])
train_y = data.matrix(train[,39])
test_x = data.matrix(test[,-39])
test_y = data.matrix(test[,39])


#logistic regression
library(glmnet)
glm.fit = cv.glmnet(train_x,train_y,family = "binomial",type.measure = "class")
plot(glm.fit)
pred = predict(glm.fit,newx=train_x,s="lambda.min",type="class")
accuracy_train = sum(pred==train_y)/length(train_y)
accuracy_train

pred_test = predict(glm.fit,newx=test_x,s="lambda.1se",type="class")
accuracy_test = sum(pred_test==test_y)/length(test_y)
accuracy_test
