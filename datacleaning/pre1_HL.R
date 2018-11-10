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
july_trans_col_index = grep("July.Trans", colnames(open_user))
july_trans_col_index
july_index = grep("July_Bal", colnames(open_user))
Aug_index = grep("Aug_Bal", colnames(open_user))
Sept_index = grep("Sept_Bal", colnames(open_user))
open_user = open_user[,c(1:6,39,7:17,40,18:28,41,29:38)]



#replace the invalid ZIPCODE data to the mode of ZIPCODE
open_user$ZipCode_Validated = as.numeric(as.character(open_user$ZipCode_Validated))
open_user[is.na(open_user$ZipCode_Validated)==TRUE,"ZipCode_Validated"] = 0
valid_zip_mask = (open_user$ZipCode_Validated-10000)>0
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


open_user[!valid_zip_mask,"ZipCode_Validated"] = getmode(open_user[valid_zip_mask,"ZipCode_Validated"]) 
open_user[!valid_zip_mask,"ZipCode_Validated"]
