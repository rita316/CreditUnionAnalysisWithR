library(readr)
Member_Dataset <- read_csv("~/Google_Drive/IS_590PF/Member Dataset.csv")
str(Member_Dataset)
Member_Dataset$ZipCode_Validated  = as.numeric(Member_Dataset$ZipCode_Validated)
str(Member_Dataset)
Member_Dataset$EarliestMemAcctDate = as.Date(Member_Dataset$EarliestMemAcctDate, format = '%m/%d/%Y')
Member_Dataset$ClosedDate = as.Date(Member_Dataset$ClosedDate, format = '%m/%d/%Y')
Member_Dataset$ClosedDate[Member_Dataset$ClosedDate> 2018-7-1 & Member_Dataset$ClosedDate<2018-9-30 ]
Member_Dataset$ClosedDate[!is.na(Member_Dataset$ClosedDate) ]
