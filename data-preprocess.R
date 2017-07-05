#read approved loan data
approved1 <- read.csv("./LoanStats/LC2007-2011.csv", stringsAsFactors = FALSE, header = T, skip = 1)
approved2 <- read.csv("./LoanStats/LC2012-2013.csv", stringsAsFactors = FALSE, header = T, skip = 1)
approved3 <- read.csv("./LoanStats/LC2013-2014.csv", stringsAsFactors = FALSE, header = T, skip = 1)
approved4 <- read.csv("./LoanStats/LC2015.csv", stringsAsFactors = FALSE, header = T, skip = 1)
approved <- rbind(approved1, approved2, approved3, approved4)
#some rows are notes, remove them
approved <- approved[-which(rowSums(!is.na(approved))<30),]

#read rejected loan data
rejected1 <- read.csv("./RejectStats/RejectStatsA.csv", stringsAsFactors = FALSE, header = T, skip = 1)
rejected2 <- read.csv("./RejectStats/RejectStatsB.csv", stringsAsFactors = FALSE, header = T, skip = 1)
rejected3 <- read.csv("./RejectStats/RejectStatsD.csv", stringsAsFactors = FALSE, header = T, skip = 1)
rejected <- rbind(rejected1, rejected2, rejected3)

rm(approved1,approved2,approved3,approved4,rejected1,rejected2,rejected3)

#select variables we need
#loan_amnt, term, grade, emp_length, home_ownership, annual_inc, loan_status, purpose, addr_state, dti, fico_range_high
approved <- approved[,c(3,6,9,12,13,14,17,21,24,25,29)]
colnames(approved) <- c("amount", "term", "grade", "employment", "home_ownership", "income", "status", "purpose", "state", "dti", "score")

#Amount Requested, Risk_Score(fico), Debt-To-Income Ratio, State, Employment Length
rejected <- rejected[,c(1,4,5,7,8)]
colnames(rejected) <- c("amount", "score", "dti", "state", "employment")

#convert term values to numberic
approved <- transform(approved, term=ifelse(grepl(" months", term),gsub(" months","",term),term))
approved$term <- as.numeric(approved$term)

#convert dti values to numberic
rejected <- transform(rejected, dti=ifelse(grepl("%", dti),gsub("%","",dti),dti))
rejected$dti <- as.numeric(rejected$dti)

#remove the records with fico scores < 660
#Lending Club changed their policy and does not issue loan for scores below 660
approved <- subset(approved, score > 660)

# Removing "Does not meet the credit policy.  Status:" from:
# Does not meet the credit policy.  Status:Charged Off
# Does not meet the credit policy.  Status:Fully Paid
approved$status <- ifelse(grepl("Does not meet the credit policy. Status:", approved$status),
                         gsub("Does not meet the credit policy. Status:","",approved$status),
                         approved$status)

#convert employment length to numberic(< 1 year:0, 10+ years:10)
rejected$employment <- ifelse(rejected$employment=="< 1 year", 0,
                       ifelse(rejected$employment=="1 year", 1,
                       ifelse(rejected$employment=="10+ years",10,
                       ifelse(grepl(" years",rejected$employment),gsub(" years","",rejected$employment),rejected$employment))))

rejected$employment <- as.numeric(rejected$employment)

approved$employment <- ifelse(approved$employment=="< 1 year", 0,
                       ifelse(approved$employment=="1 year", 1,
                       ifelse(approved$employment=="10+ years",10,
                       ifelse(grepl(" years",approved$employment),gsub(" years","",approved$employment),approved$employment))))

approved$employment <- as.numeric(approved$employment)

#save data
save(approved, file="approved.RData")
save(rejected,file="rejected.RData")
