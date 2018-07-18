
load_lendingclub <- function(){
  library(pacman)
pacman::p_load(lubridate, data.table, stringr, caret)

lending <- fread("/Users/hauptjoh/Seafile/Credit EMP/data/lendersclub2007-2015.csv", 
                 na.strings = c("NA", ""))
lending <- lending[!is.na(id),]
lending[, term := as.numeric(str_extract(term, pattern = "[[:digit:]]*"))]

# Select time frame to be loans that matured in 2016
issueDateList <- strsplit(as.character(lending$issue_d), "-")
issueYear <- as.numeric(sapply(issueDateList, "[[", 2))
issueMonth <- sapply(issueDateList, "[[", 1)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
issueMonth <- match(issueMonth, months)
lending$issue_d <- parse_date_time(paste(issueMonth, issueYear, sep = "-"), "m-Y")
#lending$maturity_d <- lending$issue_d + lending$term * months(1)

# Use only loans originating in 2012 with a duration of 3 years
lending <- lending[issue_d >= ymd("2012-01-01") & issue_d <= ymd("2012-12-31") & term == 36,]

# Reduce to only completed loans
lending <- lending[loan_status == "Fully Paid" | loan_status == "Default" | loan_status == "Charged Off", ]

### Variable selection and cleaning ####

# Remove id variables
lending[, c("id", "member_id") := NULL]
# Remove text variables
lending[, c("url", "desc") := NULL]

# lending[, .SD := ifelse(is.na(.SD), "", 
#          .SDcols = colnames(lending)[sapply(lending, function(x) sum(is.na(x)) > nrow(lending)/2 )]


int_cf <- 0.018 # OECD average long-term interest rate for US 2012
# int_cf_month <- (1+int_cf) ^ (1/12) - 1
# 
# # Present value of installments
# PV <- function(A, int_cf, l){
#   interest <- (1 + int_cf) ^ l
#   return( (A/int_cf) * (1 - ( 1 / interest) ))  
# }
# 
# average_interest <- mean(lending$int_rate)

# Profit is present value of total payment - funded amount
lending[, PROFIT := (total_pymnt/((1+int_cf)^(term/12))) - funded_amnt]

# Calculate ROI, p1, p0 for EMP measure
ROI <- mean(lending[loan_status == "Fully Paid", (PROFIT / funded_amnt)])
p1 <- mean(lending[loan_status != "Fully Paid", total_pymnt/funded_amnt] <= 0.01)
p0 <- mean(lending[loan_status != "Fully Paid", total_pymnt/funded_amnt] >= 1)

temp <- lending[loan_status != "Fully Paid", total_pymnt/funded_amnt]
temp[temp>1] <- 1
beta_mean <- mean(temp)
sd(temp)
beta_var <- var(temp)
           
# Create target variable
lending[, BAD := factor(ifelse(loan_status == "Fully Paid", 0, 1), labels = c("GOOD", "BAD"))]
lending[, loan_status := NULL]

# Mean imputation
lending[is.na(revol_util), revol_util := mean(lending$revol_util, na.rm = TRUE)]
lending[,mortgage := 1*(home_ownership=="MORTGAGE")]
lending[,verification:= 1*(verification_status=="Verified")]
lending[, BAD:=1*(BAD=="BAD")]

# Clean up variables not useful for prediction
# Also excludes time variables for now
predVars <- c("BAD", "PROFIT",
  "funded_amnt","annual_inc", "annual_inc_joint", "application_type", 
  "dti_joint","emp_length","fico_range_high","fico_range_low","grade",
  "mortgage", "inq_fi", "inq_last_12m", "inq_last_6mths", "last_fico_range_high",
  "last_fico_range_low", "max_bal_bc","pub_rec","pub_rec_bankruptcies","pymnt_plan","revol_bal","revol_util",
  "tax_liens", "verification", "verification_status_joint")
lending <- lending[, colnames(lending) %in% predVars, with = FALSE]


# Clean up factor levels
#lending[, c("emp_title", "purpose", "title") := lapply(.SD,function(x) toupper(x)), .SDcols = c("emp_title", "purpose", "title")]
#lending[is.na(emp_title), emp_title := "MISSING"]
#lending <- lending[!is.na(title), ]

# Remove all near zero variance columns
chrCols <- names(lending)[sapply(lending, is.character)]
lending[, (chrCols) := lapply(.SD, factor), .SDcols = chrCols]
lending[, nearZeroVar(lending) := NULL]

setnames(lending, make.names(colnames(lending)))

lending <- model.matrix(~.-1, data=lending)
std <- preProcess(lending[,.(fundedn_amnt,annual_inc,inq_last_6mths, revol_bal,
                             revol_util)], method=c("center","scale"))

return(lending)
}


