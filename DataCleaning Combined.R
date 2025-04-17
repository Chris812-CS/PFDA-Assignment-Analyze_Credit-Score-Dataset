library(VIM)
library(mice)
library(DataExplorer)
library(ggplot2)
fileurl = "C:\\Users\\User\\Documents\\5. credit_risk_classification.csv"
df = read.csv(fileurl)
df

getmode = function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
# Nareen
#age--------------------------------------------------------------------------------------------------
View(df)
hist(df$age)
# replace age with median since age distribution is skewed
df$age = replace(df$age, is.na(df$age), median(df$age, na.rm = TRUE))
df$age = round(df$age,0)

sum(is.na(df$age))
plot_missing(df)

#personal status-------------------------------------------------------------------------------------
unique(df$personal_status)
df$personal_status = replace(df$personal_status, df$personal_status == "male div/sep",
                             "male div")
str(df$personal_status)
df$personal_status = factor(df$personal_status)
summary(df$personal_status)
sum(is.na(df$personal_status))
temp = kNN(df, "personal_status", k=5,dist_var = "age", imp_var = FALSE)
temp
df$personal_status = temp$personal_status

# savings status-----------------------------------------------------------------------------------------
unique(df$savings_status)
df$savings_status = replace(df$savings_status, df$savings_status == "no known savings",
                           "0")
df$savings_status = replace(df$savings_status, df$savings_status == "500<=X<10000",
                            "500<=X<1000")
str(df$savings_status)
df$savings_status = factor(df$savings_status, levels = c("0","<100","100<=X<500","500<=X<1000",">=1000"))
summary(df$savings_status)
df$savings_status
temp1 = kNN(df, "savings_status", k=5,dist_var = "age", imp_var = FALSE)
df$savings_status = temp1$savings_status
sum(is.na(df$savings_status))

#residence since------------------------------------------------------------------------------------
unique(df$residence_since)
#create temp data frame
df1 = df
# create temporary variable to perform mice imputation on all variables
temp23 = mice(df1, m=5, seed = 678)
# fill in the missing values in the temporary dataset created
df1 = complete(temp23)
# change the values of residence since from original data frame by replacing it with
# values of residence since from temporary data frame
df$residence_since = df1$residence_since
# round values to integer since the variable is years
df$residence_since = ceiling(df$residence_since)

# other parties-----------------------------------------------------------------------------------------
unique(df$other_parties)
df$other_parties = factor(df$other_parties)
str(df$other_parties)
summary(df$other_parties)

sum(is.na(df$other_parties))
# replace missing values of other parties variable with mode
df$other_parties = replace(df$other_parties, is.na(df$other_parties), getmode(df$other_parties))

# Chris--------------------------------------------------------------------------------------------------
#Data cleaning
#lowercase all column name
names(df) = tolower(names(df))
colnames(df)

#check missing
plot_missing(df)

#credit history
unique(df$credit_history)

#job
unique(df$job)

#property magnitude
unique(df$property_magnitude)

#credit history - assume 'no credits/all paid' as 'all paid'
df$credit_history = replace(df$credit_history, df$credit_history == 'no credits/all paid', 'all paid')
unique(df$credit_history)
as.data.frame(table(df$credit_history))

#Change credit history to factor
class(df$credit_history)
unique(df$credit_history)
df$credit_history = factor(df$credit_history,
                           labels = c("critical/order existing credit", "existing paid",
                                      "delayed previously", "all paid"),
                           levels = c("critical/order existing credit", "existing paid",
                                      "delayed previously", "all paid"))

#change property magnitude to factor
class(df$property_magnitude)
unique(df$property_magnitude)
df$property_magnitude = factor(df$property_magnitude,
                               labels = c("real estate", "life insurance", "no known property",
                                          "car"),
                               levels = c("real estate", "life insurance", "no known property",
                                          "car"))

#change job to factor
class(df$job)
unique(df$job)
df$job = factor(df$job,
                labels = c("skilled", "unskilled resident", "high qualif/self emp/mgmt",
                           "unemp/unskilled non res"),
                levels = c("skilled", "unskilled resident", "high qualif/self emp/mgmt",
                           "unemp/unskilled non res"))

#change class to factor
class(df$class)
unique(df$class)
df$class = factor(df$class,
                  labels = c("good", "bad"),
                  levels = c("good", "bad"))

#check class for credit history, job, property magnitude & class
class(df$credit_history)
class(df$job)
class(df$property_magnitude)
class(df$class)

#place credit history, job, property magnitude, class into dataframe
dataframe = data.frame(
  credit_history = df$credit_history,
  job = df$job,
  property_magnitude = df$property_magnitude,
  class = df$class
)

#check na amount
sum(is.na(dataframe))

# perform imputation (knn)
pre <- kNN(dataframe, variable = c('credit_history', 'job', 'property_magnitude'), k = 5,
           catFun = sampleCat, impNA = TRUE, trace = TRUE,
           addRF = TRUE, weightDist = TRUE)

#check for na value are cleaned
sum(is.na(pre))

#input na to original dataset
df$credit_history = pre$credit_history
df$job = pre$job
df$property_magnitude = pre$property_magnitude

#check for missing
plot_missing(df)

#Clean for existing credit and num  dependent
unique(df$existing_credits)
unique(df$num_dependents)

#assume existing credit and num dependents are integer
#use ceiling function to round up
df$existing_credits = ceiling(df$existing_credits)
df$num_dependents = ceiling(df$num_dependents)

#place existing credit, num dependents, class into dataframe
pre_data <- data.frame(existing_credits = df$existing_credits,
                       num_dependents = df$num_dependents,
                       class = df$class)

#check for na amount
sum(is.na(pre_data))

#use mice to fill na
imputed_data = mice(pre_data, m = 5, maxit = 10, method = c("pmm", "pmm", ""),
                    printFlag = TRUE, seed = 1234)

#obtain complete dataset
completed_data = complete(imputed_data)

#check complete dataset
sum(is.na(completed_data))

#impute na values to original dataset
df$existing_credits = completed_data$existing_credits
df$num_dependents = completed_data$num_dependents

#check na amount after imputing
sum(is.na(df$existing_credits))
sum(is.na(df$num_dependents))

# Owi ----------------------------------------------------------------------------------------------------

# (Credit Amount Column)
# Median  imputation
summary(df$credit_amount)
# Round up value to integer
df$credit_amount = round(df$credit_amount, digits = 0)
# Assume minimum amount of credit is 1000 (250 is illogical for new car purpose)
df$credit_amount = replace(df$credit_amount, df$credit_amount < 1000, 1000)
# Check data distribution (Distribution is skewed)
ggplot(df, aes(x = credit_amount)) +
  geom_histogram()
# Replace NA with median
df$credit_amount = replace(df$credit_amount, is.na(df$credit_amount),
                           median(df$credit_amount, na.rm = TRUE))
summary (df$credit_amount)
ggplot(df, aes(x = credit_amount)) +
  geom_histogram()
# Boxplot to check outliers
ggplot(df, aes(credit_amount))+
  geom_boxplot()
# Check the rows with max value
df %>%
  filter(credit_amount == max(credit_amount, na.rm = TRUE))
# Remove rows with max value
df = df[-which(df$credit_amount == max(df$credit_amount, na.rm = TRUE)),]


#(Employment Column)
# Use KNN imputation
# Factor employment column
df$employment = factor(df$employment)
# Count NA
count(is.na(df$employment))
summary(df$employment)
# kNN
df = kNN(df, variable = "employment", k =5, dist_var = c("age", "foreign_worker") , imp_var = FALSE)
summary(df$employment)


# (Foreign Worker Column)
# Use mode imputation
# Factor foreign worker column
df$foreign_worker = factor(df$foreign_worker)
df$foreign_worker
# Count NA
summary(df$foreign_worker)
count(is.na(df$foreign_worker))
# Replace NA with mode
df$foreign_worker = replace(df$foreign_worker, is.na(df$foreign_worker),
                            getmode(df$foreign_worker))
summary(df$foreign_worker)


# (Purpose Column)
# Use KNN imputation
# Factor purpose column
df$purpose = factor(df$purpose)
df$purpose
# Count NA
summary(df$purpose)
count(is.na(df$purpose))
# kNN
df = kNN(df, variable = "purpose", dist_var = "credit_amount",k = 5, imp_var = FALSE)
summary(df$purpose)


# (Other payment plans Column)
# Use mode imputation
# Replace empty space to NA
df$other_payment_plans = replace(df$other_payment_plans, df$other_payment_plans == "", NA)
# Factor column
df$other_payment_plans = factor(df$other_payment_plans)
# Count NA
summary(df$other_payment_plans)
count(is.na(df$other_payment_plans))
# Replace NA with mode
df$other_payment_plans = replace(df$other_payment_plans, is.na(df$other_payment_plans),
                                 getmode(df$other_payment_plans))
summary(df$other_payment_plans)


# variable <- imputation method(reason)
# credit amount <- median imputation (data distribution is skewed)
# employment <- kNN imputation (preserve the local structure and relationship within the dataset)
# foreign worker <- mode imputation (mode represents the most common category)
# purpose <- kNN imputation (preserve the local structure and relationship within the dataset)
# other payment plans <- mode imputation (mode represents the most common category)


# Naviin--------------------------------------------------------------------------------------------------

# FOR ANALYSIS
# Installment_commitment (DONE)
# Housing (DONE)

# Checking Status (status of current account[daily transactions account]) (DONE)
# Own Telephone (DONE)
# Duration (DONE)

# MODE function
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

ggplot(df, aes(x = installment_commitment, y = duration))+
  geom_point()

# For categorical variables
ggplot(df, aes(x =checking_status, color = class))+
  geom_bar()+
  facet_wrap(~class)

# For continuous variables
ggplot(df, aes(x =installment_commitment, color = class))+
  geom_histogram()+
  facet_wrap(~class)


# Check freq of categorical variables
as.data.frame(table(df$own_telephone, df$class))




############### duration #######################
# checking data type of 'duration' column
typeof(df$duration) #double

# months should be whole number, thus rounding up the data
df$duration = as.integer(round(df$duration))
is.integer(df$duration) #TRUE

# checking for extreme outlier
ggplot(df, aes(x = duration))+
  geom_histogram()
ggplot(df, aes(x = duration))+
  geom_boxplot()

# outlier = 72
# check if reasonable
df[df$duration == 72 & !is.na(df$duration), 'purpose']


# purpose is for radio/TV (assumed not reasonable)
# replaced with max value before 72
df$duration = replace(df$duration, df$duration > 60, 60)

# replace NA's
# kNN imputation using other variables assumed having relationships with imputed variable
imputed_duration = kNN(df,k = 50 ,variable = 'duration', dist_var = c('credit_amount','installment_commitment'),numFun = 'mean')
typeof(imputed_duration$duration) #double

imputed_duration$duration = as.integer(round(imputed_duration$duration))

# substitute the imputed column into original column
df$duration = imputed_duration$duration


############### own_telephone #######################
ggplot(df, aes(x = own_telephone))+
  geom_bar()
# column contains categorical values
# change column into factor
df$own_telephone = factor(df$own_telephone)

# renaming category for better readability
levels(df$own_telephone) = c('no', 'yes')

# mode imputation
# since there are no significant relationship between imputed variable and other variable
df$own_telephone = replace(df$own_telephone, is.na(df$own_telephone),getmode(df$own_telephone))


############### checking_status #######################
ggplot(df, aes(x = checking_status))+
  geom_bar()
# column contains categorical values
# change column into factor
df$checking_status = factor(df$checking_status)

df$checking_status = replace(df$checking_status, is.na(df$checking_status), getmode(df$checking_status))


############### housing #######################
ggplot(df, aes(x = housing))+
  geom_bar()
# column contains categorical values
# change column into factor
df$housing = factor(df$housing)


# assuming imputed variable has relationship with other 2 variables
imputed_housing= kNN(df,k = 10, variable = 'housing', dist_var = c('personal_status','age'))
df$housing = imputed_housing$housing

############### installment_commitment #######################
ggplot(df, aes(x=installment_commitment))+
  geom_histogram()

# round up values to 1 decimal (percentage)
df$installment_commitment = round(df$installment_commitment,1)

# impute using median as data is skewed
imputed_installment = replace(df$installment_commitment, is.na(df$installment_commitment), median(df$installment_commitment,na.rm = TRUE))
df$installment_commitment =  round(imputed_installment,1)


summary(df)
plot_missing(df)
