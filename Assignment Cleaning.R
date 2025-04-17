library(DataExplorer)
library(dplyr)
library(ggplot2)
library(VIM)
library(plyr)
library(mice)
options(scipen = 999)

# read file
fileurl = "C:\\Users\\Chris Teo\\OneDrive\\Documents\\PFDA\\5. credit_risk_classification.csv"
df = read.csv("C:\\Users\\Chris Teo\\OneDrive\\Documents\\PFDA\\5. credit_risk_classification.csv")
df

data %>%
  group_by(df$credit_history) %>%
  summarise(count = n())

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
as.data.frame(table(df$credit_history))
df$credit_history = replace(df$credit_history, df$credit_history == 'no credits/all paid', 'all paid')


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

#plot missing
plot_missing(df)

ggplot(df, aes(x = num_dependents)) + 
  geom_boxplot()
