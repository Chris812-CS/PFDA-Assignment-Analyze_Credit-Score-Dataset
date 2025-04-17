#Christian Teo Li Xin TP077611
#Objective 1: To investigate the relationship between credit history and credit class
#---------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(vcd)
library(nnet)
library(randomForest)
library(caret)
options(scipen = 999)

dataURL = "C:/Users/Chris Teo/OneDrive/Documents/PFDA/Assignment/DataCleaningUpdatedCSV.csv"
data = read.csv(dataURL)

convert_to_types <- function(data) {
  factor_columns <- c(
    "checking_status", "credit_history", "purpose", "savings_status",
    "employment", "personal_status", "other_parties", "property_magnitude",
    "other_payment_plans", "housing", "job", "own_telephone",
    "foreign_worker", "class"
  )
  
  numeric_columns <- c(
    "credit_amount", "installment_commitment", "residence_since",
    "age", "existing_credits", "num_dependents"
  )
  
  data[factor_columns] <- lapply(data[factor_columns], as.factor)
  
  data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)
  
  return(data)
}

data = convert_to_types(data)
data

#1. What is the overall distribution of credit classes?
#-----------------------------------------------------------------------------------------------------------------------------
#visualization - bar plot to show the count of each data
ggplot(data, aes(credit_history, fill = credit_history)) + 
  geom_bar() + 
  labs(
    title = "Credit History Distribution",
    x = "Credit History Category",  # Label for the x-axis
    y = "Count"                     # Label for the y-axis
  )

count = data %>%
  group_by(class) %>%
  summarise(total_count = sum(n()))
count

count$percentage <- round(count$total_count / sum(count$total_count) * 100, 1)

ggplot(count, aes(x = "", y = total_count, fill = class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert bar chart to pie chart
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  # Add percentages
  labs(title = "Proportion of Credit Classes", fill = "Credit Class") 

#2. What is the distribution of credit history across two credit classes?
#---------------------------------------------------------------------------------------------------------------------------------
#find relationship between credit history and class using chi-square test
contingency_table <- table(data$credit_history, data$class)
chi_test <- chisq.test(contingency_table)
chi_test

# distribution of credit history across credit class
observed_value = chi_test$observed

#visualize distribution to compare 
observed_value <- data.frame(observed_value)
colnames(observed_value) = c("Credit_History", "credit_class", "Freq")
observed_value

ggplot(observed_value, aes(x = Credit_History, y = Freq, fill = credit_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Bad and Good in Categories",
    x = "Credit History Category",
    y = "Count",
    fill = "Status"
  ) + 
  geom_text(
    aes(label = Freq),  # Specify the column that holds the text labels
    position = position_dodge(width = 0.9),  # Adjust position for dodged bars
    vjust = -0.5,       # Place text slightly above bars
    size = 4            # Adjust text size
  )

#3. why all paid and delayed previously has higher bad credit class counts? 
#why existing paid has the highest good credit class among other category? 
#-----------------------------------------------------------------------------------------------------------------------------------------------------
data$credit_history = relevel(data$credit_history, ref = "critical/order existing credit")
data$job = relevel(data$job, ref = "unskilled resident")

unique(data$job)

# Chi Square test to check the relationship of credit history and job
contingency_table <- table(data$credit_history, data$job)
chi_test <- chisq.test(contingency_table)
chi_test

# multinomial logistic regression model to study how
# job impact credit history 
set.seed(24)
model <- multinom(credit_history ~ job, data = data)

# Collect the estimated coefficient
estimates = coef(model)
estimates = t(estimates)

#calculate the odds_ratio
odds_ratio = exp(estimates)
odds_ratio = as.data.frame(odds_ratio)
odds_ratio[-1, ]

#Counts of each credit history, class and job
count_data <- data %>%
  filter(credit_history != "critical/order existing credit" &
           job != "unskilled resident") %>%
  group_by(credit_history, job, class) %>%
  summarise(count = n())

ggplot(count_data, aes(x = job, y = credit_history, fill = count)) +
  geom_tile(color = "white") +  # Adds gridlines
  scale_fill_gradient(low = "lightpink", high = "red") +  # Adjust colors
  labs(title = "Heatmap of Credit History vs Job", x = "Job", 
       y = "Credit History", fill = "Count") +
  facet_wrap(~class) + 
  geom_text(aes(label = count))

#4. How well can credit history predict the credit class (good or bad), 
#and what role does job play in enhancing the predictive accuracy across 
#different credit history categories?
#--------------------------------------------------------------------------------------------------------------------------------------
#random forest without job
#set seed
set.seed(42)

#split data
train_index <- sample(1:nrow(data), 0.8*nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]

#random forest
rf_model <- randomForest(class ~ credit_history, data = train_data)

# Predict the credit class on the test set
rf_predictions <- predict(rf_model, test_data)

# Evaluate the model’s accuracy
confusionMatrix(rf_predictions, test_data$class)

#----------------------------------------------------------------------------------------------------------
#random forest with job
rf_model_1 <- randomForest(class ~ credit_history + job, data = train_data)

# Predict the credit class on the test set
rf_predictions_1 <- predict(rf_model_1, test_data)

# Evaluate the model’s accuracy
confusionMatrix(rf_predictions_1, test_data$class)

dataset <- data.frame(credit_history = data$credit_history, 
                      job = data$job, 
                      class = data$class)