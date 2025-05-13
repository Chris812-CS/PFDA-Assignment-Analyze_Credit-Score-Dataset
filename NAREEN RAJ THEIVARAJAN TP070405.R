# Objective 2: To analyse how age impacts credit class. (TP070405 – NAREEN RAJ A/L THEIVARAJAN)

library(dplyr) #for data manipulation
library(ggplot2) # for data visualization
library(pROC) # for ROC and AUC calculation
library(caret) # for confusion matrix
library(caTools) # for splitting data
library(randomForest) # machine learning model

df123 = read.csv("C:\\Users\\User\\Documents\\Lecture Slides PFDA\\Assignment\\DataCleaningUpdatedCSV.csv")

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


df2 = convert_to_types(df123)

# Analysis 2-1----------------------------------------------------------------------------------------
# What is the distribution of age across class, are the distribution same for all age groups?

# (a) Violin + Box plot
ggplot(df2, aes(x = class, y = age, fill = class)) +
  geom_violin(trim = FALSE, alpha = 0.6) + # violin plot to show distribution
  geom_boxplot(width = 0.1, outlier.color = 'yellow') + # box plot to show IQR and outliers
  labs(
    title = "Age Distribution by Class",
    subtitle = "Comparing Age Distribution for Good and Bad Credit Classes",
    x = "Credit Class",
    y = "Age",
    fill = "Credit Class"
  )


# (b) Faceted bar plot
summary(df2$age) # check the lowest and highest age to do grouping

# classify the age into 4 different age groups with each of the range being 15 years
# only last age group has 12 years
data  = df2 %>%
        mutate(age_group =  cut(age, breaks = c(19,33,48,63,75),
                      include.lowest = TRUE,
                      labels = c("19-33", "34-48", "49-63", "64-75")))

# check if data is correctly categorized
data %>%
  filter(age == 33)%>%
  select(age_group) %>%
  unique()

data %>%
  filter(age==34) %>%
  select(age_group) %>%
  unique()

# plot the graph of age group, categorizing it by housing
ggplot(data, aes(x = age_group, fill = class)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~housing) +
  labs(
    title = "Age Group Distribution by Class and Housing",
    subtitle = "Comparison of Credit Classes Across Age Groups and Housing Categories",
    x = "Age Group",
    y = "Count",
    fill = "Credit Class"
  )


# (c) Summary statistics
data %>%
  group_by(age_group) %>%
  summarize(mean_age = mean(age), median_age = median(age), sd_age = sd(age),
            good = sum(class == 'good'),bad = sum(class == 'bad'),total = n())

# Analysis 2-2--------------------------------------------------------------------------------------
# Why are the customers who are aged below 34 generally have a bad credit class,
# does housing having an influence on this?

# (a) Chi Square Test
# see the relationship between housing and class for the age under 34
table1 = table(data$housing, data$class, data$age_group)
chisq.test(table1[,,1])
# [,,1] too choose the first age group which is 19-33 (below 34)

# (b) Logistic Regression
# subset data to contain only data where age is below 34
df_under34 <- subset(data, age < 34)

# use logistic regression on class with housing as predictor
model1 = glm(class~housing, df_under34, family = binomial)
# check the coefficients and p value
summary(model1)


# (c) Pie chart
# make data for pie chart
pie = df_under34 %>%
      group_by(housing,class) %>%
      summarize(sum = n())%>%
      group_by(housing)%>%
      mutate(total = sum(sum),percentage = paste(round((sum/total)*100,1), "%", sep = ""))

# pie chart
ggplot(pie, aes(x = "", y = sum, fill = class)) +
  geom_bar(stat = 'identity', width = 3) +
  coord_polar("y", start = 0) +
  facet_wrap(~housing, scale = 'free') +
  geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
  labs(
    title = "Credit Class Distribution by Housing Category (Under Age 34)",
    subtitle = "Proportion of Credit Classes for Different Housing Groups",
    x = NULL,  # No x-axis needed
    y = NULL,  # No y-axis needed
    fill = "Credit Class",  # Legend title
  )



# Analysis 2-3------------------------------------------------------------------------------------------
# How does age and credit amount influence the prediction of credit class across different models?

# set seed so that every time it will be the same random sample
set.seed(123)
# change class into 0 and 1, bad and good respectively
data$class = factor(df2$class, levels = c("bad", "good"), labels = c("0","1"))

# split data set into training and test sets in a 80:20 ratio
split = sample.split(data$class, SplitRatio = 0.8)
training_set = subset(data, split == T)
test_set = subset(data, split == F)

# (a) Logistic Regression
# train the model using training set with age and credit amount
LogRegmodel = glm(class ~age+credit_amount,training_set, family = binomial)
summary(LogRegmodel)

# predict on the test set using the trained model and fit the model
pred_prob_test = predict(LogRegmodel, type = 'response', test_set[,-21])
pred_class_test = ifelse(pred_prob_test >0.5, 1,0)

# use confusion matrix to check accuracy, sensitivity and specificity
confusionMatrix(table(pred_class_test, test_set$class))

# make a variable to store the values of sensitivity and
# specificity using roc function
roc_curve <- roc(test_set$class, pred_prob_test)

# (b) Random Forest
# train the model using training set with age and credit amount
RFModel = randomForest(class~age+credit_amount,training_set,ntree=200 )

# predict on the test set using the trained model
# extract probability of good classes to be used for roc
y_prob_rf = predict(RFModel, newdata = test_set[,-21], type = 'prob')[,2]
# predict and store 0s and 1s into variable to be used for comparison
y_pred_rf = predict(RFModel, newdata = test_set[,-21])

# store predictions in a new column in test set for future use
test_set$predicted_class = y_pred_rf

# use confusion matrix to check accuracy, sensitivity and specificity
confusionMatrix(table(y_pred_rf, test_set$class))

# make a variable to store the values of sensitivity and
# specificity using roc function
roc_curve1 <- roc(test_set$class, y_prob_rf)


# (c) ROC Curve
# Create data frames for both models
# to store sensitivities (true positive rate) and
# 1-specificities (false positive rate)
dfRoC <- data.frame(
  FPR = 1 - roc_curve$specificities,
  TPR = roc_curve$sensitivities,
  Model = "LogReg Model"
)
dfRoC1 <- data.frame(
  FPR = 1 - roc_curve1$specificities,
  TPR = roc_curve1$sensitivities,
  Model = "RF Model"
)

# Combine data into one frame for plotting
roc_data <- rbind(dfRoC, dfRoC1)

# Compute AUC values (area under curve)
aucLR <- auc(roc_curve)
aucRF <- auc(roc_curve1)



# Plot ROC curves
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  # 50% random guessing line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray", size = 1) +
  labs(
    title = "ROC Curve",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  scale_color_manual(
    values = c("LogReg Model" = "blue", "RF Model" = "red"),
    labels = c(
      paste0("LogReg Model (AUC = ", round(auc1, 2), ")"),
      paste0("RF Model (AUC = ", round(auc2, 2), ")")
    )
  )


# Analysis 2-4-------------------------------------------------------------------------------------
# How can the credit class be improved by targeting specific age groups and credit amount?

# Plot faceted bar plot
ggplot(data, aes(x = credit_amount, fill = class)) +
  geom_histogram(position = 'dodge') +
  facet_wrap(~age_group, scale = 'free') +
  labs(
    title = "Distribution of Credit Amount by Age Group and Credit Class",
    x = "Credit Amount",
    y = "Frequency",
    fill = "Credit Class",
    subtitle = "Separated by Age Group"
  )


# limit credit amount to 5000 for all age group to random amount between 1000-5000
data_adjusted <- data %>%
  mutate(credit_amount = ifelse(credit_amount > 5000,
                                sample(1000:5000, size = sum(data$credit_amount > 5000), replace = TRUE),
                                credit_amount))

# plot the faceted bar plot after limiting
ggplot(data_adjusted, aes(x = credit_amount, fill = class)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~age_group, scale = "free") +
  labs(title = "Credit Amount Distribution After Capping",
       x = "Credit Amount", y = "Frequency")

# Random Forest
# perform random forest and store the predicted values in another variable
y_pred_rf1 = predict(RFModel, newdata = test_set1[,-21])
test_set1$predicted_class = y_pred_rf1

# compare counts of class before and after limiting
# before limiting
test_set%>%
  group_by(age_group,predicted_class) %>%
  summarise(count = n())

# after limiting
test_set1 %>%
  group_by(age_group, predicted_class) %>%
  summarise(count = n())

