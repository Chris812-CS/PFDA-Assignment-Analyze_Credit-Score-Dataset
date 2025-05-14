library(DataExplorer)
library(ggplot2)
library(VIM)
library(caret)
library(caTools)
library(randomForest)
library(dplyr)

#For Chi-squared test
library(MASS)

url = "C:\\Users\\Owi Sze Jay\\Downloads\\DataCleaningUpdatedCSV.csv"

df = read.csv(url)
options(scipen = 999)

#View(df)
summary(df)
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
df = convert_to_types(df)

summary(df)




#random forest
classifier = randomForest(x = training_set[,-21], y = training_set$class, ntree = 500,)

#Descriptive
#Distribution of the employment variable in the dataset

#Change the order of the levels
df$employment = factor(df$employment, levels = c("unemployed","<1","1<=X<4","4<=X<7",">=7"))

#Bar Chart to show the frequency of each employment
ggplot(df, aes(employment, fill=employment))+ #employment as the x axis
  geom_bar() + #generate bar plot
  geom_text(aes(label = ..count..), 
            stat = "count",vjust = 1.5, color="white") + #add label of frequency
  ggtitle(label = "Bar Plot of Frequency for each Employment Group")+ #add title
  xlab("Employment Years")+ #add x-axis label
  ylab("Frequency") #add y-axis label
  

#Bar Chart to show the frequency of each employment
ggplot(df, aes(x = class, fill = employment))+
  geom_bar()+
  facet_wrap(df$employment, scales = "free")

# Frequency of the employment group of good versus bad class
# Group Bar Plot
ggplot(df, aes(employment, fill = class))+
  geom_bar(position = "dodge")+ #make the bar chart into groups
  geom_text(aes(label = ..count..), stat= "count", 
            vjust = 1.5, position = position_dodge(width = .9), 
            color = "white")+ #add label for the frequency
  scale_fill_brewer(palette = "Set1")+ #change the colour of the bar
  ggtitle(label = "Grouped Bar Plot of Employment Group for good versus bad class")+ #add title
  xlab("Employment Years")+ #add x-axis label
  ylab("Frequency") #add y-axis label
  
  

ggplot(df, aes(x = age, fill = class))+
  geom_histogram()+
  facet_wrap(df$employment, scales= "free")

ggplot(df, aes(x = employment, y = credit_amount))+
  geom_violin()


emp_lt4 = df %>% filter(employment == "<1" | employment == "1<=X<4") %>%
  mutate(employment = "<4") %>%
  select(employment, class)

emp_mt4 = df %>% filter(employment == "4<=X<7" | employment == ">=7") %>%
  mutate(employment = ">=4") %>%
  select(employment, class)


emp_unp = df %>% filter(employment == "unemployed")%>%
  select(employment, class)

new_df = rbind(emp_lt4,emp_mt4,emp_unp)


#Count the frequency grouped by employment and class
loan_class_summary <- new_df %>%
  group_by(employment, class) %>%
  summarise(count = n()) 

#Build a labeller to label the facet wrap
employment_label = c("<4" = "Less Than 4 Years", 
                     ">=4" = "More Than Equals to 4 Years",
                     "unemployed" = "Unemployed")

employment_labeller = function(variable, value){
  return(employment_label[value])
}

#Calculate the percentage for each credit class and employment
loan_class_percentage = loan_class_summary %>%
  group_by(employment) %>%
  mutate(percentage = count/sum(count) *100)

#Extra Feature 1
#Pie chart to show compare the percentage of good and bad credit class
#for each employment groups

#Construct a pie chart that consist of different employment period
ggplot(loan_class_percentage, aes(x ="", y = percentage, fill = class))+
  geom_bar(stat = "identity", width = 1)+ 
  facet_wrap(~employment, labeller = employment_labeller)+
  coord_polar("y", start = 0)+
  theme_void()+
  ggtitle("Pie Chart of good and bad class for each employment group")+
  geom_text(aes(label = paste(round(percentage,2),"%"))
            ,position = position_stack(vjust=0.5))

#################################################################################################
#Split train-test set
set.seed(123)
split = sample.split(df$class, SplitRatio = 0.8)

training_set = subset(df, split == T)
test_set = subset(df, split == F)

#check the number of rows for each dataset
dim(df)#5997 rows
dim(training_set)#4798 rows
dim(test_set)#1199 rows

#build logistic regression
classifier = glm(class~employment+credit_amount, training_set, family = binomial)
#use only "employment" to build the reg
summary(classifier)

#predict on test set
pred_prob_test = predict(classifier, type = "response", test_set[,-21])
pred_class_test = ifelse(pred_prob_test > 0.5, "good","bad")

confusion_matrix =confusionMatrix((table(pred_class_test, test_set$class)), positive = "good")
confusion_matrix

#Random Forest
#Build Random Forest modelling
rf_classifier = randomForest(class~employment+credit_amount, training_set, ntree = 500)

#Prediction on test set
y_pred_rf = predict(rf_classifier, newdata = test_set[-21])
y_prob_rf = predict(rf_classifier, newdata = test_set[-21], type = "prob")[,2]
confusion_matrix_rf = confusionMatrix(table(y_pred_rf, test_set$class), positive = "good")
confusion_matrix_rf #display confusion matrix

#ROC for Logistic Regression
roc_obj = roc(test_set$class, pred_prob_test)
roc_obj #AUC = 0.6182

#ROC for Random Forest
roc_obj_rf = roc(test_set$class, y_prob_rf)
roc_obj_rf #AUC = 0.7467

#Plot ROC curve for both model
roc_data = data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities),
  Model = "Logistic Regression"
)

roc_data_rf = data.frame(
  specificity = rev(roc_obj_rf$specificities),
  sensitivity = rev(roc_obj_rf$sensitivities),
  Model = "Random Forest"
)

roc_data_comb = rbind(roc_data,roc_data_rf)
roc_data_comb

ggplot(roc_data_comb, aes(x = 1 - specificity, y = sensitivity, color = Model))+
  geom_line()+
  geom_abline(linetype = "dashed", color = "blue")+ #Plot line for random guessing (0.5)
  ggtitle(paste0("ROC Curve"))+ #Title
  xlab(label = "False Positive Rate (1 - Specificity)")+ #x-axis label
  ylab(label = "True Positive Rate (Sensitivity)") #y-axis label


logistic_reg = glm(class~employment, df, family = binomial)
summary(logistic_reg)

#################################################################################################


summary(classifier)
summary(rf_classifier)








#Extra Feature
#ROC Curve and AUC
library(pROC)
#make a pred_class_test to numeric (1 and 0)
pred_class_test_num = ifelse(pred_class_test == 'good', 1,0)
#make test set class into numeric
test_set_num = test_set
test_set_num$class = ifelse(test_set$class == 'good',1,0)

#create roc object
roc_obj = roc(test_set$class,pred_prob_test)
roc_obj #display auc

#create dataframe for roc object
#contains specificity column and sensitivity column
roc_data = data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

#Build Random Forest modelling
rf_classifier = randomForest(x = training_set[-21], y = training_set$class, ntree = 500)

#Prediction on test set
y_pred_rf = predict(rf_classifier, newdata = test_set[-21], type = "prob")
y_pred_rf_num = ifelse(y_pred_rf == "good", 1,0)
confusion_matrix_rf = confusionMatrix(y_pred_rf, test_set$class, positive = "good")
confusion_matrix_rf #display confusion matrix

roc_obj_rf = roc(test_set$class,y_pred_rf)
roc_obj_rf #display AUC

roc_data_rf = data.frame(
  specificity = rev(roc_obj_rf$specificities),
  sensitivity = rev(roc_obj_rf$sensitivities)
)

roc_data = rbind(roc_data,roc_data_rf)
roc_data$model[1:3] = "Logistic Regression"
roc_data$model[4:6] = "Random Forest"

#Plot ROC Curve
ggplot(roc_data, aes(x = 1 - specificity , y = sensitivity, color = model))+
  geom_line()+
  geom_abline(linetype = "dashed", color = "blue")+ #Plot line for random guessing (0.5)
  ggtitle(paste0("ROC Curve"))+ #Title
  xlab(label = "False Positive Rate (1 - Specificity)")+ #x-axis label
  ylab(label = "True Positive Rate (Sensitivity)") #y-axis label



####################################
# Split train-test set
set.seed(123)
split = sample.split(df$class, SplitRatio = 0.7)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Check the number of rows for each dataset
dim(df) # 5997 rows
dim(training_set) # 4798 rows
dim(test_set) # 1199 rows

# Build logistic regression model
classifier = glm(class ~ employment, data = training_set, family = binomial)

# Convert "good" to 1 and "bad" to 0 for the training set class
training_set_new = training_set
training_set_new$class = ifelse(training_set_new$class == "good", 1, 0)

summary(classifier)

# Predict on the test set
pred_prob_test = predict(classifier, newdata = test_set[,-21], type = "response")
pred_class_test = ifelse(pred_prob_test > 0.5, "good", "bad")

# Confusion matrix for Logistic Regression
library(caret)
conf_matrix_log_reg = confusionMatrix(table(pred_class_test, test_set$class), positive = "good")
print(conf_matrix_log_reg)

# ROC Curve and AUC for Logistic Regression
library(pROC)
roc_obj = roc(test_set$class, pred_prob_test)
roc_obj # Display AUC

# Create dataframe for ROC object (contains specificity and sensitivity)
roc_data = data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# Build Random Forest model
library(randomForest)
rf_classifier = randomForest(x = training_set$employment, y = training_set$class, ntree = 500)

# Prediction on the test set
y_pred_rf = predict(rf_classifier, newdata = test_set[-21], type = "prob")
y_pred_rf_num = ifelse(y_pred_rf[,2] > 0.5, "good", "bad")

# Confusion matrix for Random Forest
conf_matrix_rf = confusionMatrix(y_pred_rf, test_set$class, positive = "good")
conf_matrix_rf

# ROC Curve and AUC for Random Forest
roc_obj_rf = roc(test_set$class, y_pred_rf[,2])
roc_obj_rf # Display AUC

# Create dataframe for ROC object (contains specificity and sensitivity for RF)
roc_data_rf = data.frame(
  specificity = rev(roc_obj_rf$specificities),
  sensitivity = rev(roc_obj_rf$sensitivities)
)

# Combine both ROC dataframes (Logistic Regression and Random Forest)
roc_data$model = c(rep("Logistic Regression", nrow(roc_data)),
                   rep("Random Forest", nrow(roc_data_rf)))

roc_data = rbind(roc_data, roc_data_rf)

# Plot ROC Curve
library(ggplot2)
ggplot(roc_data, aes(x = 1 - specificity , y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(linetype = "dashed", color = "blue") + # Line for random guessing (0.5)
  ggtitle("ROC Curve") + # Title
  xlab("False Positive Rate (1 - Specificity)") + # x-axis label
  ylab("True Positive Rate (Sensitivity)") # y-axis label
####################################
roc_obj
plot(roc_obj)
plot(roc_obj_rf)
  #Plot ROC Curve using ggplot2
ggroc(roc(test_set$class, pred_class_test_num)) +
  ggtitle(paste0("ROC Curve (AUC = ",
                 round(auc(test_set$class, pred_class_test_num),4),")"))+
  xlab(label = "False Positive Rate (1 - specificity)")+
  ylab(label = "True Positive Rate (sensitivity)")+
  geom_abline(intercept = 0, slope = 1, color = "green", linetype = "dashed", 
                          size = 1)



#Bar classifier#Bar Chart Regarding Employment and Housing
ggplot(df,aes(employment, fill = housing))+
  geom_bar()+ #Construct bar chart
  facet_wrap(~employment, scales = "free")+ #Facet Wrap employment
  ggtitle("Bar Chart of Employment and Housing Relationship") #add title


hi-squared test

#Mosaic plot
library("graphics")
mosaicplot(table, shade = TRUE,
           main = "Mosaic Plot of Employment versus Class")

#Extra Feature 2
#Chi-squared test
#Make employment and class into a table for c
table = table(df$employment, df$class)
#To test the significant for employment and class
chisq = chisq.test(table)
chisq


vec1 = c(130,132,138,136,131,153,131,133,129,132,133,110,132,129,134,135,135,134,136,133,133,133,132,130,131,134,135,130)
IQR(vec1)
quantile(vec1)
boxplot(vec1)


#Histogram of Employment and Credit Amount
ggplot(df, aes(x = credit_amount, fill = class))+
  geom_histogram(position = "dodge")+
  facet_wrap(~employment, scales = "free")

#Summary for Logistic Regression
summary(classifier)
