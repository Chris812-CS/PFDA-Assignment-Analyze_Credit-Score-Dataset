# NAVIIN RAJ A/L THEIVARAJAN, TP070404
# Objective 3: To investigate the relationship between loan duration and credit class. 

###########################################################################################

library(DataExplorer)
library(ggplot2) # data visualization
library(VIM) # for missing values
library(e1071) # machine learning
library(caret) # machine learning
library(caTools) # machine learning
library(randomForest) # random forest model
library(dplyr) # data transformation
library(RColorBrewer) # color palette
library(moments) # skewness
library(pROC) # ROC
library(car) # VIF

# make numbers readable
options(scipen = 999)

# load dataset into data frame
df = read.csv("C:\\Users\\User\\Documents\\Degree\\Computer Science (DA)- Year 2\\Computer Science (DA)- Year 2 SEM 1\\PFDA\\Assignment\\DataCleaningUpdatedCSV.csv", stringsAsFactors = TRUE)
df
summary(df)


###########################################################################################

# Analysis 3-1
# What is the distribution of loan duration, and the outliers present in each credit class? 



# average loan duration and standard deviation for each class
df%>%
  select(class, duration) %>%
  group_by(class) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration))


# detecting outliers of duration in both credit class + distribution
ggplot(df, aes(x = class,y = duration, fill = class)) +
  geom_violin() +
  geom_boxplot(width= 0.1,position = "dodge") +
  scale_fill_brewer(palette = "Pastel2")+
  labs(title = "Violin + Box plot of duration of each credit class")



# divide duration term into two categories:
max(df$duration)
# max duration = 60 months

# short = duration < 20 months
# moderate = 20 <= duration < 40
# long = duration >= 40 months
new_df = df %>%
  mutate(loan_duration= case_when(duration <20 ~ "short",
                                  duration >=40 ~ "long", 
                                  duration >= 20 & duration <40 ~ "moderate"))


new_df %>%
  select(duration, loan_duration)
# duration distribution
ggplot(new_df, aes(x=loan_duration, fill = loan_duration))+
  geom_bar() +
  scale_fill_brewer(palette = "Accent") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust=-0.4)
  


# statistics of duration in category
stats_duration = new_df %>%
  select(class, loan_duration, duration) %>%
  group_by(class, loan_duration) %>%
  summarise(total = n()) %>%
  mutate(percentage = total/sum(total) * 100) %>%
  arrange(class, desc(loan_duration))

# view percentage of long and short terms in each class
stats_duration


# visualization (pie chart)
stats_duration %>%
  ggplot(aes(x = "", y = total, fill= loan_duration)) +
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y", start = 0) +
  facet_wrap(~class, scales = "free") +
  geom_text(aes(label = paste(total,"\n","(",round(percentage,2)," %",")",sep = "")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3.5) +
  ggtitle("Percentage of loan duration in each credit class") +
  scale_fill_brewer(palette = "Set2")



## talk about short to long term duration ratio in different credit class
## talk about outliers, how data is skewed
###########################################################################################

# Analysis 3-2
# Does loan duration significantly impact the credit class of a customers?



# compare the mean of duration in two different class
# null hypothesis: the mean of the duration is equal across the two class
# alternative hypothesis: the mean of the duration is different across the two class
t.test(duration~class, data = new_df)

# reject null hypothesis since p-value < 0.05


# null hypothesis: there is no relationship between loan duration and credit class
# alternative hypothesis: there is relationship between loan duration and credit class
contigency_table = table(new_df$loan_duration, new_df$class)
chisq.test(contigency_table)

# since p-value is less than 0.5, can reject the null hypothesis




# simple logistic regression 
levels(new_df$class) = c(0,1)
classifier = glm(class ~ duration,
                 data = new_df,
                 family = "binomial")


summary(classifier)
# talk about coefficients 

# rearrange categories
new_df$loan_duration = factor(new_df$loan_duration, levels = c("short", "moderate", "long"))

# statistics of duration in category
stats_duration = new_df %>%
  select(class, loan_duration, duration) %>%
  group_by(class, loan_duration) %>%
  summarise(total = n()) %>%
  mutate(percentage = total/sum(total) * 100) %>%
  arrange(class, desc(loan_duration))

# visualization using bar chart
ggplot(stats_duration,aes(loan_duration, total, fill = class)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Distribution of Loan Durations by Class (Good vs. Bad)")
# make inference based on the plot



###########################################################################################

# Analysis 3-3
# Can loan duration alone accurately predict the outcome of credit class of customers?

set.seed(123)
# split train test data
split = sample.split(new_df$class, SplitRatio = 0.8)
train_set = subset(new_df, split == TRUE)
test_set = subset(new_df, split == FALSE)

# creating a simple logistic regression model
glmclassifier1 = glm(class~duration,data= train_set, family = binomial)

# make predictions using model 1
y_pred_probs1 = predict(glmclassifier1, test_set[-21], type = "response")
y_pred1 = ifelse(y_pred_probs1 > 0.5, 1, 0)

summary(glmclassifier1)

  
# compute confusion matrix for model 1
confusionMatrix(table(y_pred1, test_set$class))
# accuracy is only 0.5855 using one predictor
# not enough to accurately predict class

# check other variables that contributes significantly and use it together in prediction
rfclassifier = randomForest(x = train_set[-21], y = train_set$class, ntree = 250)

varImpPlot(rfclassifier)


# make model with additional features with high contributions in rf model
glmclassifier2 = glm(class ~ duration + checking_status + purpose + credit_amount,
                     data = train_set, family = binomial)

summary(glmclassifier2)

# checking the variance inflation factor to ensure 
# there are no serious multicollinearity
vif(glmclassifier2)

# make predictions using model 2
y_pred_probs2 = predict(glmclassifier2, test_set[-21],type = "response")
y_pred2 = ifelse(y_pred_probs2 > 0.5, 1, 0)

# compute confusion matrix for model 2
confusionMatrix(table(y_pred2, test_set$class))


# Specificity =  TN/(TN+FP)
# Sensitivity = TP/(TP/FN)
# prediction is the column on left
# actual is row on top


# check roc-auc curve and compare models
roc1 = roc(test_set$class,y_pred_probs1) # simple logistic regression
roc2 = roc(test_set$class,y_pred_probs2) # multiple variable logistic regression

# Calculating auc values for both model
cat("AUC for Model 1:",auc(roc1))
cat("AUC for Model 2:",auc(roc2))

# preparing for roc curve for model 1
roc1plot = data.frame(specificities = roc1$specificities, 
                  sensitivities = roc1$sensitivities, 
                  thresholds = roc1$thresholds)

# preparing for roc curve for model 2
roc2plot = data.frame(specificities = roc2$specificities, 
                  sensitivities = roc2$sensitivities, 
                  thresholds = roc2$thresholds)

# compare roc curve of both models
# add diagonal line for 0.5
ggplot()+
  geom_smooth(data = roc1plot, aes(x = specificities, y = sensitivities),
              col = "dodgerblue",alpha = 0.1)+
  geom_smooth(data = roc2plot, aes(x = specificities, y = sensitivities), 
              col = "deeppink1",alpha = 0.1)+
  geom_line(data = roc1plot, aes(x = specificities, y = sensitivities), 
            col = "blue")+
  geom_line(data = roc2plot, aes(x = specificities, y = sensitivities), 
            col = "red")+
  geom_text(aes(x = 0.35, y= 0.70, label = "Model 1", col= "red"))+
  geom_text(aes(x = 0.75, y= 0.9, label = "Model 2", col= "darkblue"))+
  scale_x_reverse()+
  geom_abline(slope = 1, intercept = 1.0, linetype = "dashed", col = "purple")
  labs(title = "Roc Curve")







###########################################################################################

# Analysis 3-4
# What is the recommended duration of loan for customers to lessen the risk of
# being classified as “bad” while considering other factors?

# make test data sets for combination of different data 
# mean for each loan duration
# short = (0+19)/2 = 9.5
# moderate = (20+39)/2 = 29.5
# long = (40+60)/2 = 50

# credit amount max = 15945, min = 1000
# 14945/3 categories: 
# short = 1000 - 5981 , mean = 3480.5
# moderate = 5982 - 10963 , mean = 8472.5
# long = 10963 - 15945, mean = 13 454

# generate new dataset with different combinations of data
test_data_prescriptive = expand.grid(duration = c(9.5,29.5,50),
                                     credit_amount = c(3480.5, 8472.5,13454),
                                     purpose = unique(new_df$purpose),
                                     checking_status = unique(new_df$checking_status))

# make predictions on credit class outcome using new dataset using model 2
prescriptive_pred_probs = predict(glmclassifier2, test_data_prescriptive, type = "response")

# change the probability into 1s and 0s
prescriptive_pred = ifelse(prescriptive_pred_probs>0.5,1,0)

# bind the probabilities and class outcome to original dataset
test_data_prescriptive = cbind(test_data_prescriptive, 
                               class = prescriptive_pred, 
                               prob = prescriptive_pred_probs)

# make relevant information as factor for easy plotting
test_data_prescriptive$credit_amount = factor(test_data_prescriptive$credit_amount)
test_data_prescriptive$duration = factor(test_data_prescriptive$duration)
test_data_prescriptive$class = factor(test_data_prescriptive$class)

# show the final dataset with predicted outcome
head(test_data_prescriptive)

# plot the most risky and most safest outcome using faceted grid and dodged bar chart
test_data_prescriptive %>%
  filter(prob > 0.95 | prob < 0.05)%>%
  ggplot(aes(checking_status, fill = duration))+
  geom_bar(position = "dodge")+
  facet_grid(purpose~class)




###########################################################################################


