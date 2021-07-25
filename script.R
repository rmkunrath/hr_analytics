# loading libraries and ensuring repetibility

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(mgcv)) install.packages("mgcv", repos = "http://cran.us.r-project.org")

set.seed(1, sample.kind="Rounding")

# IMPORTING THE DATA

dt <- read.csv("data/dataset.csv", na.strings="")

# EXPLORING THE DATA

## Understanding the data

# 14 Variables.
head(dt)
summary(dt)

# 19158 entries
nrow(dt)

# 25% of the candidates wish to change jobs
dt %>% summarise(total_values = mean(target))
dt %>% ggplot(aes(target)) + geom_bar()

mu_target <- mean(dt$target)

# gender X target
# Seeing through this perspective the only inclination is that NAs tend to leave. Can man be avoiding to explicit their gender?
dt %>% ggplot(aes(x= gender,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent)

# company_size X target
dt %>% ggplot(aes(x= company_size,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="company_size") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Company Size") 

# company_type X target
dt %>%
  ggplot(aes(x= company_type,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="company_type") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Company Type") 

# training_hours*10 X target
dt %>% ggplot(aes(x= as.integer(training_hours/20),  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="Training hours * 20") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Training hours / 20") 

# education_level X target
dt %>%
  ggplot(aes(x= education_level,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="education_level") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Education level") 

# relevent_experience X target
dt %>%
  ggplot(aes(x= relevent_experience,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="relevent_experience") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Relevant experience") 

# experience X target
dt %>%
  mutate(experience = as.numeric(experience)) %>%
  ggplot(aes(x= experience,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="experience") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Experience") 

# last_new_job X target
dt %>%
  ggplot(aes(x= last_new_job,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Last new job") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Last new job") 

# enrolled_university X target
dt %>%
  ggplot(aes(x= enrolled_university,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="enrolled_university") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Enrolled University") 

# major_discipline X target
dt %>%
  ggplot(aes(x= major_discipline,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="major_discipline") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Major Discipline") 

# city_development_index*10 X target
dt %>% ggplot(aes(x= as.integer(city_development_index*10),  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="city_development_index hours * 10") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("City DI * 10") 

# city X target
dt %>%
  group_by(city) %>%
  summarise(mean_target = mean(target), entries = sum(city != 0), IDH = mean(city_development_index)) %>%
  filter(mean_target > mu_target & entries > 50) %>%
  arrange(desc(mean_target))

rm(mu_target)

# Conclusions
# Factors that increase employees' wish to leave
# lower development index cities
# have no relevant experience
# are graduates
# full time course
# do not state the company size
# do not state gender
# do not state company type

# WRANGLING

# Values considered NA can be useful
# As the dataset have been loaded with base r, the values have already been imported as factors.
# With the objective of using NA values for prediction, the dataset is going to be loaded again with readr
# and the NA values are going to be categorized as 'ND'

# Also, the levels are going to be ordered with the insight data exploration gave.

dt <- read_csv("data/dataset.csv")
dt[is.na(dt)] <- "ND"

summary(dt)

dt$gender <- factor(dt$gender)

dt$relevent_experience <- factor(dt$relevent_experience) %>% ordered()

dt$enrolled_university <- factor(dt$enrolled_university, 
                                 levels = c("ND", "Full time course", "Part time course",
                                            "no_enrollment")) %>% ordered()

dt$education_level <- factor(dt$education_level,
                             levels = c("Graduate","Masters","High School",
                                        "Phd", "Primary School", "ND")) %>% ordered()

dt$major_discipline <- factor(dt$major_discipline) %>% ordered()

dt[dt == ">20"] <- "21"
dt[dt == "<1"] <- "0"
dt$experience <- as.numeric(dt$experience)

dt$company_size <- factor(dt$company_size,
                          levels = c("ND", "<10", "10/49", "50-99", "100-500", "500-999", "1000-4999",
                                     "5000-9999", "10000+")) %>% ordered()

# company_size X target
dt %>% 
  filter(company_size != "ND") %>%
  ggplot(aes(x= company_size,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="company_size") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Company Size") 

# From data exploration, it seems that employees from Pvt Ltd moved to ND. That's the reason for reordering
dt$company_type <- factor(dt$company_type,
                          levels = c("ND", "Pvt Ltd", "Public Sector", "Other",
                                     "NGO", "Early Stage Startup", "Funded Startup"))

dt[dt == ">4"] <- "5"
dt[dt == "never"] <- "0"
dt$last_new_job <- as.numeric(dt$last_new_job)

# last_new_job X target
dt %>%
  ggplot(aes(x= last_new_job,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Last new job") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Last new job") 

# dt$target <- factor(dt$target)

summary(dt)

# removing NAs
dt <- na.omit(dt)

# SEPARATING IN TRAIN AND TEST

# Leaving 15% of the data to test and creating test and train set
test_index <- createDataPartition(y = dt$target, times = 1,
                                  p = 0.15, list = FALSE)
train_set <- dt %>% slice(-test_index)
test_set <- dt %>% slice(test_index)

rm(test_index)

# CREATING EVALUATION METHOD

# Just guessing the value.
# Determining a mean rate and predict it
p <- mean(as.numeric(train_set$target))
p

n <- nrow(test_set)

y_hat <- sample(c(1, 0), n, replace = TRUE, prob = c(p,1-p))

# Doesn't have a good specificity, in other words the proportion of negatives is low and the model doesn't predict well when the employee wishes to stay.
cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))

results <- data_frame(method = "Just guessing", 
                      Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                      Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                      Specificity = round(as.double(cm$byClass["Specificity"]),3),
                      Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
                      F1 = round(as.double(cm$byClass["F1"]),3))
results



# MODELING

# Linear regression

lm_fit <- train_set %>%
  lm(target ~ company_size + company_type + education_level + 
       relevent_experience + experience + enrolled_university +
       city_development_index, data=.)

y_hat_linear_numeric <- predict(lm_fit, newdata = test_set, type = "response")

y_hat <- ifelse(y_hat_linear_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))

results <- bind_rows(results, data_frame(method="Linear regression", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results


# GAM fit

gam_fit <- train_set %>%
  gam(target ~ company_size + company_type + education_level + 
        relevent_experience + experience + enrolled_university +
        city_development_index, data=.)

y_hat_gam_numeric <- predict(gam_fit, newdata = test_set)

y_hat <- ifelse(y_hat_gam_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))

results <- bind_rows(results, data_frame(method="GAM Smooth", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results

# Logistic regression

fit_glm <- train(target ~ . , "glm", data = train_set)

y_hat_glm_numeric <- predict(fit_glm, newdata = test_set)

y_hat <- ifelse(y_hat_glm_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))

results <- bind_rows(results, data_frame(method="Logistic Regression", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results

# Random Forest

dt %>% ggplot(aes(city_development_index, company_size, color = target)) +
   geom_point(alpha = 0.2)

fit_rf <- train(target ~  city_development_index +
                  company_size, "Rborist", data = train_set)

y_hat_rf_numeric <- predict(fit_rf, newdata = test_set)

y_hat <- ifelse(y_hat_rf_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))

results <- bind_rows(results, data_frame(method="Random Forest", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results

# # Several models
# 
# models <- c("glm", "lda", "naive_bayes", "svmLinear", "gamboost",
#             "gamLoess", "qda", "knn", "kknn", "loclda", "gam", "rf",
#             "ranger","wsrf", "Rborist", "avNNet", "mlp", "monmlp", "gbm",
#             "adaboost", "svmRadial", "svmRadialCost", "svmRadialSigma")
# 
# 
# fit_gen <- train(target ~  experience + city_development_index, "Rborist", data = train_set)
# 
# y_hat_gen_numeric <- predict(fit_gen, newdata = test_set)
# 
# y_hat <- ifelse(y_hat_gen_numeric > 0.5, 1, 0)
# 
# cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set$target))
# 
# results <- bind_rows(results, data_frame(method="Several models", 
#                                          Accuracy = round(as.double(cm$overall["Accuracy"]),3),
#                                          Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
#                                          Specificity = round(as.double(cm$byClass["Specificity"]),3),
#                                          Prevalence = round(as.double(cm$byClass["Prevalence"]),3),
#                                          F1 = round(as.double(cm$byClass["F1"]),3)))
# results

# EVALUATING THE MODEL