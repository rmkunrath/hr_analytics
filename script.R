# title: 'HR Analytics: Job Change of Data Scientists'
# author: "Rodrigo Moraes Kunrath"

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
dt %>% summarise("Target mean rate" = mean(target))  %>% knitr::kable()
dt %>% ggplot(aes(target)) + geom_bar() +
  xlab("Target") + ylab("Total entries")

mu_target <- mean(dt$target)

# gender X target
# Seeing through this perspective the only inclination is that NAs tend to leave. Can man be avoiding to explicit their gender?
dt %>% ggplot(aes(x= gender,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender")

# company_size X target
dt %>% ggplot(aes(x= company_size,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="company_size") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Company Size") +
  theme(axis.text.x = element_text(angle = 90))

# company_type X target
dt %>%
  ggplot(aes(x= company_type,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="company_type") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Company Type") +
  theme(axis.text.x = element_text(angle = 90))

# training_hours*10 X target
dt %>% ggplot(aes(x= as.integer(training_hours/20),  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="Training hours * 20") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Training hours / 20") 


# relevent_experience X target
dt %>%
  ggplot(aes(x= relevent_experience,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="relevent_experience") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Relevant experience") +
  theme(axis.text.x = element_text(angle = 90))

# experience X target
dt %>%
  mutate(experience = as.numeric(experience)) %>%
  ggplot(aes(x= experience,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
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

# education_level X target
dt %>%
  ggplot(aes(x= education_level,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="education_level") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Education level")  +
  theme(axis.text.x = element_text(angle = 90))

# enrolled_university X target
dt %>%
  ggplot(aes(x= enrolled_university,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="enrolled_university") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Enrolled University")  +
  theme(axis.text.x = element_text(angle = 90))

# major_discipline X target
dt %>%
  ggplot(aes(x= major_discipline,  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="major_discipline") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Major Discipline")  +
  theme(axis.text.x = element_text(angle = 90))


# city_development_index*10 X target
dt %>% ggplot(aes(x= as.integer(city_development_index*10),  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  labs(y = "Percent", fill="city_development_index hours * 10") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("City DI * 10") 

# city X target
tab <- dt %>% 
  group_by(city) %>%
  summarise(mean_target = mean(target), entries = sum(city != 0), HDI = mean(city_development_index)) %>%
  filter(mean_target > mu_target & entries > 50) %>%
  arrange(desc(mean_target)) %>%
  head(10)

colnames(tab) <- c("City", "Mean target", "Entries count", "HDI")
tab %>%  knitr::kable()

rm(tab)

# can the city leave rate order be connected?
dt_city_rate <- dt %>% 
  group_by(city) %>% 
  summarise(city_target_rating = mean(target))

# Adding the column
dt <- left_join(dt, dt_city_rate, by = "city")

dt %>% ggplot(aes(city_target_rating, company_size, color = target)) +
  geom_violin()

dt %>% ggplot(aes(city_target_rating, company_type, color = target)) +
  geom_violin()

dt %>% ggplot(aes(city_target_rating, enrolled_university, color = target)) +
  geom_violin()

dt %>% ggplot(aes(x= as.integer(city_target_rating*10),  group=target)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="city_target_rating * 10") +
  facet_grid(~target) +
  scale_y_continuous(labels = scales::percent) +
  xlab("City Target Rate* 10") 


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

dt_city_rate <- dt %>% 
  group_by(city) %>% 
  summarise(city_target_rating = mean(target))

# Adding the column
dt <- left_join(dt, dt_city_rate, by = "city")

summary(dt)

# removing NAs and city factor
dt <- na.omit(dt)
dt$city <- NULL

# SEPARATING IN TRAIN AND TEST

# Leaving 10% of the data to test and creating test and train set
test_index <- createDataPartition(y = dt$target, times = 1,
                                  p = 0.10, list = FALSE)
train_set <- dt %>% slice(-test_index)
test_set_final <- dt %>% slice(test_index)

rm(test_index)

# SEPARATING THE TRAIN IN TWO

# Leaving 10% of the data to test and creating test and train set
test_index <- createDataPartition(y = train_set$target, times = 1,
                                  p = 0.10, list = FALSE)
train_set_model <- train_set %>% slice(-test_index)
test_set_development <- train_set %>% slice(test_index)

rm(test_index)

# CREATING EVALUATION METHOD

# Just guessing the value.
# Determining a mean rate and predict it
p <- mean(as.numeric(train_set_model$target))
p

n <- nrow(test_set_development)

y_hat <- sample(c(1, 0), n, replace = TRUE, prob = c(p,1-p))

# Doesn't have a good specificity, in other words the proportion of negatives is low and the model doesn't predict well when the employee wishes to stay.
cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- data_frame(Method = "Guessing target rate", 
                      Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                      Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                      Specificity = round(as.double(cm$byClass["Specificity"]),3),
                      F1 = round(as.double(cm$byClass["F1"]),3))

results %>% knitr::kable()



# MODELING

# Linear regression

lm_fit1 <- train_set_model %>%
  lm(target ~ company_size + company_type + education_level + 
       relevent_experience + experience + enrolled_university +
       city_development_index, data=.)

y_hat_linear_numeric <- predict(lm_fit1, newdata = test_set_development, type = "response")

y_hat <- ifelse(y_hat_linear_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Linear regression", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

# Linear regression

lm_fit2 <- train_set_model %>%
  lm(target ~ . , data=.)


y_hat_linear_numeric <- predict(lm_fit2, newdata = test_set_development, type = "response")

y_hat <- ifelse(y_hat_linear_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Complete Linear regression", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

# GAM fit

gam_fit <- train_set_model %>%
  gam(target ~ company_size + company_type + education_level + 
        relevent_experience + experience + enrolled_university +
        city_development_index + city_target_rating, data=.)

y_hat_gam_numeric <- predict(gam_fit, newdata = test_set_development)

y_hat <- ifelse(y_hat_gam_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="GAM Smooth", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

# Logistic regression

fit_glm <- train(target ~ . , "glm", data = train_set_model)

y_hat_glm_numeric <- predict(fit_glm, newdata = test_set_development)

y_hat <- ifelse(y_hat_glm_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Logistic Regression", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

# Random Forest
# 4 models crossing city_target_rating, city_development_index and company_size

train_set_model %>% ggplot(aes(city_development_index, company_size, color = target)) +
   geom_point(alpha = 0.2)
train_set_model %>% ggplot(aes(city_target_rating, company_size, color = target)) +
  geom_point(alpha = 0.2)
train_set_model %>% ggplot(aes(city_development_index, city_target_rating, color = target)) +
  geom_point(alpha = 0.2)


fit_rf1 <- train(target ~  city_development_index +
                  company_size, "Rborist", data = train_set_model)

y_hat_rf_numeric <- predict(fit_rf1, newdata = test_set_development)

y_hat <- ifelse(y_hat_rf_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Random Forest: Company Size X City DI", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()



fit_rf2 <- train(target ~  city_target_rating +
                  company_size, "Rborist", data = train_set_model)

y_hat_rf_numeric <- predict(fit_rf2, newdata = test_set_development)

y_hat <- ifelse(y_hat_rf_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Random Forest: City Target Rating x Company Size", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

fit_rf3 <- train(target ~  city_target_rating +
                   city_development_index , "Rborist", data = train_set_model)

y_hat_rf_numeric <- predict(fit_rf3, newdata = test_set_development)

y_hat <- ifelse(y_hat_rf_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Random Forest: City Target Rating x City DI", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

fit_rf4 <- train(target ~  city_target_rating +
                   city_development_index + company_size, "Rborist", data = train_set_model)

y_hat_rf_numeric <- predict(fit_rf4, newdata = test_set_development)

y_hat <- ifelse(y_hat_rf_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_development$target))

results <- bind_rows(results, data_frame(Method="Random Forest: City Target Rating x City DI x Company Size", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))
results %>% knitr::kable()

# EVALUATING THE MODEL
# rf1 chosen

y_hat_numeric <- predict(fit_rf1, newdata = test_set_final)

y_hat <- ifelse(y_hat_numeric > 0.5, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_final$target))

results_final <- data_frame(Method = "Random Forest: City Target Rating x Company Size", 
                      Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                      Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                      Specificity = round(as.double(cm$byClass["Specificity"]),3),
                      F1 = round(as.double(cm$byClass["F1"]),3))

results_final %>% knitr::kable()


# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c(1, 0), n, replace = TRUE, prob=c(p, 1-p))
  y_hat <- factor(y_hat)
  list(method = "Guessing target rate",
       FPR = 1 - specificity(y_hat, factor(test_set_final$target)),
       TPR = sensitivity(y_hat, factor(test_set_final$target)))
})
# guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- seq(0, 1, length.out = 20)
y_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(y_hat_numeric > x, 1, 0)
  y_hat <- factor(y_hat)
  list(method = "Random Forest",
       FPR = 1-specificity(y_hat, factor(test_set_final$target)),
       TPR = sensitivity(y_hat, factor(test_set_final$target)))
})

# plot both curves together
bind_rows(guessing, y_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

# 0.4 cutoff proposed
y_hat <- ifelse(y_hat_numeric > 0.4, 1, 0)

cm <- confusionMatrix(data = factor(y_hat), reference = factor(test_set_final$target))

results_final <- bind_rows(results_final, data_frame(Method= "Random Forest with a 0.4 cutoff", 
                                         Accuracy = round(as.double(cm$overall["Accuracy"]),3),
                                         Sensitivity = round(as.double(cm$byClass["Sensitivity"]),3),
                                         Specificity = round(as.double(cm$byClass["Specificity"]),3),
                                         F1 = round(as.double(cm$byClass["F1"]),3)))                           
                           
results_final %>% knitr::kable()
