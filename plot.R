# Plotting fun
#install.packages('ggiraph')
#install.packages('ggiraphExtra')
#install.packages('plyr')

library(ggiraph)
library(ggiraphExtra)
library(plyr)


# Looking at Annie's blood sugar

fig <- diabetes_full %>%
  ggplot(aes(x = hour, y = blood_sugar, color = range)) +
  geom_point()

show(fig)

# Relationship between carb and insulin
fig2 <- diabetes_full %>%
  ggplot(aes(x = carbs, y = bolus_volume, color = range)) +
  geom_point() 

show(fig2)

# What scenario (Carb_No Carb) does Annie take Insulin?
fig3 <- diabetes_full %>%
  ggplot(aes(x = insulin_food)) +
  geom_bar() +
  facet_wrap(~ time_of_day)

show(fig3)

# How many meals a day
fig4 <- meals_per_day %>%
  ggplot(aes(x = meals_day)) +
  geom_bar()

show(fig4)

# Frequency ~ Blood sugar
fig5 <- meals_per_day %>% 
  group_by(meals_day, range) %>%
  summarise(test = sum(!is.na(range))) %>%
  ggplot(aes(x = meals_day, y = test, fill = range)) +
  geom_bar(stat = 'identity', width = .5, position = 'dodge')

show(fig5)

# MLR Predict bolus volume based on carb intake, time of day, basal amount
model1 <- lm(bolus_volume ~ carbs + time_of_day_coded + basal_amount, data = diabetes_full)
summary(model1)

# Predict carb intake based on hour of the day
model2 <- lm(carbs ~ time_of_day_coded, data = diabetes_full)
summary(model2)    

# Predict blood sugar level based on carb intake, time of day, basal amount
model3 <- lm(blood_sugar ~ carbs + time_of_day_coded + basal_amount + bolus_volume, data = diabetes_full)
summary(model3)  

# Logistic Regression for model 3
model4 <- lm(range_coded ~ carbs + time_of_day_coded + basal_amount + bolus_volume, data = diabetes_full)
summary(model4) 

# Hour ~ blood sugar
model5 <- lm(blood_sugar ~ hour, data = diabetes_full)
summary(model5)

# Hour ~ blood sugar
model6 <- lm(blood_sugar ~ hour + bolus_volume + carbs, data = diabetes_full)
summary(model6)

ggPredict(model1,se=TRUE,interactive=TRUE)