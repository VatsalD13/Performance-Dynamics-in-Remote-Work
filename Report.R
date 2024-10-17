#### Satisfaction_homework
rm(list=ls())

#### load packages
library(haven)
library(stargazer)D
library(Hmisc)
library(ggplot2)
library(lattice)

#### load data
perform13 <- read_dta("C:/Users/doshi/Downloads/Working from home-20240512/Performance.dta")
p13<-data.frame(perform13)

#### clean data
c13 <- na.omit(p13)
View(c13)
summary(c13)

stargazer(c13,type="text",align=TRUE)

### Marital Status
ggplot(c13, aes(x = factor(married), fill = factor(married))) +
  geom_bar() +
  labs(title = "Marital Status", x = "Status", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Single", "Married")) +
  theme_minimal()

### Parental Status
ggplot(c13, aes(x = factor(children), fill = factor(children))) +
  geom_bar() +
  labs(title = "Parental Status", x = "Status", y = "Count") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"),
                    labels = c("No child", "Have a child")) +
  theme_minimal()

# Are married employees performing better WFH? 
model1 <- lm(data=c13, perform1~experiment_treatment+personid) 
model2 <- lm(data=c13, perform1~experiment_treatment+married+personid) 
model3 <- lm(data=c13, perform1~experiment_treatment*married+personid) 

stargazer(model1, model2, model3 , type="text", align=TRUE,omit=c("personid")) 

# Are employees with children performing better? 
model4 <- lm(data=c13,perform1~experiment_treatment+children+personid) 
model5 <- lm(data=c13,perform1~experiment_treatment*children+personid) 

stargazer(model4, model5, type="text", align=TRUE,omit=c("personid")) 

# Impact of WFH on married employee having children? 
model6 <- lm(data=c13,perform1~experiment_treatment*children*married+personid) 

stargazer(model6 , type="text",align=TRUE,omit=c("personid","year_weekF"))



