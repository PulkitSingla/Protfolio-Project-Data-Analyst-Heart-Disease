library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(glmnet)
library(plotmo)

heart <- read_csv("C:/Users/Lenovo/Downloads/heart.csv")
View(heart)
head(heart)
str(heart)

summary(heart)

heart_df <- heart %>% select(age, sex, cp, trestbps, chol, target)

heart %>% filter(is.na(age))

ggplot(heart_df,aes(x=age)) + geom_histogram(bins=50) 


heart_df$sex %>% table
heart_df$cp %>% table

ggplot(heart_df, aes(x=trestbps)) + geom_histogram(bins=50)

heart_df <- heart_df %>% mutate(log1_trestbps = log1p(trestbps)) %>% select(-trestbps)

ggplot(heart_df, aes(x=log1_trestbps)) + geom_histogram(bins=50)

ggplot(heart_df, aes(x=chol)) + geom_histogram(bins=50)

heart_df <- heart_df %>% mutate(log1_chol = log1p(chol)) %>% select(-chol)

ggplot(heart_df, aes(x=log1_chol)) + geom_histogram(bins=50)

heart_df$target %>% table

ggplot(heart_df, aes(x=age, fill=factor(target))) +
  geom_density(alpha=0.5) +
  geom_vline(xintercept=c(54, 70), color='red', linetype=2) + labs(title = "Target vs. age")

heart_df <- heart_df %>% mutate(
  AgeCategories = case_when(
    age < 54 ~ 'Adults',
    age >= 54 & age < 70 ~ 'Older Adults',
    age >= 70 ~ 'Elderly',
  )
)
heart_df$AgeCategories %>% table(useNA='always')

heart_df %>% mutate(
  sex = ifelse(sex==1, "Male", "Female")
)%>%ggplot(aes(x=sex, fill=factor(target))) +
  geom_bar(position='dodge') + labs(title = "Target vs. sex")

ggplot(heart_df, aes(x=cp, fill=factor(target))) +
  geom_bar(position='dodge') + labs(title = "Target vs. chest pain")

ggplot(heart_df, aes(x=log1_trestbps, fill=factor(target))) +
  geom_density(alpha=0.5) + labs(title = "Target vs. resting blood pressure") 

ggplot(heart_df, aes(x=log1_chol, fill=factor(target))) +
  geom_density(alpha=0.5) + labs(title = "Target vs. cholestrol rate")

print(cor(heart_df$age, heart_df$log1_trestbps))
ggplot(heart_df, aes(x=age, y=log1_trestbps)) +
  labs(title = "Multicollinearity check for Age and Log1_trestbps") +
  geom_point() +
  geom_smooth(method='lm')

print(cor(heart_df$age, heart_df$log1_chol))
ggplot(heart_df, aes(x=age, y=log1_chol)) +
  labs(title = "Multicollinearity check for Age and Log1_chol") +
  geom_point() +
  geom_smooth(method='lm')

print(cor(heart_df$log1_trestbps, heart_df$log1_chol))
ggplot(heart_df, aes(x=log1_trestbps, y=log1_chol)) +
  labs(title = 'Multicollinearity check for Log1_trestbps and Log1_chol') +
  geom_point() +
  geom_smooth(method='lm')
