install.packages(c('tidyverse' , 'vtable'))
install.packages('gghighlight')
library(tidyverse)
library(vtable)
library(ggplot2)
library(gghighlight)
library(scales)
df <- readRDS('college_expenses_and_enrollment.Rdata') 
vtable::vtable(df)
summary(df)

data1 <- df %>%
  mutate('Total_Income_Per_Capita' = Total.Income/Total.Enrollment) %>%
  mutate('Total_Expenses_Per_Capita' = Total.Expenses/Total.Enrollment) %>%
  mutate('Overall.Income' = Total.Income + Tuition + Federal + State + Local + Private + Sales) %>%
  mutate('Overall.Expenses' = Total.Expenses + Research + Public.Service + Student.Services + Instruction + Academic.Support + Other) %>%
  mutate('Overall_Income_Per_Capita' = Overall.Income/Total.Enrollment) %>%
  mutate('Overall_Expenses_Per_Capita' = Overall.Expenses/Total.Enrollment) %>%
  mutate('Profit_Per_Capita' = Overall_Income_Per_Capita - Overall_Expenses_Per_Capita) %>%
  mutate('Tuition_Per_Capita' = Tuition/Total.Enrollment)

data1 %>%
  group_by(type) %>%
  summarise(Institutional_Profit_Per_Capita = mean(Profit_Per_Capita, na.rm = TRUE)) %>%
  ggplot(aes(x = type, y = Institutional_Profit_Per_Capita)) + geom_col(position = 'dodge') + geom_bar(stat="identity",fill="lightblue")


data1 %>%
  ggplot(aes(x = Overall.Income, y = Research)) + 
  geom_point() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

cor(data1$Total.Income, data1$Research)

           
data1 %>%
  group_by(STABBR) %>%
  summarise(Avg_Tuition_Per_State_Per_Capita = mean(Tuition_Per_Capita, na.rm = TRUE)) %>%
  arrange(-Avg_Tuition_Per_State_Per_Capita) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(STABBR, -Avg_Tuition_Per_State_Per_Capita), y = Avg_Tuition_Per_State_Per_Capita)) + 
  geom_col(position = 'dodge', fill='lightblue') + 
  gghighlight(max(Avg_Tuition_Per_State_Per_Capita) > 25000) + 
  geom_text(aes(label = sprintf("%0.2f", round(Avg_Tuition_Per_State_Per_Capita, digits = 2)), vjust = 1.5)) +
  theme(legend.position="none") +
  xlab("American States") +  
  ylab("Average Tuition Per State Per Student") +
  ggtitle("Top 10 States with Highest Average Tuition Fee") +
  theme(plot.title = element_text(face='bold'),axis.title = element_text(face='bold'))



cor(df$Total.Income, df$Total.Expenses)
lm(Total.Income ~ Total.Expenses, data = df)


  
