library(caTools)
library(tidyverse)
library(miscTools)
library(Metrics)
library(plotly)
data = read.csv("/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/AE/HWs/HW2/framingham.csv")
data$TenYearCHD <- factor(data$TenYearCHD)
data$male <- factor(data$male)
data$currentSmoker <- factor(data$currentSmoker)
data$BPMeds <- factor(data$BPMeds)
data$prevalentStroke <- factor(data$prevalentStroke)
data$prevalentHyp <- factor(data$prevalentHyp)
data$diabetes <- factor(data$diabetes)
set.seed(38)
N <- nrow(data)
idx = sample.split(data$TenYearCHD, 0.75)
train <- data[idx,]
test = data[!idx,]



#a
ggplot(data, aes(sysBP, after_stat(count), fill = TenYearCHD)) +
  geom_density(position = "fill")+ 
  xlab("Systolic Blood Pressure") + labs(fill='Chronic Heart Diseas') +
  theme(legend.text=element_text(size=12), 
        axis.title=element_text(size=14))

ggplot(data, aes(diaBP, after_stat(count), fill = TenYearCHD)) +
  geom_density(position = "fill")+ 
  xlab("Diastolic Blood Pressure") + labs(fill='Chronic Heart Diseas') +
  theme(legend.text=element_text(size=12), 
        axis.title=element_text(size=14))
#b


#c


#d