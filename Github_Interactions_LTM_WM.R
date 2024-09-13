#Working memory predicts long-term recognition of auditory sequences: Dissociation between confirmed predictions and prediction errors

#Gemma Fernández-Rubio
#Center for Music in the Brain, Aarhus University, Aarhus (Denmark)
#13-09-2024

#LIBRARIES, WORKING DIRECTORY AND DATA ====
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(broom)
library(ggfortify)
library(dplyr)
library(devtools)
library(patchwork)

setwd('/Your_Path')

data_2021 <- read_excel('dataset_2021.xlsx')
data_2022 <- read_excel('dataset_2022.xlsx')
data_2024 <- read_excel('dataset_2024.xlsx')

#2021 DATASET ====

#GENERALIZED LINEAR MODEL (GLM)
#memorized sequences
model_old_2021 <- glm(old ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2021); summary(model_old_2021) #compute GLM
AIC(model_old_2021) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_old_2021) #plot GLM
sum_old_2021 <- tidy(model_old_2021); sum_old_2021[, 2:5] <- round(sum_old_2021[, 2:5], digits = 3) #summarize GLM

#novel sequences
model_new_2021 <- glm(new ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2021); summary(model_new_2021) #compute GLM
AIC(model_new_2021) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_new_2021) #plot GLM
sum_new_2021 <- tidy(model_new_2021); sum_new_2021[, 2:5] <- round(sum_new_2021[, 2:5], digits = 3) #summarize GLM

#2022 DATASET ====

#GENERALIZED LINEAR MODEL (GLM)
#memorized sequences
model_old_2022 <- glm(old ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2022); summary(model_old_2022) #compute GLM
AIC(model_old_2022) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_old_2022) #plot GLM
sum_old_2022 <- tidy(model_old_2022); sum_old_2022[, 2:5] <- round(sum_old_2022[, 2:5], digits = 3) #summarize GLM

#novel sequences
model_new_2022 <- glm(new ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2022); summary(model_new_2022) #compute GLM
AIC(model_new_2022) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_new_2022) #plot GLM
sum_new_2022 <- tidy(model_new_2022); sum_new_2022[, 2:5] <- round(sum_new_2022[, 2:5], digits = 3) #summarize GLM

#2024 DATASET ====
#RESHAPE DATA
data_2024$old <- (data_2024$old_k1 + data_2024$old_k2)/2
data_2024$new <- (data_2024$new_k1 + data_2024$new_k2)/2
data_2024 <- data_2024 %>% filter(dem < 3)

#GENERALIZED LINEAR MODEL (GLM)
#memorized sequences
model_old_2024 <- glm(old ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2024); summary(model_old_2024) #compute GLM
AIC(model_old_2024) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_old_2024) #plot GLM
sum_old_2024 <- tidy(model_old_2024); sum_old_2024[, 2:5] <- round(sum_old_2024[, 2:5], digits = 3) #summarize GLM

#novel sequences
model_new_2024 <- glm(new ~ wm + age + musictraining, family = Gamma(link = 'log'), data = data_2024); summary(model_new_2024) #compute GLM
AIC(model_new_2024) #check goodness-of-fit
par(mfrow = c(2, 2)); plot(model_new_2024) #plot GLM
sum_new_2024 <- tidy(model_new_2024); sum_new_2024[, 2:5] <- round(sum_new_2024[, 2:5], digits = 3) #summarize GLM

#PLOTTING ====
#2021 DATASET
zscores_2021 <- data.frame(old = scale(data_2021$old), new = scale(data_2021$new), wm = scale(data_2021$wm))
mem2021 <- ggplot(data = zscores_2021, mapping = aes(x = old, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM memorized (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

nov2021 <- ggplot(data = zscores_2021, mapping = aes(x = new, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM novel (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

#2022 DATASET
zscores_2022 <- data.frame(old = scale(data_2022$old), new = scale(data_2022$new), wm = scale(data_2022$wm))
mem2022 <- ggplot(data = zscores_2022, mapping = aes(x = old, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM memorized (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

nov2022 <- ggplot(data = zscores_2022, mapping = aes(x = new, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM novel (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

#2024 DATASET
zscores_2024 <- data.frame(old = scale(data_2024$old), new = scale(data_2024$new), wm = scale(data_2024$wm))
mem2024 <- ggplot(data = zscores_2024, mapping = aes(x = old, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM memorized (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

nov2024 <- ggplot(data = zscores_2024, mapping = aes(x = new, y = wm)) + 
  geom_smooth(method = 'glm', color = 'black') + 
  geom_point(alpha = 0.2) +
  labs(x = 'LTM novel (z-scores)', y = 'WM (z-scores)') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'))

(mem2021 | nov2021) / (mem2022 | nov2022) / (mem2024 | nov2024)
ggsave('Figure_1.pdf')


