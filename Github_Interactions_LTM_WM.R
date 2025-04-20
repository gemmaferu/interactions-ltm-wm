#Working memory predicts long-term recognition of auditory sequences: Dissociation between confirmed predictions and prediction errors

#Gemma Fernández-Rubio
#Center for Music in the Brain, Aarhus University, Aarhus (Denmark)
#01-04-2024

#LIBRARIES, WORKING DIRECTORY AND DATA ====
library(readxl)
library(dplyr)
library(psych)
library(broom)
library(effects)
library(ggplot2)
setwd('/YourPath')
data <- read_excel('SupplementaryTable1.xlsx')
data <- data %>% filter(dataset == 2021) #select 2021, 2022, or 2024

#DESCRIPTIVE STATISTICS ====
describe(data[,c('age','wm','musictraining','mem_n','nov_n')]) #summarize main variables

#GENERALIZED LINEAR MODELS (GLMs) ====
#memorized sequences
model_mem <- glm(cbind(mem_n, mem_k - mem_n) ~ wm + age + musictraining, family = binomial(link='logit'), data = data); summary(model_mem) #compute GLM
par(mfrow = c(2, 2)); plot(model_mem) #plot GLM
sum_mem <- tidy(model_mem); sum_mem[, 2:5] <- round(sum_mem[, 2:5], digits = 3) #summarize GLM
eff_mem <- data.frame(predictorEffects(model_mem, ~ wm)) #compute effects
ggplot(eff_mem, aes(x = wm$wm, y = wm$fit)) + #plot effects
  geom_line(color = 'dodgerblue', size = 1) +
  geom_ribbon(aes(ymin = wm$lower, ymax = wm$upper), alpha = 0.2, fill = 'dodgerblue') +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(min(eff_mem$wm$wm), max(eff_mem$wm$wm), by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  labs(x = 'WM', y = 'Probability LTM-M') +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'sans'))

#novel sequences
model_nov <- glm(cbind(nov_n, nov_k - nov_n) ~ wm + age + musictraining, family = binomial(link='logit'), data = data); summary(model_nov) #compute GLM
par(mfrow = c(2, 2)); plot(model_nov) #plot GLM
sum_nov <- tidy(model_nov); sum_nov[, 2:5] <- round(sum_nov[, 2:5], digits = 3) #summarize GLM
eff_nov <- data.frame(predictorEffects(model_nov, ~ wm)) #compute effects
ggplot(eff_nov, aes(x = wm$wm, y = wm$fit)) + #plot effects
  geom_line(color = 'dodgerblue', size = 1) +
  geom_ribbon(aes(ymin = wm$lower, ymax = wm$upper), alpha = 0.2, fill = 'dodgerblue') +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(min(eff_nov$wm$wm), max(eff_nov$wm$wm), by = 2)) +
  scale_y_continuous(breaks = seq(.1, 1, by = 0.05)) +
  labs(x = 'WM', y = 'Probability LTM-N') +
  theme_bw() +
  theme(text = element_text(size = 15, family = 'sans'))

#nt1 sequences
model_novt1 <- glm(cbind(novt1_n, novt1_k - novt1_n) ~ wm + age + musictraining, family = binomial(link = 'logit'), data = data); summary(model_novt1) #compute GLM
par(mfrow = c(2, 2)); plot(model_novt1) #plot GLM
sum_novt1 <- tidy(model_novt1); sum_novt1[, 2:5] <- round(sum_novt1[, 2:5], digits = 3) #summarize GLM

#nt2 sequences
model_novt2 <- glm(cbind(novt2_n, novt2_k - novt2_n) ~ wm + age + musictraining, family = binomial(link = 'logit'), data = data); summary(model_novt2) #compute GLM
par(mfrow = c(2, 2)); plot(model_novt2) #plot GLM
sum_novt2 <- tidy(model_novt2); sum_novt2[, 2:5] <- round(sum_novt2[, 2:5], digits = 3) #summarize GLM

#nt3 sequences
model_novt3 <- glm(cbind(novt3_n, novt3_k - novt3_n) ~ wm + age + musictraining, family = binomial(link = 'logit'), data = data); summary(model_novt3) #compute GLM
par(mfrow = c(2, 2)); plot(model_novt3) #plot GLM
sum_novt3 <- tidy(model_novt3); sum_novt3[, 2:5] <- round(sum_novt3[, 2:5], digits = 3) #summarize GLM

#nt4 sequences
model_novt4 <- glm(cbind(novt4_n, novt4_k - novt4_n) ~ wm + age + musictraining, family = binomial(link = 'logit'), data = data); summary(model_novt4) #compute GLM
par(mfrow = c(2, 2)); plot(model_novt4) #plot GLM
sum_novt4 <- tidy(model_novt4); sum_novt4[, 2:5] <- round(sum_novt4[, 2:5], digits = 3) #summarize GLM
