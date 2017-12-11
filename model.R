# https://docs.google.com/spreadsheets/d/1Cz3wnwugHApFXO0ogMLtBe5CzD_Uu1q8YBPpZPS2R3U/edit?usp=sharing

# library(generalhoslem)
library(tidyverse)
library(oilabs)
library(gsheet)
library(GGally)
titanic <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Cz3wnwugHApFXO0ogMLtBe5CzD_Uu1q8YBPpZPS2R3U/edit?usp=sharing')
titanic$passenger_id <- as.factor(titanic$passenger_id)
titanic$gender <- as.factor(titanic$gender)
titanic$age <- as.factor(titanic$age)
titanic$passenger_class <- as.factor(titanic$passenger_class)
titanic$embarked_from <- as.factor(titanic$embarked_from)
# summary(titanic)
# titanic %>% select(-passenger_id) %>% ggpairs()


m_full <- glm(has_survived ~ gender + age + number_of_siblings_and_spouses + number_of_parents_and_children + passenger_class + fare + embarked_from, data = titanic, family = binomial(link = "logit"))
summary(m_full)
m_wo_number_of_parents_and_children <- glm(has_survived ~ gender + age + number_of_siblings_and_spouses + passenger_class + fare + embarked_from, data = titanic, family = binomial(link = "logit"))
summary(m_wo_number_of_parents_and_children)
m_best <- glm(has_survived ~ gender + age + number_of_siblings_and_spouses + passenger_class + embarked_from, data = titanic, family = binomial(link = "logit"))
summary(m_best)

qplot(x = .fitted, y = .resid, data = m_best) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")
qplot(sample = .resid, data = m_best, geom = "qq")

# logitgof(titanic$has_survived, fitted(m_best))

library(ResourceSelection)
hl <- hoslem.test(titanic$has_survived, fitted(m_best), g=8)
hl

m_best_2 <- glm(has_survived ~ gender + age + number_of_siblings_and_spouses + passenger_class, data = titanic, family = binomial(link = "logit"))
summary(m_best_2)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_2), g=8)
hl

m_best_3 <- glm(has_survived ~ gender + age + passenger_class, data = titanic, family = binomial(link = "logit"))
summary(m_best_3)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_3), g=8)
hl

m_best_5 <- glm(has_survived ~ age + passenger_class, data = titanic, family = binomial(link = "logit"))
summary(m_best_4)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_4), g=8)
hl

m_best_6 <- glm(has_survived ~ gender + passenger_class, data = titanic, family = binomial(link = "logit"))
summary(m_best_6)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_6), g=8)
hl

m_best_7 <- glm(has_survived ~ gender + age, data = titanic, family = binomial(link = "logit"))
summary(m_best_7)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_7), g=8)
hl

m_best_8 <- glm(has_survived ~ passenger_class + age + number_of_siblings_and_spouses, data = titanic, family = binomial(link = "logit"))
summary(m_best_8)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_8), g=8)
hl

m_best_9 <- glm(has_survived ~ passenger_class + age + number_of_siblings_and_spouses + embarked_from, data = titanic, family = binomial(link = "logit"))
summary(m_best_9)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_9), g=8)
hl
hl$observed
hl$expected

m_best_10 <- glm(has_survived ~ gender + number_of_siblings_and_spouses, data = titanic, family = binomial(link = "logit"))
summary(m_best_10)
hl <- hoslem.test(titanic$has_survived, fitted(m_best_10), g=8)
hl
hl$observed
hl$expected

