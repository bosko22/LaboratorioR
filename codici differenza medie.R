library(tidyverse)
library(readxl)
titanic <- read_excel(here("dati","titanic_train.xlsx"))
titanic <- janitor::clean_names(titanic)

titanic %>%
  group_by(sex) %>%
  summarise(mean = mean(survived, na.rm = TRUE),
            sd = sd(survived, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (alpha / 2), df = n - 1) * se, # lower.ci = mean + qt(alpha / 2, df = n - 1) * se,
         upper.ci = mean + qt(1 - (alpha / 2), df =  n - 1) * se) %>% # upper.ci = mean + qt(1 - (alpha / 2), df =  n - 1) * se)
  ggplot() +
  aes(x = mean, y = sex) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower.ci, xmax = upper.ci), width=.05,
                position = position_dodge(.9)) +
  labs(x = "mean survived", y = "sex")

t <- t.test(survived ~ sex, data = titanic, conf.level = .95)

t$conf.int
t$estimate
inf <- t$conf.int[1]
sup <- t$conf.int[2]
diff <- t$estimate[1] - t$estimate[2]

titanic %>%
  group_by(sex) %>%
  summarise(mean = mean(survived, na.rm = TRUE),
            sd = sd(survived, na.rm = TRUE),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (alpha / 2), df = n - 1) * se, # lower.ci = mean + qt(alpha / 2, df = n - 1) * se,
         upper.ci = mean + qt(1 - (alpha / 2), df =  n - 1) * se) %>% # upper.ci = mean + qt(1 - (alpha / 2), df =  n - 1) * se)
  add_row(sex = "difference", mean = diff, lower.ci = inf, upper.ci = sup) %>% 
  ggplot() +
  aes(x = mean, y = sex) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower.ci, xmax = upper.ci), width=.05,
                position = position_dodge(.9)) +
  labs(x = "mean survived", y = "sex")


ggplot() +
  aes(x = diff, y = "diff") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = inf, xmax = sup), width=.05,
                position = position_dodge(.9)) +
  labs(x = "", y = "")
