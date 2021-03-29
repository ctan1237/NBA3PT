setwd("~/")
library(tidyverse)
library(caret)
champs <- read_csv(file = "championsdata.csv")
runners <- read_csv(file = "runnerupsdata.csv")
colnames(champs)
champs %>% 
  select(Team) %>%
  distinct()
champs <- champs %>%
  mutate(Team = ifelse(Team == "Warriorrs","Warriors",Team)) %>%
  mutate(Team = ifelse(Team == "'Heat'","Heat",Team)) %>%
  mutate(Win = as.factor(Win)) %>%
  mutate(Home = as.factor(Home)) %>%
  mutate(X = as.numeric(X))

runners <- runners %>%
  mutate(Win = as.factor(Win)) %>%
  mutate(Home = as.factor(Home)) %>%
  rename(X = Y)

fullPostSeasons <- bind_rows(champs,runners) %>%
  mutate(id = 1:(nrow(champs)*2))

fullPostSeasons_train <- sample_frac(fullPostSeasons,0.75)
fullPostSeasons_test <- anti_join(fullPostSeasons,fullPostSeasons_train,by = 'id')

champs %>%
  select(Team) %>%
  distinct()

ggplot(data = champs, 
       aes(x = Year, y = TPP, color = Team, shape = Win)) + 
  geom_point(size = 1) +
  scale_x_continuous(breaks = 1980:2017) +
  scale_y_continuous(breaks = seq(0,1,by = 0.05), limits = c(0,1)) + 
  theme_dark() +
  scale_color_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(size = 3, angle = 45),
        axis.text.y = element_text(size = 6, angle = 45)) + 
  facet_wrap(~ Home, labeller = "label_both") + 
  ylab("Three Point Percentage")
