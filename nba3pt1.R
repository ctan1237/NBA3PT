setwd("~/")
suppressMessages(library(tidyverse))
averages <- read.csv(file = 'champs_and_runner_ups_series_averages.csv')
champs <- read.csv(file = "championsdata.csv")
runners <- read.csv(file = "runnerupsdata.csv")

head(champs)
head(runners)

# Check dataset dimensions 
nrow(champs)
ncol(champs)
nrow(runners)
ncol(runners)

# Varibale Type Check
champs <- champs %>%
  mutate(Win = as.factor(Win)) %>%
  mutate(Home = as.factor(Home)) %>%
  mutate(Team = as.integer(Team))
runners <- runners %>%
  mutate(Win = as.factor(Win)) %>%
  mutate(Home = as.factor(Home)) %>%
  mutate(Team = as.integer(Team)) 

eighty_to_now  <- ggplot(data = averages,aes(x = Year, y = TP,fill = Status)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  ylab('Average Three Point Field Goals') +
  theme(axis.text.x = element_text(angle=45,hjust = 0.5,vjust = 1)) +
  ggtitle("NBA Finals, 1980-2017")

eighty_to_now

# Plot of PTS: Points socred in NBA Finals, 1980-2017
PPTS <- ggplot(data = df, aes(x = Year, y = PTS, fill='')) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  geom_smooth(method = 'loess',se=FALSE, color = 'yellow') + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ylab(' Points Scored')
ggtitle("Total points socred NBA Finals, 1980-2017")


# Plot of PTS: Points socred in NBA Finals, 1980-2017
PPTS <- ggplot(data = df, aes(x = Year, y = PTS, fill='')) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  geom_smooth(method = 'loess',se=FALSE, color = 'yellow') + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ylab(' Points Scored')
ggtitle("Total points socred NBA Finals, 1980-2017")

# Plot of PF: Personal fouls
PPF <- ggplot(data = df, aes(x = Year, y = PF, fill='')) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  geom_smooth(method = 'loess',se=FALSE, color = 'yellow') + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ylab(' Personal Foul')
ggtitle("Personal Fouls NBA Finals, 1980-2017")

# Plot of TOV: Turnovers
PTOV <- ggplot(data = df, aes(x = Year, y = TOV, fill='')) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  geom_smooth(method = 'loess',se=FALSE, color = 'yellow') + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ylab(' Turnover')
ggtitle("Turnovers NBA Finals, 1980-2017")

grid.arrange(PPTS, PPF, PTOV,ncol=1)

# Plot of TP: Three point field goals in NBA Finals, 1980-2017
PTP <- ggplot(data = df,aes(x = Year, y = TP)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  ylab('Three Point Field Goals in NBA Finals, 1980-2017') +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ggtitle("Three points scored NBA Finals, 1980-2017")

# Plot of TPA: Three point attempts
PTPA <- ggplot(data = df, aes(x = Year, y = TPA)) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  geom_smooth(method = 'loess',se=FALSE,color = 'blue') +
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  ylab('Three point attempts')
ggtitle("Three point attempts NBA Finals, 1980-2017")

# Plot of TPP
Percentage_of_Three_points <- df %>% mutate(Percentage_of_Three_points = 3*TP/PTS*100)
PTPP<- ggplot(data = Percentage_of_Three_points, aes(x = Year, y = Percentage_of_Three_points)) +
  geom_point(size = 1) +
  geom_smooth(method = 'loess',se=FALSE, color = 'blue') +
  scale_x_continuous(breaks=seq(1980, 2017, 1)) +
  theme(axis.text.x = element_text(angle=90,hjust = 1,vjust = 1)) +
  theme(legend.position="none") +
  ylab('%') 
ggtitle("Three point percentage NBA Finals, 1980-2017")

grid.arrange(PTP, PTPA, PTPP,ncol=1)

# Fit the logistic model to full data
lm <- glm(Win ~ TPA + TPP + Personalfoulsdifference + Turnoversdifference, data = df, family = binomial)
summary(lm)

# Using original data, create random indices for train/test sets (80% training size)
n <- nrow(df)
train_ind <- sample(seq_len(n), size = floor(0.8 * n))

# Split train/test sets
df_train <- df[train_ind, ]
df_test <- df[-train_ind, ]

# Check training data
df_train %>% head

# Check test data
df_test %>% head

# Fit model to training data
lm_train <- glm(Win ~ TPA + TPP, data=df_train, family='binomial')

# Predict on training set (within-sample predictions)
df_train$y_pred_probs <- predict(lm_train, df_train, type="response")
df_train$y_pred <- ifelse(df_train$y_pred_probs > 0.5, 1, 0)

# Predict on test set (out-of-sample predictions)
df_test$y_pred_probs <- predict(lm_train, df_test, type="response")
df_test$y_pred <- ifelse(df_test$y_pred_probs > 0.5, 1, 0)

# Convert logistic probabilities into binary predictions
df_train$y_pred <- ifelse(df_train$y_pred_probs > 0.5, 1, 0)
df_test$y_pred <- ifelse(df_test$y_pred_probs > 0.5, 1, 0)

# Confusion matrix for predictions on training set
cm_train <- confusionMatrix(as.factor(df_train$y_pred), as.factor(df_train$Win), positive='1')
cm_train$table

cm_train$overall['Accuracy'] %>% round(2)
cm_train$byClass['Recall'] %>% round(2)
cm_train$byClass['Precision'] %>% round(2)

# Confusion matrix for predictions on test set
cm_test <- confusionMatrix(as.factor(df_test$y_pred), as.factor(df_test$Win), positive='1')
cm_test$table

cm_test$overall['Accuracy'] %>% round(2)
cm_test$byClass['Recall'] %>% round(2)
cm_test$byClass['Precision'] %>% round(2)

