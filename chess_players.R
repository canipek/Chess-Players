getwd()
setwd("C:/Users/canip/Masaüstü/data")
df1 <- read.csv("chess_games.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gplots)
library(FSA)
library(rstatix)
library(ggpubr)

str(df1)
head(df1)

######## Data Manipulation Part #########

#We only want rated games in the dataframe
df <- df1[df1$rated == TRUE, ]
df$rated <- NULL

# To drop duplicate rows, we have to remove id column first
df$game_id <- NULL
df <- df[!duplicated(df), ]


print(table(df$time_increment))
# We don't want custom time increment types
# Create a vector that includes default time increment types of Lichess.com
normal_times <- c("1+0", "2+1", "3+0", "3+2", "5+0", "5+3", "10+0", "10+5", "15+10", "30+0", "30+20")

# Drop the others
df <- df[df$time_increment %in% normal_times, ]
print(table(df$time_increment))

# It's impossible for a Chess game to end before 4 moves naturally
# So we remove the rows with "turn" value under 4
df <- df[df$turns > 3, ]
table(df$victory_status)

# Also remove the rows with "turn" value under 10 unless victory status is mate or draw
df <- df[(df$turns > 10 | df$victory_status %in% c("Mate", "Draw")), ]

# Create two new variables for rating difference and average rating of the game
df$avg_rating <- (df$black_rating + df$white_rating) / 2
df$dif_rating <- (df$white_rating - df$black_rating)

# Save data in here to use in research part for Linear Regression
df_lm <- df


# Drop the rows with rating difference higher than 300
df <- df[df$dif_rating < 300 & df$dif_rating > -300, ]

############ Data description and visualization part ##############

#Head
head(df)

#Structure
str(df)

#Summary
summary(df)


# Let's see victory status ratios with a pie chart
tbl_vs <- table(df$victory_status)
df_plot <- data.frame(
  cat_vs = names(tbl_vs),
  count_vs = as.numeric(tbl_vs)
)

grf_vic <- ggplot(df_plot, aes(x = "", y = count_vs, fill = cat_vs)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  theme_void() +
  guides(fill = guide_legend(title = "End"))
print(grf_vic)


# Let's visualize game results

ggplot(data = df)+
  geom_bar(mapping = aes(x = winner, fill = victory_status)) +
  ggtitle("Barplot of game results") +
  xlab("Winner") +
  ylab("") +
  theme_minimal()


# How much information can we get from a single plot?
ggplot(data = df)+
  geom_point(aes(x = dif_rating, y= avg_rating, color = victory_status, shape = time_increment), 
             show.legend = T)+
  facet_wrap(~winner)+
  ylab("Average Rating")+
  xlab("Rating Difference")+
  theme_bw()


# Which openings are more favorable for any of two sides?

tbl_1 <- df %>%
  group_by(winner, opening_shortname) %>%
  summarise(count = n()) %>%
  spread(winner, count, fill = 0)


tbl_1$total_games <- tbl_1$White + tbl_1$Black + tbl_1$Draw

tbl_1 = tbl_1[tbl_1$total_games > 19, ]

tbl_1$w_index <- (tbl_1$White + (1/2)*tbl_1$Draw)/tbl_1$total_games
tbl_1$b_index <- 1- tbl_1$w_index


black_highest <- tbl_1 %>% 
  arrange(desc(b_index)) %>% 
  head(5)

white_highest <- tbl_1 %>% 
  arrange(desc(w_index)) %>% 
  head(5)

b_plot <- ggplot(black_highest, aes(x = opening_shortname, y = b_index, color= opening_shortname, fill = opening_shortname)) +
  geom_bar(stat = "identity") +
  labs(title = "Best openings for Black", x = "opening_shortname", y = "index")


w_plot <- ggplot(white_highest, aes(x = opening_shortname, y = w_index, fill = opening_shortname)) +
  geom_bar(stat = "identity") +
  labs(title = "Best openings for White", x = "opening_shortname", y = "index") +
  scale_fill_brewer(palette = "Set3")


library(gridExtra)
grid.arrange(w_plot, b_plot, ncol = 2)

# Check the distributions of ratings. Are they distributed normal or not?
ggplot(df, aes(x = black_rating, y = ..density..)) +
  geom_histogram(binwidth = 20, color = "darkgray", fill = "black")
theme_minimal()


qqnorm(df$black_rating, main = "Black Rating")
qqline(df$black_rating)

ggplot(df, aes(x = white_rating, y = ..density..)) +
  geom_histogram(binwidth = 20, color = "darkgray", fill = "white") +
  theme_dark()

qqnorm(df$white_rating, main = "White Rating")
qqline(df$white_rating)

ggplot(df, aes(x = avg_rating, y = ..density..)) +
  geom_histogram(binwidth = 20, color = "black", fill = "gray") +
  theme_minimal()

qqnorm(df$avg_rating, main = "Average Rating")
qqline(df$avg_rating)

# What is the reason of density around 1500?
mod_black <- df %>% 
  count(white_rating) %>% 
  filter(n == max(n))

mod_white <- df %>% 
  count(black_rating) %>% 
  filter(n == max(n))

print(mod_white)
print(mod_black)

# Use Kolmogorov-Smirnov test to check normality with alpha = 0.05

ks.test(df$black_rating, "pnorm")
ks.test(df$white_rating, "pnorm")
ks.test(df$avg_rating, "pnorm")

# P<0.05 for all, H0 rejected, distributions are not normal.



######### Compare side preferences of amateur players, professional players and AI bots ###############

# We have two other datasets to use in this part

df2 <- read.csv("alphazero_stockfish_games_df.csv")
df3 <- read.csv("chess_data.csv", sep = ";")

# Pie chart for Amateur Players
tbl_w <- table(df$winner)
amateurs_pie_data <- data.frame(
  cat_w = names(tbl_w),
  count_w = as.numeric(tbl_w)
)

amateurs_pie <- ggplot(amateurs_pie_data, aes(x = "", y = count_w, fill = cat_w)) +
  geom_bar(stat = "identity", width = 50, color = "darkgray") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title= "Amateur Players (Lichess database)")+
  scale_fill_manual(values = c("White" = "white", "Black" = "black", "Draw" = "gray"))

print(amateurs_pie)


# Pie chart for AI Bots

ai_tbl <- table(df2$result)

ai_pie <- ggplot(data.frame(ai_tbl), aes(x = "", y = Freq, fill = names(ai_tbl))) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "AI Bots (Stockfish vs AlphaZero)") +
  scale_fill_manual(values = c("1-0" = "white", "0-1" = "black", "1/2-1/2" = "darkgray"))


print(ai_pie)


# Pie chart for Professional Players (without using ggplot this time)
# We have to manipulate the data to create pie chart of results properly

df3$total_white <- (df3$wp_win + df3$wp_draw + df3$wp_lose)
df3$white_win_rate <- (df3$wp_win/df3$total_white)

df3$total_black <- (df3$bp_win + df3$bp_draw + df3$bp_lose)
df3$black_win_rate <- (df3$bp_win/df3$total_black)


df3$rate_draw <- (df3$wp_win + df3$bp_draw) / df3$total_games

pro_black <- mean(df3$black_win_rate)
pro_white <- mean(df3$white_win_rate)
pro_draw <- mean(df3$rate_draw)

pro_pie_data <- c(pro_black, pro_white, pro_draw)

# Data is ready for visualization

pro_pie <- pie(pro_pie_data, col = c("black", "white", "gray"), main = "Professional Players (FIDE database)")
print(pro_pie)



############################# Hypothesis 1 ##########################################
# Is there any significant relationship between level of the game and number of turns?
# H0: There is no significant relationship between level of the game and number of turns (r = 0)
# H1: There is significant relationship between level of the game and number of turns (r ≠ 0)

df_selected <- select(df, turns, avg_rating)
df_filtered <- filter(df_selected, !abs(df$dif_rating)>50)
df_with_correlation <- mutate(df, correlation = cor(turns, avg_rating))

correlation_test <- cor.test(df_with_correlation$turns, df_with_correlation$avg_rating)

print(correlation_test)

scatter_plot <- ggplot(df_with_correlation, aes(x = turns, y = avg_rating)) +
  geom_point() +
  xlab("Turns") +
  ylab("Rating") +
  ggtitle("Relation between turns and rating")
print(scatter_plot)

# p<0.05 there is no significant relationship between rating level of the game and number of turns

############################# Hypothesis 2 ##########################################
#Does the winner status depends on rating difference?

# H0: The winner status does not depends on rating difference
# H1: The winner status depends on rating difference

ggboxplot(df, x='winner', y='dif_rating',
          color = 'winner',
          palette = "jco")

df %>% group_by(winner) %>% 
  shapiro_test(dif_rating)

#p<0.05, residuals of rating difference data is not normal.
#Kruskal-Wallis test should be employed.

df %>% kruskal_test(dif_rating ~ winner)

#p<0.05, at least one of the groups come from different distribution.

# Multiple Comparison
df %>% dunn_test(dif_rating ~ winner)

#p<0.05 for all tests, each one of the groups come from different distribution.

############################# Hypothesis 3 ##########################################
#Does the winning side chances based on average rating?

# H0: The winning side does not chances based on average rating.
# H1: The winning side chances based on average rating.

ggboxplot(df[!df$winner=="Draw", ], x='winner', y='avg_rating',
          color = 'winner',
          palette = "jco")

df[!df$winner=="Draw", ] %>% group_by(winner) %>%
  shapiro_test(avg_rating)

#p<0.05, residuals of rating difference data is not normal.
#Mann-Whitney U test should be employed.

df[!df$winner=="Draw", ] %>% 
  wilcox_test(avg_rating ~ winner, paired = F)

# p>0.05, HO cannot be rejected, winning side does not chances based on average rating.
# Chances of winning of both sides (black or white) are equal for all rating intervals.

############################# Hypothesis 4 ##########################################
# Are the winner and victory status variables are related with each other

# H0: Victory status and winner status variables are not related with each other.
# H1: Victory status and winner status variables are related with each other.

df[!df$winner=="Draw", ] %>% group_by(winner, victory_status) %>% 
  summarise(
    n=n()
  ) %>% ggbarplot(x='winner', y='n', color = 'victory_status',
                  position = position_dodge(),
                  label = TRUE, lab.pos = "out", fill = 'victory_status', palette = "jco"
  )

chisq_test(table(df[!df$winner=="Draw", ]$winner, df[!df$winner=="Draw", ]$victory_status))

# According to chi-square test result p<0.05.
# Victory status and winner status variables are related with each other.

############################# Hypothesis 5 ##########################################
# Which opening pairs are being used by players of different rating distributions?

# We will test separately for all opening pairs. The hypotheses for each are as follows: 

# H0: Player rating distributions of both openings are same.
# H1: Player rating distributions of both openings are different.


most_common_openings <- tbl_1 %>% 
  arrange(desc(total_games))

# Visualize distributions for each group
ggboxplot(subset(df, opening_shortname == most_common_openings$opening_shortname[1:5]), x= "opening_shortname", y='avg_rating',
          color = "opening_shortname",
          palette = "jco")


for (i in most_common_openings$opening_shortname[1:5]) {
  subset_df <- df$avg_rating[df$opening_shortname == i]
  shapiro_test <- shapiro.test(subset_df)
  print(paste("Opening:", i))
  print(shapiro_test)
}
#According to Shapiro-Wilk tests, some of the groups are distributed normal. Let's test their variance equality status

var.test(avg_rating~opening_shortname, data= subset(df, opening_shortname == "Sicilian Defense" | opening_shortname == "Italian Game"))
var.test(avg_rating~opening_shortname, data= subset(df, opening_shortname == "Sicilian Defense" | opening_shortname == "Queen's Gambit"))
var.test(avg_rating~opening_shortname, data= subset(df, opening_shortname == "Italian Game" | opening_shortname == "Queen's Gambit"))

# p>0 for all. Variances are equal.

# We have to use Mann-Whitney U test for pairs that include the openings "Queen's Pawn Game" and "French Defense" and Two-Sample T test for others.

# Create group couples
dual_openings <- combn(unique(most_common_openings$opening_shortname[1:5]), 2)

not_normal <- c("Queen's Pawn Game", "French Defense")

# Create the function that generates proper test for each group pairs
mean_compare_tests <- function(cat_1, cat_2) {
  cat_1_vals <- df$avg_rating[df$opening_shortname == cat_1]
  cat_2_vals <- df$avg_rating[df$opening_shortname == cat_2]
  if (cat_1 %in% not_normal | cat_2 %in% not_normal) {
    result <- wilcox.test(cat_1_vals, cat_2_vals)
  } else {
    result <- t.test(cat_1_vals, cat_2_vals, alternative = "two.sided", var.equal = TRUE)
  }
  
  return(result)
}

# Apply proper test for each group pairs
test_results <- apply(dual_openings, 2, function(x) mean_compare_tests(x[1], x[2]))


# Print results
for (i in 1:ncol(dual_openings)) {
  cat("Groups:", dual_openings[1, i], "-", dual_openings[2, i], "\n")
  cat("Avg. ratings:", mean(df$avg_rating[df$opening_shortname == dual_openings[1, i]]), mean(df$avg_rating[df$opening_shortname == dual_openings[2, i]]), "\n")
  cat("Test statsitic:", test_results[[i]]$statistic, "\n")
  cat("P value:", test_results[[i]]$p.value, "\n\n")
}



################# Regression Analysis ##########################

# Let's try to expect rating of rival player with our rating as independent variable (black or white rating).

model <- lm(black_rating ~ white_rating, data = df_lm)
model_limited <- lm(black_rating ~ white_rating, data = df)
summary(model)
summary(model_limited)

cat("Black= 1000 - White= ", predict(model, newdata = data.frame(white_rating = 1000)))
cat("Black= 1300 - White= ", predict(model, newdata = data.frame(white_rating = 1300)))
cat("Black= 1500 - White= ", predict(model, newdata = data.frame(white_rating = 1500)))
cat("Black= 1600 - White= ", predict(model, newdata = data.frame(white_rating = 1600)))
cat("Black= 1700 - White= ", predict(model, newdata = data.frame(white_rating = 1700)))
cat("Black= 1900 - White= ", predict(model, newdata = data.frame(white_rating = 1900)))
cat("Black= 2100 - White= ", predict(model, newdata = data.frame(white_rating = 1900)))

model_reverse <- lm(white_rating ~ black_rating, data = df_lm)

cat("White= 1000 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1000)))
cat("White= 1300 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1300)))
cat("White= 1500 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1500)))
cat("White= 1600 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1600)))
cat("White= 1700 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1700)))
cat("White= 1900 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1900)))
cat("White= 2100 - Black= ", predict(model_reverse, newdata = data.frame(black_rating = 1900)))

ggplot(df_lm, aes(x = white_rating, y = black_rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "White Rating", y = "Black Rating") +
  ggtitle("Scatter Plot")

y_val <- c()
x_val <- c()
for (i in 700:2700) {
  prd <- predict(model_reverse, newdata = data.frame(black_rating = i))
  y_val <- c(prd, y_val)
  x_val <- c(i, x_val)
}

dif_val <- abs(y_val-x_val)

pred_dist_data <- data.frame(x = x_val, y = dif_val)

ggplot(data = pred_dist_data, aes(x = x_val, y = dif_val)) +
  geom_line(color = "blue", linewidth = 1.5) +
  labs(title = "Estimated Difference", x = "Black Rating", y = "Difference")

##################### Part 2 ###########################

chess_data_reduced <- read.csv("chess_data_reduced.csv", header = TRUE, sep = ";")
str(chess_data_reduced)


head(chess_data_reduced)


correlation <- cor.test(chess_data_reduced$standard, chess_data_reduced$draw_rate)
correlation

# There is a significant positive correlation between player's level and draw rate.

library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(MASS)
library(corrplot)

summary<-chess_data_reduced %>%
  group_by(gender) %>%
  get_summary_stats(standard, type = "mean_sd")
data.frame(summary)

hist(chess_data_reduced$draw_rate, breaks = 20)


shapiro.test(chess_data_reduced$draw_rate)

# Distribution is not distributed normally.

boxplot(standard~gender,
        data=chess_data_reduced,
        main= "Standard Chess Rating Distribution for Each Gender",
        xlab = "Gender",
        ylab = "Standard Chess Rating",
        col = "yellow",
        border = "navy")

hist(chess_data_reduced$pc_index, breaks = 20)

shapiro.test(chess_data_reduced$pc_index)

leveneTest(pc_index ~ continent, data = chess_data_reduced)

#Assumptions have met for ANOVA

summary<-chess_data_reduced %>%
  group_by(continent) %>%
  get_summary_stats(standard, type = "mean_sd")
data.frame(summary)


res_aov <- aov(standard ~ continent,
               data = chess_data_reduced)
summary(res_aov)



TukeyHSD(res_aov)

boxplot(standard~continent,
        data=chess_data_reduced,
        main= "Standard Chess Rating Distribution for Each Continent",
        xlab = "Continent",
        ylab = "Standard Chess Rating",
        col = "yellow",
        border = "navy")


kikare <- data.frame(chess_data_reduced$best, chess_data_reduced$gender)
str(kikare)


kikare = table(chess_data_reduced$best, chess_data_reduced$gender)
print(kikare)

print(chisq.test(kikare))

# Time control preference and gender variables are significantly related with each other.


newdata <- data.frame(chess_data_reduced$standard, chess_data_reduced$rapid, chess_data_reduced$blitz)
str(newdata)



round(cor(newdata), digits = 2)



corrplot(cor(newdata), method = "number", type = "upper")

newdata2 <- newdata %>% 
  rename(
    std = chess_data_reduced.standard,
    rpd = chess_data_reduced.rapid,
    btz = chess_data_reduced.blitz
  )
str(newdata2)

corrplot(cor(newdata2), method = "number", type = "upper")

round(cor(newdata2), digits = 2)

attach(chess_data_reduced)


chess_data_reduced$gender<-c(Female=0,Male=1)[chess_data_reduced$gender]
chess_data_reduced$gender
str(chess_data_reduced)




model <- chess_data_reduced %>% dplyr::select(standard, gender)
head(model)

png(file="LogisticRegression.png")
plot(model$standard, model$gender, xlab = "standard", ylab = "gender")

g = glm(model$gender ~ model$standard , family=binomial, model)
summary(g)

points(standard, fitted(g), pch=200)
summary(g)

dev.off()

pred <- predict(g, type = "response")
head(pred)

str(pred)

y_pred_num <- ifelse(pred > 0.5, 1, 0)

y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- model$gender
misClasificError <- mean(y_pred != model$gender)
print(paste('Accuracy',1-misClasificError))

table(y_pred, model$gender)


relation <- lm(standard ~ pc_index)
print(relation)

print(summary(relation))


resultregression <-  predict(relation)

print(resultregression)
resultregression <- data.frame(resultregression)
write.csv(resultregression, "C:/Users/canip/Documents/IST3162 - Homework 1 - Group 11/resultregression.csv", row.names=FALSE)


png(file = "LinearRegression.png")
plot(standard, pc_index, col = "blue",main = " Regression", 
     abline(lm(pc_index~standard)),
     cex = 1.3,pch = 16,xlab = "Standard Chess Rating",
     ylab = "Piece Color Index")

dev.off()

# Estimating Continent of Players with Decision Tree Classification

df <- read.csv("chess_data_reduced.csv", header = TRUE, sep=";")

str(df)


set.seed(1234)
ind <- sample(2, nrow(df), replace = T, prob = c(0.7, 0.3))
train <- df[ind == 1,]
test <- df[ind == 2,]

##################################
##Decision Tree (Classification)
##################################

library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(caret)
library(pROC)
library(tree)


tree <- rpart(continent ~ standard + rapid + win_rate + draw_rate + age, data = train)
rpart.plot(tree)


printcp(tree)
rpart(formula = continent ~ standard + rapid + win_rate + draw_rate + age, data = train)

p <- predict(tree, train, type = 'class')
confusionMatrix(p, as.factor(train$continent), positive = 'EUROPE')

p2 <- predict(tree, test, type = 'class')
confusionMatrix(p2, as.factor(test$continent), positive = 'EUROPE')

##################################
#Random Forest for Classification
##################################

library(randomForest)
library(datasets)
library(caret)

df$continent <- as.factor(df$continent)
table(df$continent)

set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]


rf <- randomForest(continent ~ standard + rapid + win_rate + draw_rate + age, data=train, proximity=TRUE) 
print(rf)
randomForest(formula = continent ~ standard + rapid + win_rate + draw_rate + age, data = train)


p1 <- predict(rf, train)
confusionMatrix(p1, train$continent)


p2 <- predict(rf, test)
confusionMatrix(p2, test$ continent)


#################################################
#Support Vector Machine (SVM) for Classification 
#################################################

library(e1071)

df$continent <- as.factor(df$continent)
table(df$continent)
str(df$continent)

set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]


svm_model <- svm(continent ~ standard + rapid + win_rate + draw_rate + age, data = train, 
                 type = "C-classification", kernel = "linear")


#compute training accuracy
pred_train <- predict(svm_model, train)
mean(pred_train == train$continent)


library(caret)
confusionMatrix(pred_train, train$continent)


#compute test accuracy
pred_test <- predict(svm_model, test)
mean(pred_test == test$continent)


library(caret)
confusionMatrix(pred_test, test$continent)


##############################################
# Artificial Neural Network for Classification
##############################################


library(dplyr)
library(MASS)


data_nnc = df %>% dplyr::select(standard, rapid, win_rate, draw_rate, age, gender)
str(data_nnc)


data_nnc$gender<-c(Female=0,Male=1)[df$gender]
str(data_nnc)


# Normalize the data
maxs <- apply(data_nnc, 2, max) 
mins <- apply(data_nnc, 2, min)
scaled <- as.data.frame(scale(data_nnc, center = mins, 
                              scale = maxs - mins))

# Split the data into training and testing set
index <- sample(1:nrow(data_nnc), round(0.75 * nrow(data_nnc)))
train <- scaled[index,]
test <- scaled[-index,]

train$continent <- factor(train$gender)

library(neuralnet)
set.seed(1234)
nn_classification_model <- neuralnet(gender ~ standard + rapid + win_rate + draw_rate + age,
                                     data = train,
                                     hidden = 3,
                                     err.fct = "ce",
                                     linear.output = FALSE)
plot(nn_classification_model)


# Predict on train data
nnc1 <- compute(nn_classification_model, train[,1:5])


actual <- round(train$gender, digits = 0)
prediction <- round(nnc1$net.result, digits = 0)
traintab <- table(actual,prediction)
traintab
library(caret)
confusionMatrix(traintab)


# Predict on test data
nnc2 <- compute(nn_classification_model, test[,1:5])

actual2 <- round(test$gender, digits = 0)
prediction2 <- round(nnc2$net.result, digits = 0)
traintab2 <- table(actual2,prediction2)
traintab2
library(caret)
confusionMatrix(traintab2)



####################################################
# Comparison of Machine Learning REGRESSION Models
####################################################


#data partition
set.seed(1234)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]


####################################################
#1-Linear Regression
#####################################################

linear_regression_model <- lm(total_games ~ standard + rapid + blitz+ win_rate + draw_rate + age, data = train)
#prediction of linear regression for train
lrm1 <- predict(linear_regression_model, train)
#prediction of linear regression for test
lrm2 <- predict(linear_regression_model, test)

#Linear Regression plot for train data
plot(train$total_games, lrm1,
     main = "Linear Regression for train data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#Linear Regression plot for test data
plot(test$total_games, lrm2,
     main = "Linear Regression for test data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#model evaluation
library(MLmetrics)
#Machine Learning Evaluation Metrics for train
mse = MSE(train$total_games, lrm1)
mae = MAE(train$total_games, lrm1)
rmse = RMSE(train$total_games, lrm1)
r2 = R2_Score(train$total_games, lrm1)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

#Machine Learning Evaluation Metrics for test
mse = MSE(test$total_games, lrm2)
mae = MAE(test$total_games, lrm2)
rmse = RMSE(test$total_games, lrm2)
r2 = R2_Score(test$total_games, lrm2)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)



#####################################
#2-Regression Tree
#####################################
library(rpart)
regression_tree_model <- rpart(total_games ~ standard + rapid + blitz+ win_rate + draw_rate + age, data = train)
rpart.plot(regression_tree_model)
#prediction of regression tree for train
rtree1 <- predict(regression_tree_model, train)

#prediction of regression tree for test
rtree2 <- predict(regression_tree_model, test)

#Regression tree plot for train data
plot(train$total_games, rtree1,
     main = "Regression tree for train data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#Regression tree plot for test data
plot(test$total_games, rtree2,
     main = "Regression tree for test data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#model evaluation
library(MLmetrics)
#Machine Learning Evaluation Metrics for train
mse = MSE(train$total_games, rtree1)
mae = MAE(train$total_games, rtree1)
rmse = RMSE(train$total_games, rtree1)
r2 = R2_Score(train$total_games, rtree1)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

#Machine Learning Evaluation Metrics for test
mse = MSE(test$total_games, rtree2)
mae = MAE(test$total_games, rtree2)
rmse = RMSE(test$total_games, rtree2)
r2 = R2_Score(test$total_games, rtree2)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)


#############################
#3-Random Forest Regression
#############################

library(randomForest)
random_forest_model <- randomForest(total_games ~ standard + rapid + blitz+ win_rate + draw_rate + age, data = train)
#prediction of random forest for train
rf1 <- predict(random_forest_model, train)
#prediction of random forest for test
rf2 <- predict(random_forest_model, test)

#Random forest plot for train data
plot(train$total_games, rf1,
     main = "Random forest plot for train data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#Random forest plot for test data
plot(test$total_games, rf2,
     main = "Random forest plot for test data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#model evaluation
library(MLmetrics)
#Machine Learning Evaluation Metrics for train
mse = MSE(train$total_games, rf1)
mae = MAE(train$total_games, rf1)
rmse = RMSE(train$total_games, rf1)
r2 = R2_Score(train$total_games, rf1)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

#Machine Learning Evaluation Metrics for test
mse = MSE(test$total_games, rf2)
mae = MAE(test$total_games, rf2)
rmse = RMSE(test$total_games, rf2)
r2 = R2_Score(test$total_games, rf2)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

################################  
#4-Support Vector Regression
################################

library(e1071)
support_vector_machine_model <- svm(total_games ~ standard + rapid + blitz+ win_rate + draw_rate + age, data = train)
#prediction of random forest for train
svm1 <- predict(support_vector_machine_model, train)
#prediction of random forest for test
svm2 <- predict(support_vector_machine_model, test)

#Support vector machine plot for train data
plot(train$total_games, svm1,
     main = "Support vector machine for train data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#Support vector machine plot for test data
plot(test$total_games, svm2,
     main = "Support vector machine for test data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#model evaluation
library(MLmetrics)
#Machine Learning Evaluation Metrics for train
mse = MSE(train$total_games, svm1)
mae = MAE(train$total_games, svm1)
rmse = RMSE(train$total_games, svm1)
r2 = R2_Score(train$total_games, svm1)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

#Machine Learning Evaluation Metrics for test
mse = MSE(test$total_games, svm2)
mae = MAE(test$total_games, svm2)
rmse = RMSE(test$total_games, svm2)
r2 = R2_Score(test$total_games, svm2)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)



##########################################
#5- Artificial Neural Network for Regression
##########################################

library(dplyr)
library(MASS)


data_nn = df %>% dplyr::select(standard, rapid, win_rate, draw_rate, age, total_games)
str(data_nn)

# Import Required packages
set.seed(500)
library(neuralnet)
library(MASS)


# Normalize the data
maxs <- apply(data_nn, 2, max) 
mins <- apply(data_nn, 2, min)
scaled <- as.data.frame(scale(data_nn, center = mins, 
                              scale = maxs - mins))

# Split the data into training and testing set
index <- sample(1:nrow(data_nn), round(0.75 * nrow(data_nn)))
train <- scaled[index,]
test <- scaled[-index,]


# Build Neural Network
nn_regression_model <- neuralnet(total_games ~ standard + rapid + win_rate + draw_rate + age, 
                                 data = train, hidden = 3, 
                                 linear.output = TRUE)

plot(nn_regression_model)


# Predict on train data
nnrm1 <- compute(nn_regression_model, train[,1:5])

# Predict on test data
nnrm2 <- compute(nn_regression_model, test[,1:5])


#Linear Regression plot for train data
plot(train$total_games, nnrm1$net.result,
     main = "ANN for train data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 

#Linear Regression plot for test data
plot(test$total_games, nnrm2$net.result,
     main = "ANN for test data",
     xlab = "total_games",
     ylab = "Prediction of total_games",
     col = c("red", "blue")) 


#model evaluation
library(MLmetrics)
#Machine Learning Evaluation Metrics for train
mse = MSE(train$total_games, nnrm1$net.result)
mae = MAE(train$total_games, nnrm1$net.result)
rmse = RMSE(train$total_games, nnrm1$net.result)
r2 = R2_Score(train$total_games, nnrm1$net.result)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

#Machine Learning Evaluation Metrics for test
mse = MSE(test$total_games, nnrm2$net.result)
mae = MAE(test$total_games, nnrm2$net.result)
rmse = RMSE(test$total_games, nnrm2$net.result)
r2 = R2_Score(test$total_games, nnrm2$net.result)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)
