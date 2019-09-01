library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv("binary.csv", header = T)
# first line as header 

str(data)
xtabs(~admit+rank, data=data)
# cross tabulation (contingency table in frequency-weighted format)
# Use xtabs() when you want to numerically study 
# the distribution of one categorical variable, 
# or the relationship between two categorical variables. 


# converting to factors
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

str(data)


# Visualizing..
pairs.panels(data[-1])

# Boxplots
ggplot(data, aes(x=admit, y=gre, fill = admit)) +
geom_boxplot() +
ggtitle("Box Plot: admit - gre")

ggplot(data, aes(x=admit, y=gpa, fill = admit)) +
  geom_boxplot() +
  ggtitle("Box Plot: admit - gpa")

# Density Plot
ggplot(data, aes(x=gre, fill=admit)) +
geom_density(alpha=0.7, color="black") +
ggtitle("Density Plot: admit - gre")

ggplot(data, aes(x=gpa, fill=admit)) +
geom_density(alpha=0.7, color="black") +
ggtitle("Density Plot: admit - gpa")


# splitting data . .
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# naive bayes
# admit as a function of all the features
model <- naive_bayes(admit ~ ., data=train)
model

summarise(filter(train, admit == "0"), mean(gre), sd(gre))

plot(model)

#predict
#type of predictions -> probability (this will include all the probs)
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# confusion matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))

# misclassifications - train data
1 - sum(diag(tab1)) / sum(tab1)

# confusion matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))

# misclassifications - test data
1 - sum(diag(tab2)) / sum(tab2)