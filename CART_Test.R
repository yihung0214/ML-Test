##################### Load Packages (For Analysis) ####################
## libraries Path
.libPaths()
.libPaths("C:/Program Files/R/R-4.4.1/library")
.libPaths()

## Load Packages 
install.packages(c("rpart", "rpart.plot", "caTools", "ggplot2", "GGally", "gridExtra"))
library(rpart)  # For decision tree model
library(rpart.plot)  # For data visualization
library(caTools)  # For data splitting
library(ggplot2)  # For data visualization
library(GGally) # For Scatter Plot
library(gridExtra) # For Plot Arrange

#############################################################################
## Classification and Regression Tree
## Iris Data (For Classification)
## https://search.r-project.org/CRAN/refmans/MVTests/html/iris.html
#############################################################################

## Descriptive Statistics
data("iris")
dim(iris)

summary(iris)
str(iris)
head(iris, n=10)

boxplot(iris[, -5], col = c("red", "blue", "green", "yellow"), 
        main = "Boxplot of Iris Data")

ggpairs(iris,
        columns = 1:4,
        aes(color = Species, alpha = 0.7)) +
        theme_bw() +
        theme(legend.position = "right") 

p1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Sepal Length") +
  theme_bw()
p2 <- ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Sepal Width") +
  theme_bw()
p3 <- ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Petal Length") +
  theme_bw()
p4 <- ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot(aes(fill = Species)) +
  labs(title = "Petal Width") +
  theme_bw()
windows()
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

## Classification Tree
fit.iris <- rpart(Species ~ ., data = iris)
rpart.plot(fit.iris, extra = 101)


#############################################################################
## Classification and Regression Tree
## Boston Housing Data (For Regression)
## https://search.r-project.org/CRAN/refmans/mlbench/html/BostonHousing.html
#############################################################################
install.packages("mlbench") # For Data
library("mlbench") # For Data
data("BostonHousing", package = "mlbench")
dim(BostonHousing)
summary(BostonHousing)
str(BostonHousing)
head(BostonHousing, n=10)

## Training Data and Testing Data
split <- sample.split(BostonHousing, SplitRatio = 0.8)
train_data <- subset(BostonHousing, split == "TRUE")
test_data <- subset(BostonHousing, split == "FALSE")
dim(train_data)
dim(test_data)

########################################################################
## Multiple Linear Regression
lm.fit <- glm(medv ~ .,
          family= gaussian(link = "identity"),
          data = train_data)
?glm
summary(lm.fit)

pred.lm.fit = predict(lm.fit, test_data)
mse.lm.fit <- mean((pred.lm.fit - test_data$medv)^2)
mse.lm.fit

data.frame(pred.medv = pred.lm.fit, true.medv = test_data$medv)
qqplot(pred.lm.fit, test_data$medv, 
       xlim=c(0,50), ylim=c(0,50), 
       xlab="Predicted Values", ylab="Observed Values")
abline(a = 0, b = 1, col = "red", lty = 2)

########################################################################
## Regression Tree or Decision Tree Regression
tree.fit <- rpart(medv ~ .,
               method = "anova",
               data = train_data)
rpart.plot(tree.fit, extra = 101)
tree.fit$variable.importance # The Importance of VCariables

pred.tree.fit = predict(tree.fit, test_data)
mse.tree.fit <- mean((pred.tree.fit - test_data$medv)^2)
mse.tree.fit

data.frame(pred.tree.fit = pred.tree.fit, true.medv = test_data$medv)
qqplot(pred.tree.fit, test_data$medv,
       xlim=c(0,50), ylim=c(0,50), 
       xlab="Predicted Values", ylab="Observed Values")
abline(a = 0, b = 1, col = "red", lty = 2)
