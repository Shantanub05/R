#linear Reg
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

relation<-lm(y~x)
summary(relation)

a<-data.frame(x=162)
result<-predict(relation,a)
print(result)
# Plot the chart.
plot(y,x,col = "blue",main = "Height & Weight Regression", abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

#logistic Reg
# Select some columns form mtcars.
input <- mtcars[,c("am","cyl","hp","wt")]
View(mtcars)
print(head(input))

input <- mtcars[,c("am","cyl","hp","wt")]

am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)

print(summary(am.data))

#naive bayes
# Loading data
data(iris)

# Structure
str(iris)

# Installing Packages
install.packages("e1071",dependencies = TRUE)
install.packages("caTools" )
install.packages("caret", dependencies = TRUE)
#install.packages("Rcpp")

# Loading package
library(e1071)
library(caTools)
#library(Rcpp)
library(caret)

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

classifier_cl<-naiveBayes(Species ~ ., data = train_cl)
# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Species, y_pred)
cm

#knn classification


# Loading package
library(e1071)
library(caTools)
library(class)

# Loading data
data(iris)
head(iris)

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 1)

classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Species, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))


#decision tree
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
data("readingSkills")
head(readingSkills)



sample_data = sample.split(readingSkills, SplitRatio = 0.8)

train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)

model<- ctree(nativeSpeaker ~ ., train_data)
plot(model)


# testing the people who are native speakers
# and those who are not
predict_model<-predict(model, test_data)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_data$nativeSpeaker, predict_model)
m_at

test=sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', test))

#k means clustering
# Installing Packages
install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)
# Removing initial label of 
# Species from original dataset
View(iris)
iris_1 <- iris[, -5]
# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster
# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

# Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 3) 

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans)

#time series
# Load necessary libraries and the dataset
install.packages("forecast")

library(forecast)
data("AirPassengers")

# Convert the dataset to a time series object
ts_data <- ts(AirPassengers, frequency = 12)

# Plot the time series data
plot(ts_data, main = "AirPassengers Time Series", ylab = "Passenger Count")

# Decompose the time series into trend, seasonality, and remainder
decomposed_ts <- decompose(ts_data)

# Plot the decomposed components
plot(decomposed_ts)

# Fit a simple exponential smoothing model
ets_model <- ets(ts_data)

# Make future predictions using the forecast function
forecast_values <- forecast(ets_model, h = 24)  # Predicting the next 24 time points

# Plot the original time series and the forecast
plot(ts_data, main = "AirPassengers Time Series with Forecast", ylab = "Passenger Count")
lines(forecast_values$mean, col = "red", lty = 2)
legend("topright", legend = c("Original", "Forecast"), col = c("black", "red"), lty = c(1, 2))

#multiple reg
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))

# Create the relationship model.
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(model)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(Xdisp)
print(Xhp)
print(Xwt)


#Y = a+Xdisp.x1+Xhp.x2+Xwt.x3
#or
#Y = 37.15+(-0.000937)*x1+(-0.0311)*x2+(-3.8008)*x3
#For a car with disp = 221, hp = 102 and wt = 2.91 the predicted mileage is −
#Y = 37.15+(-0.000937)*221+(-0.0311)*102+(-3.8008)*2.91 = 22.7104

#Creating a panel of different plots
library(ggplot2)
library(gridExtra)

# Selecting specific columns from mtcars dataset
selected_cols <- c("mpg", "disp", "hp", "drat")
selected_data <- mtcars[, selected_cols]

# Create histograms for individual variables
hist_plot_mpg <- ggplot(selected_data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "white") +
  labs(title = "Histogram: Miles per Gallon", x = "Miles per Gallon", y = "Frequency")

hist_plot_disp <- ggplot(selected_data, aes(x = disp)) +
  geom_histogram(binwidth = 50, fill = "red", color = "white") +
  labs(title = "Histogram: Displacement", x = "Displacement", y = "Frequency")

hist_plot_hp <- ggplot(selected_data, aes(x = hp)) +
  geom_histogram(binwidth = 20, fill = "green", color = "white") +
  labs(title = "Histogram: Horsepower", x = "Horsepower", y = "Frequency")

hist_plot_drat <- ggplot(selected_data, aes(x = drat)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "white") +
  labs(title = "Histogram: Drat", x = "Drat", y = "Frequency")

# Arrange the plots in a grid
grid.arrange(hist_plot_mpg, hist_plot_disp, hist_plot_hp, hist_plot_drat,
             ncol = 2)

#scatter 
# Geometric layer
ggplot(data = mtcars, aes(x = hp, y = mpg, col = disp)) +
  geom_point() +
  labs(title = "Miles per Gallon vs Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")

# Histogram plot
ggplot(data = mtcars, aes(x = hp)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Horsepower",
       x = "Horsepower",
       y = "Count")

#bar
ggplot(mtcars, aes(x = gear)) +geom_bar()

#box
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot() 

#Correlogram
library(ggcorrplot)
data(mtcars)
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

#heatmap
ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_hex(bins = 20, color = "grey") 
+ scale_fill_distiller(palette = "Spectral", direction = 1)

Modify Column Names
colnames(data) <- paste0("col", 1:ncol(data))             # Modify all column names
data

Format Missing Values
data[data == ""]
data[data == ""] <- NA

# Drop empty rows
Remove Empty Rows & Columns
data <- data[rowSums(is.na(data)) != ncol(data), ]       

data <- data[ , colSums(is.na(data)) != nrow(data)]       # Drop empty columns
data


Remove Duplicates
data <- unique(data)
