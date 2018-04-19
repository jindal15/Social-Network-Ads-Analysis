# K-Nearest Neighbors (K-NN)

#Loading the dataframe for social network ads


data_frame = read.csv('Social_Network_Ads.csv')

data_frame = data_frame[3:5]

# Encoder functions for special features
data_frame$Purchased = factor(data_frame$Purchased, levels = c(0, 1))

# creating sets of training and testing data_frame

#Using eror Handeling using tryCatch function
tryCatch(library(caTools),finally=install.packages('caTools'))
try(library(caTools))

set.seed(123)

split = sample.split(data_frame$Purchased, SplitRatio = 0.75)

trainset = subset(data_frame, split == TRUE)

testingset = subset(data_frame, split == FALSE)

# data scaling for faster processing
trainset[-3] = scale(trainset[-3])

testingset[-3] = scale(testingset[-3])

# Fitting K-NN to the Training set and Predicting the Test set results

tryCatch(library(class),finally = install.packages("class"))
try(library(class))

new_y = knn(train = trainset[, -3],
             test = testingset[, -3],
             cl = trainset[, 3],
             k = 5,
             prob = TRUE)

# Confusion Matrix
confusion_matrix = table(testingset[, 3], new_y)

# Visualising the Training set results
tryCatch(library(ElemStatLearn),finally = install.packages("ElemStatLearn"))
try(library(ElemStatLearn))

set = trainset

x_1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)

x_2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_value = expand.grid(x_1, x_2)

colnames(grid_value) = c('Age', 'EstimatedSalary')

y_grid_value = knn(train = trainset[, -3], test = grid_value, cl = trainset[, 3], k = 5)

plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x_1), ylim = range(x_2))

contour(x_1, x_2, matrix(as.numeric(y_grid_value), length(x_1), length(x_2)), add = TRUE)

points(grid_value, pch = '.', col = ifelse(y_grid_value == 1, 'springgreen3', 'tomato'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)

set = testingset

x_1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)

x_2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_value = expand.grid(x_1, x_2)

colnames(grid_value) = c('Age', 'EstimatedSalary')

y_grid = knn(train = trainset[, -3], test = grid_value, cl = trainset[, 3], k = 5)

plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x_2), ylim = range(x_2))

contour(x_1, x_2, matrix(as.numeric(y_grid), length(x_1), length(x_2)), add = TRUE)

points(grid_value, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
