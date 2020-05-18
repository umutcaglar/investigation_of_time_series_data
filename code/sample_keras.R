
library(keras)
library(caret)

set.seed(123)

n = 400
s = seq(.1, n / 10, .1)
x1 = s * sin(s / 50) - rnorm(n) * 5 
x2 = s * sin(s) + rnorm(n) * 10
x3 = s * sin(s / 100) + 2 + rnorm(n) * 10
y1 = x1 + x2 + x3 + 2 + rnorm(n) * 2
y2 = x1 + x2 / 2 - x3 - 4 - rnorm(n)

df = data.frame(x1, x2, x3, y1, y2)

plot(s, df$y1, ylim = c(min(df), max(df)), type = "l", col = "blue")
lines(s, df$y2, type = "l", col = "red")
lines(s, df$x1, type = "l", col = "green")
lines(s, df$x2, type = "l", col = "yellow")
lines(s, df$x3, type = "l", col = "gray")

indexes = createDataPartition(df$x1, p = .85, list = F)
train = df[indexes,]
test = df[-indexes,]

xtrain = as.matrix(data.frame(train$x1, train$x2, train$x3))
ytrain = as.matrix(data.frame(train$y1, train$y2))
xtest = as.matrix(data.frame(test$x1, test$x2, test$x3))
ytest = as.matrix(data.frame(test$y1, test$y2))

in_dim = dim(xtrain)[2]
out_dim = dim(ytrain)[2] 

model = keras_model_sequential() %>%
    layer_dense(units = 100, activation="relu", input_shape=in_dim) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = out_dim, activation = "linear")

model %>% compile(
    loss = "mse",
    optimizer = "adam")

model %>% summary()

model %>% fit(xtrain, ytrain, epochs = 100, verbose = 0)
scores = model %>% evaluate(xtrain, ytrain, verbose = 0)
print(scores)

ypred = model %>% predict(xtest)

cat("y1 RMSE:", RMSE(ytest[, 1], ypred[, 1]))
cat("y2 RMSE:", RMSE(ytest[, 2], ypred[, 2]))

x_axes = seq(1:length(ypred[, 1]))

plot(x_axes, ytest[, 1], ylim = c(min(ypred), max(ytest)),
     col = "burlywood", type = "l", lwd = 2)
lines(x_axes, ypred[, 1], col = "red", type = "l", lwd = 2)
lines(x_axes, ytest[, 2], col = "gray", type = "l", lwd = 2)
lines(x_axes, ypred[, 2], col = "blue", type = "l", lwd = 2)
legend("topleft", legend = c("y1-test", "y1-pred", "y2-test", "y2-pred"),
       col = c("burlywood", "red", "gray", "blue"),
       lty = 1, cex = 0.9, lwd = 2, bty = 'n')