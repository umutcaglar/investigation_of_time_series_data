library(keras)

mnist <- dataset_mnist()
mnist$train$x <- mnist$train$x / 255
mnist$test$x <- mnist$test$x / 255

model <- keras_model_sequential() %>%
    layer_flatten(input_shape = c(28, 28)) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(10, activation = "softmax")

model %>%
    compile(loss = "sparse_categorical_crossentropy",
            optimizer = "adam",
            metrics = "accuracy")

print_dot_callback <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
        if (epoch %% 10 == 0) cat("\n")
        cat(".")
    }
)    

model %>%
    fit(
        x = mnist$train$x,
        y = mnist$train$y,
        epochs = 5,
        validation_split = 0.3,
        verbose = 0,
        callbacks = list(print_dot_callback)
    )