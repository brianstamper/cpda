# TODO: parameterize a lot more things (sizes of layers, etc.)

# Read partial MNIST set from assignment 3
read_matrix <- function(fn, ...) as.matrix(read.table(fn, sep = ';', ...))
image_data <- read_matrix('data/image_data.txt')
image_labels <- read_matrix('data/labels.txt')


## Attempt version 1 - in this case the dimensions don't line up correctly

# Function definitions
alpha <- function(k) min(1, 20 / (15 + k))
sigmoid <- function(z) 1 / (1 + exp(-z))
grad_sigmoid <- function(z) diag(as.vector(sigmoid(z) * (1 - sigmoid(z))))

## Step 1
# Initialize weights and biases. Note that I do not store this for 
# each k, I just edit them in place
# (so W^1_{k+1} := W^1_k - alpha_k * ... just looks like W1 <- W1 - alpha(k) * ...)
W1 <- matrix(runif(20*400), nrow = 20, ncol = 400)
b1 <- matrix(runif(20*1), nrow = 20, ncol = 1)
W2 <- matrix(runif(5*20), nrow = 5, ncol = 20)
b2 <- matrix(runif(5*1), nrow = 5, ncol = 1)


# The following line is used for testing, defines a k to use when I skip
# for-loop line and run the rest one line at a time.
k <- 0
number_of_iterations <- 2500
for(k in (1:number_of_iterations) - 1) {
  
  ## Step 2
  # Sample an image - later this will be expanded to "mini-batch" case
  sample_of_images <- sample(nrow(image_data), 1)
  x_i <- t(image_data[sample_of_images, , drop = FALSE])
  y_i <- matrix(0, nrow = 5, ncol = 1)
  y_i[image_labels[sample_of_images], ] <- 1

  # Forward propagation (with T = 3)
  s1 <- x_i             # s1 is 400 x 1
  z2 <- W1 %*% s1 + b1  # z2 is 20 x 1
  s2 <- sigmoid(z2)     # s2 is 20 x 1
  zT <- W2 %*% s2 + b2  # zT is 5 x 1
  sT <- sigmoid(zT)     # sT is 5 x 1
  
  ## Step 3
  # Backward propagation
  error <- sT - y_i                      # error is 5 x 1
  delta_T <- grad_sigmoid(zT) %*% error  # delta_T is 5 x 1
  grad_W2E <- delta_T %*% t(s2)          # grad_W2E is 5 x 5 <-- surely this is not correct
  grad_b2E <- delta_T                    # grad_b2E is 5 x 1
  
  delta_2 <- grad_sigmoid(z2) %*% t(W2) %*% delta_T   # delta_2 is 20 x 1
  grad_W1E <- delta_2 %*% t(s1)                       # grad_W1E is 20 x 20
  grad_b1E <- delta_2                                 # grad_b1E is 20 x 1
 
  ## Step 4 
  # Update W* and b*
  W2 <- W2 - alpha(k) * grad_W2E 
  b2 <- b2 - alpha(k) * grad_b2E
  W1 <- W1 - alpha(k) * grad_W1E 
  b1 <- b1 - alpha(k) * grad_b1E
  
}







## Attempt version 2 - dimensions line up, but not sure if correct

alpha <- function(k) min(1, 20 / (15 + k))
sigmoid <- function(z) 1 / (1 + exp(-z))
# using Sk(1-Sk) instead: grad_sigmoid <- function(z) diag(as.vector(sigmoid(z) * (1 - sigmoid(z))))

W1 <- matrix(runif(20*400), nrow = 20, ncol = 400)
b1 <- matrix(runif(20*1), nrow = 20, ncol = 1)
W2 <- matrix(runif(5*20), nrow = 5, ncol = 20)
b2 <- matrix(runif(5*1), nrow = 5, ncol = 1)



number_of_iterations <- 2500
for(k in (1:number_of_iterations) - 1) {
 sample_of_images <- sample(nrow(image_data), 1)
 x_i <- t(image_data[sample_of_images, , drop = FALSE])
 y_i <- matrix(0, nrow = 5, ncol = 1)
 y_i[image_labels[sample_of_images], ] <- 1

 # Forward propagation (with T = 3)
 s1 <- x_i
 z2 <- W1 %*% s1 + b1
 s2 <- sigmoid(z2)
 zT <- W2 %*% s2 + b2
 sT <- sigmoid(zT)
 
 # Backward propagation
 # not really used: error <- 0.5 * (sT - y_i) ^ 2
 grad_error <- sT - y_i
 
 delta_T <- sT * (1 - sT) * grad_error
 grad_W2E <- delta_T %*% t(s2)
 grad_b2E <- delta_T 
 
 delta_2 <- s2 * (1 - s2) * t(W2) %*% delta_T
 grad_W1E <- delta_2 %*% t(s1)
 grad_b1E <- delta_2
 
 # Update W* and b*
 W2 <- W2 - alpha(k) * grad_W2E 
 b2 <- b2 - alpha(k) * grad_b2E
 W1 <- W1 - alpha(k) * grad_W1E 
 b1 <- b1 - alpha(k) * grad_b1E
 
}








## Attempt version 3 - one big "mini"-batch per k (still not SGD, but moving in right direction?)

alpha <- function(k) min(1, 5 / (1 + k))
sigmoid <- function(z) 1 / (1 + exp(-z))
# using Sk(1-Sk) instead: grad_sigmoid <- function(z) diag(as.vector(sigmoid(z) * (1 - sigmoid(z))))

W1 <- matrix(runif(20*400), nrow = 20, ncol = 400)
b1 <- matrix(runif(20*1), nrow = 20, ncol = 1)
W2 <- matrix(runif(5*20), nrow = 5, ncol = 20)
b2 <- matrix(runif(5*1), nrow = 5, ncol = 1)


number_of_iterations <- 2500
sample_proportion <- 0.1

for(k in (1:number_of_iterations) - 1) {
  sample_of_images <- sample(nrow(image_data), ceiling(sample_proportion * nrow(image_data)), replace = TRUE)
  N <- length(sample_of_images)
  wide_vector <- function(v) matrix(v, nrow = nrow(v), ncol = N)
  row_means <- function(mat) .rowMeans(mat, m = nrow(mat), n = N)
  
  x_i <- t(image_data[sample_of_images, , drop = FALSE])
  y_i <- matrix(0, nrow = 5, ncol = N)
  y_i[cbind(image_labels[sample_of_images], 1:N)] <- 1
  

  # Forward propagation (with T = 3)
  s1 <- x_i
  z2 <- W1 %*% s1 + wide_vector(b1)
  s2 <- sigmoid(z2)
  zT <- W2 %*% s2 + wide_vector(b2)
  sT <- sigmoid(zT)
  
  # Backward propagation
  # not really used: error <- 0.5 * (sT - y_i) ^ 2
  grad_error <- row_means(sT - y_i)

  ## problems arise here with the biases, not sure what to do here
  delta_T <- sT * (1 - sT) * grad_error
  grad_W2E <- delta_T %*% t(s2)
  grad_b2E <- row_means(delta_T)
  
  delta_2 <- s2 * (1 - s2) * t(W2) %*% delta_T
  grad_W1E <- delta_2 %*% t(s1)
  grad_b1E <- row_means(delta_2)
  
  # Update W* and b*
  W2 <- W2 - alpha(k) * grad_W2E 
  b2 <- b2 - alpha(k) * grad_b2E
  W1 <- W1 - alpha(k) * grad_W1E 
  b1 <- b1 - alpha(k) * grad_b1E

  message(sum(grad_error^2))
}




