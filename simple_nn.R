
# Read data from assignment 4
read_matrix <- function(fn, ...) as.matrix(read.table(fn, sep = ';', ...))
image_data <- read_matrix('data/image_data.txt')
image_labels <- read_matrix('data/labels.txt')

set.seed(0)
train <- sample(nrow(image_data), ceiling(.8 * nrow(image_data)))
image_data_train <- image_data[train, ]
image_labels_train <- image_labels[train, ]
image_data_test <- image_data[-train, ]
image_labels_test <- image_labels[-train, ]

# alpha function, try different values for gamma
gamma_1 <- 500
gamma_2 <- 100
alpha <- function(k) min(1, gamma_1 / (gamma_2 + k))

# A couple options for activation function and gradient, change by what is commented out in loop below
sigmoid <- function(z) 1 / (1 + exp(-z))
# grad_sigmoid is simplified to e.g. sT * (1 - sT), as shown later below
relu <- function(z) ifelse(z < 0, .01 * z, .1 * z)
grad_relu <- function(z) ifelse(z < 0, .01, .1)

# Incorporate biases into weights by adding a column
W1 <- matrix(rnorm(120*401), nrow = 120, ncol = 401)
W2 <- matrix(rnorm(10*121), nrow = 10, ncol = 121)
W3 <- matrix(rnorm(5*11), nrow = 5, ncol = 11)

# Try different number of iterations and sample proportions at each step
# For some reason smaller samples work much better
number_of_iterations <- 50000
sample_proportion <- 0.1

for(k in (1:number_of_iterations) - 1) {
  sample_of_images <- sample(nrow(image_data_train),
                             1,# ceiling(sample_proportion * nrow(image_data_train)), 
                             replace = TRUE) # Also can try with FALSE
  n <- length(sample_of_images)
  x_i <- t(image_data_train[sample_of_images, , drop = FALSE])
  y_i <- matrix(0, nrow = 5, ncol = n)
  y_i[cbind(image_labels_train[sample_of_images], 1:n)] <- 1
  
  # Forward propagation (with T = 4)
  s1 <- rbind(1, x_i)
  z2 <- W1 %*% s1
  #s2 <- rbind(1, relu(z2))
  s2 <- rbind(1, sigmoid(z2))
  z3 <- W2 %*% s2
  s3 <- rbind(1, sigmoid(z3))
  zT <- W3 %*% s3
  #sT <- relu(zT)
  sT <- sigmoid(zT)
  
  # Backward propagation
  # This gradient of error is based on sum of squares, other possibilities exist
  grad_error <- .rowMeans(sT - y_i, m = 5, n = n)
  
  #delta_T <- grad_relu(zT) * grad_error
  delta_T <- sT * (1 - sT) * grad_error
  grad_W3E <- delta_T %*% t(s3)
  #delta_3 <- grad_relu(rbind(1, z3)) * t(W3) %*% delta_T
  delta_3 <- s3 * (1 - s3) * t(W3) %*% delta_T
  grad_W2E <- (delta_3 %*% t(s2))[-1,]
  #delta_2 <- grad_relu(rbind(1, z2)) * t(W2) %*% delta_T
  delta_2 <- s2 * (1 - s2) * t(W2) %*% delta_3[-1,]
  grad_W1E <- (delta_2 %*% t(s1))[-1,] # drop gradient on bias row ?
  
  # Update W*
  W3 <- W3 - alpha(k) * grad_W3E 
  W2 <- W2 - alpha(k) * grad_W2E 
  W1 <- W1 - alpha(k) * grad_W1E 
  
  if(k %% 1000 == 0) {
    message('\nk = ', k)
    s1 <- rbind(1, t(image_data_test))
    z2 <- W1 %*% s1
    #s2 <- rbind(1, relu(z2))
    s2 <- rbind(1, sigmoid(z2))
    z3 <- W2 %*% s2
    s3 <- rbind(1, sigmoid(z3))
    zT <- W3 %*% s3
    #sT <- relu(zT)
    sT <- sigmoid(zT)
    
    predicted_labels <- apply(sT, 2, which.max)
    table(predicted_labels)
    message('\nTest set accuracy: ', mean(image_labels_test == predicted_labels))
  }
}


# Check the results with the 'test' data

s1 <- rbind(1, t(image_data_test))
z2 <- W1 %*% s1
#s2 <- rbind(1, relu(z2))
s2 <- rbind(1, sigmoid(z2))
z3 <- W2 %*% s2
s3 <- rbind(1, sigmoid(z3))
zT <- W3 %*% s3
#sT <- relu(zT)
sT <- sigmoid(zT)

predicted_labels <- apply(sT, 2, which.max)
table(predicted_labels)
message('\nFinal test set accuracy: ', mean(image_labels_test == predicted_labels))
