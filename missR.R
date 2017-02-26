# Helper functions for the face analysis of Miss Finland Beauty competition
# Tuomo Nieminen 2016

# Allows the estimation of predictive linear models for classification and regression, 
# such as L1- or L2-regularized logistic regression
library(LiblineaR)


# center faces
center_faces <- function(faces) {
  faces - meanface(faces)
}


# do pca on faces (TODO: could just use prcomp() instead ... done in index.Rmd)
faces.PCA <- function(faces) {
  centered_faces = center_faces(faces)
  covariance <- cov(centered_faces)
  eigen(covariance, symmetric=T)
}

project_face <- function(face, PC, which = 1:2) {
  V <- PC[,which]
  P <- V%*%t(V)
  t(P%*%face)
}

get_randomface <- function(faces, size=nrow(faces)) {
  i <- sample(1:nrow(faces), size = size, replace = F)
  faces[i,, drop = F]
}

# draws both projected and original face
compare_faces <- function(faces, PC, which = 1, n = 1) {
  faces <- get_randomface(faces, size = n)
  projected_faces <- t(apply(faces, 1, project_face, PC = PC, which = which))
  both_faces <- matrix(NA, ncol = ncol(faces), nrow = n*2)
  both_faces[((0:(n-1))*2) + 1,] <- faces
  both_faces[((0:(n-1))*2) + 2,] <- projected_faces
  rownames(both_faces) <- sapply(rownames(faces), rep, times = 2)
  drawMultipleFaces(both_faces)
}

# draw a sample of faces
drawSample<-function(faces, n=16, dev = NULL) {
  I <- sample(1:nrow(faces),size=n,replace=F)
  todraw <- faces[I,, drop = F]
  drawMultipleFaces(todraw, dev = dev)
}

# draw a face
drawFace <- function(face, newdev=F, main = rownames(face)) {
  ##Draws the argument face in greyscale.
  ##
  ##Args:
  ##  face: a vector containing the face as a greyscale image.
  ##  newdev: open a new device?
  m <- as.numeric(face)
  dim <- sqrt(length(m))
  m<- matrix(m, dim, dim)
  m <- t(m[dim:1, ])
  if(newdev) dev.new()
  image(m, col=grey(seq(0, 1, length=256)), main=main, cex.main = 0.8)
}

# draw multiple faces
drawMultipleFaces <- function(faces, dev = NULL, titles = rownames(faces), cex = 1) {
  
  # Applies drawFace() to each row of faces
  ##Args:
  ##  faces: a matrix or data frame with greyscale faces as rows.
  
  nfaces <- nrow(faces)
  pardim <- ceiling(sqrt(nfaces))
  if(!is.null(dev)) dev()
  temp_par <- par(mfrow=c(pardim,pardim), yaxt="n", xaxt="n",  mar=c(0,0.5,2,0), cex = cex)
  i <- 1
  for(i in 1:nfaces) {
    drawFace(faces[i,],main=titles[i])
    i <- i + 1
  }
  par(temp_par)
}


# test different logistic regression models 
# on a grid of regularization costs and dimensions of eigenfaces
# returns a matrix of average correct predictions (see cross_validate below)
cross_validate_grid <- function(data, PC, C, FD, target="Kolme") {
  results <- sapply(C, function(c) {
    res <- sapply(FD, function(fd) {
      cross_validate(data=data, PC = PC,cost = c, facedim = fd,type = 6, target = target)
    })
    # cat(100*which(C==c)/length(C),"% done \n")
    res
  })
  results
}

# predicts the top3 of each competition year by using data from other years.
# returns  average prct of correct predictions
cross_validate <- function(data, PC, cost, facedim, type=6, target="Kolme") {
  
  nwinners <- ifelse(target=="Kolme",3,1)
  V <- PC[,1:facedim]
  faces <- as.matrix(data[,19:ncol(data)])
  newfaces <- faces %*% V
  X <- cbind(data[,c(2,8:18)], newfaces)
  Y <- factor(data[[target]])
  
  years <- unique(data$year)
  pred <- sapply(years, function(year) {
    train <- data$year != year
    X_tr <- X[train,]; X_te <- X[!train,]
    Y_tr <- Y[train]; Y_te <- Y[!train]
    
    winners <- which(Y_te=="1")
    if(length(winners)==0)  {
      return(c(correct = 0))
    }
    
    fit <- LiblineaR::LiblineaR(data=X_tr, target=Y_tr, type=type, 
                                cost=cost, wi=c("0"=(10-nwinners)/10,"1"=nwinners/10))
    
    pred <- predict(fit, newx = X_te, proba=T)
    p7 <- pred$probabilities[,1]
    
    # add comperitor indexes
    p7 <- cbind(p7,1:nrow(X_te))
    p7 <- p7[order(p7[,1]),]
    
    # + 1 to make the predictions a little easier...
    c(correct=sum(p7[,2][1:(nwinners+1)] %in% winners)/length(winners))
  })
  prc_correct <- sum(pred)/length(years)
  prc_correct
}

# predict future results using previous data and several model parameters
# model parameters are chosen by cross validation (see above)
fit_l1_logreg <- function(tr_data, PC, costs, facedims, target_data, target="Kolme") {
  nwinners <- ifelse(target=="Kolme",3,1)
  Y <- tr_data[[target]]
  X <- tr_data[,c(2,8:18), drop = F]
  X_ <- target_data[,c(2,8:18)]
  
  W <- matrix(0, ncol = ncol(X) + max(facedims) + 1, nrow = length(costs)) 
  colnames(W) <-  c(colnames(X), paste("PC", 1:max(facedims)), "bias")
  i <- 0
  
  probs <- mapply(function(c,fd) {
    V <- PC[,1:fd, drop = F]
    faces <- as.matrix(tr_data[,19:ncol(tr_data)]) %*% V
    X_tr <- as.matrix(cbind(X, faces))
    fit <- LiblineaR(data = X_tr, target = Y, type=6, 
                     cost = c, wi = c("0" = (10 - nwinners) / 10, "1" = nwinners/10))
    W[(i <- i + 1), 1:length(fit$W)] <<- fit$W

    target_faces <- as.matrix(target_data[,19:ncol(target_data)]) %*% V
    X_target <- cbind(X_, target_faces)
    pred <- predict(fit, newx = as.matrix(X_target), proba = T)
    pred$probabilities[,2]
  }, costs, facedims)
  return(list(weights = W, probs = probs))
}