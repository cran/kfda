kfda <- function(trainData = data, kernel.name = "rbfdot", kpar.sigma = 0.001, threshold = 1e-5){

  kfda.models <- list()
  class(kfda.models) <- "Kernel Fisher Discriminant Analysis"

  # kpca
  kpca.train <- kpca(~.,
                     data = trainData[, -dim(trainData)[2]],
                     kernel = kernel.name,
                     kpar = list(sigma = kpar.sigma),
                     th = threshold
  )

  kpca.rotation.train <- as.data.frame(cbind(kpca.train@rotated, trainData[, dim(trainData)[2]]))
  colnames(kpca.rotation.train)[dim(kpca.rotation.train)[2]] <- c("Y")
  kpca.rotation.train$Y <- trainData[, dim(trainData)[2]]

  # kpca + lda train phase
  lda.rotation.train <- lda(kpca.rotation.train$Y~.,
                            data = kpca.rotation.train)

  LDs <- kpca.train@rotated%*%as.matrix(lda.rotation.train$scaling)
  labels <- trainData[, dim(trainData)[2]]

  kfda.models$kpca.train <- kpca.train
  kfda.models$lda.rotation.train <- lda.rotation.train
  kfda.models$LDs <- LDs
  kfda.models$label <- labels

  return(kfda.models)
}
