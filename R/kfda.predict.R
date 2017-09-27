kfda.predict <- function(object = obj, testData = data){

  # kpca
  predict.kpca <- predict(object = object$kpca.train,
                          testData
  )

  # kpca + lda = kfda
  predicted.dataPoints <- as.matrix(predict.kpca)%*%as.matrix(object$lda.rotation.train$scaling)
  pre <- predict(object = object$lda.rotation.train,
                 newdata = as.data.frame(predict.kpca)
  )

  return(pre)

}
