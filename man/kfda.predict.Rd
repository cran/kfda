\name{kfda.predict}
\alias{kfda.predict}
\title{Predict Method for Kernel Fisher Discriminant Analysis (KFDA) fit}
\description{Test the testData using KFDA. This function is used after training phase is performed using the kfda function.}
\usage{kfda.predict(object = obj, testData = data)}
\arguments{
 \item{object}{An \code{R} object of class \code{kfda}.}
 \item{testData}{an optional \code{data frame} or \code{matrix} containing the variables in the model. In particular, the order of variables in the data frame must be the same as trainData, and the target value must be removed in advance.}
}
\details{
Since this function inherits \code{KPCA} and \code{LDA}, various learning can be possible by adjusting the hyper-parameters of each function.
}
\value{
The result of performing testData on the KFDA model.
 \item{class}{A class label of testData.}
 \item{posterior}{A posterior probabilities for the classes.}
 \item{x}{The scores of testData on up to \code{kfda} discriminant variables.}

}
\references{
Yang, J., Jin, Z., Yang, J. Y., Zhang, D., and Frangi, A. F. (2004) <DOI:10.1016/j.patcog.2003.10.015>. Essence of kernel Fisher discriminant: KPCA plus LDA. \emph{Pattern Recognition}, 37(10): 2097-2100.
}
\author{
Donghwan Kim\cr
\email{ainsuotain@hanmail.net}
\email{donhkim9714@korea.ac.kr}
\email{dhkim2@bistel.com}
}

\seealso{\code{\link{kfda}}}
\examples{
# data input
data(iris)

# data separation
idx <- sample(1:dim(iris)[1], round(dim(iris)[1]*0.7))
trainData <- iris[idx, ]
testData <- iris[-(idx), -dim(iris)[2]]
testData.Label <- iris[-(idx), dim(iris)[2]]

# training KFDA model
kfda.model <- kfda(trainData = trainData, kernel.name = "rbfdot")

# testing new(test)data by KFDA model
pre <- kfda.predict(object = kfda.model, testData = testData)

# plotting
plot(kfda.model$LDs, col = kfda.model$label, pch = 19, main = "Plot for KFDA")
points(pre$x, col = pre$class, cex = 2)
legend("topleft", legend = c("trainData","testData"), pch = c(19,1))

# prediction result
table(pre$class, (testData.Label))

}

\keyword{kfda}