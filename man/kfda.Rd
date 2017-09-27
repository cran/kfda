\name{kfda}
\alias{kfda}
\title{Kernel Fisher Discriminant Analysis (KFDA)}
\description{Train the trainData using KFDA. Basically, we run KFDA using Gaussian kernel. Returns trained KFDA object.}
\usage{
kfda(trainData = data, kernel.name = "rbfdot", kpar.sigma = 0.001, threshold = 1e-05)
}
\arguments{
 \item{trainData}{an optional \code{data frame} or \code{matrix} containing the variables in the model. In particular, the last column of the data frame should contain the target value.
}
 \item{kernel.name}{the kernel function used in training and predicting. This parameter is fixed in the \code{rbfdot}(Gaussian kernel).}
 \item{kpar.sigma}{
hyper-parameter of selected kernel. \code{sigma} inverse kernel width for the Gaussian kernel function "rbfdot".
}
 \item{threshold}{
the value of the eigenvalue under which principal components are ignored (only valid when features = 0). (default : 1e-05).
}
}
\details{
Train the trainData using KFDA. Basically, we run KFDA using Gaussian kernel. Returns trained KFDA object.
Since this function performs KFDA with the appropriate combination of \code{kpca} and \code{lda}, the following values can show the result of each function.
}
\value{
An object of class \code{kfda}.

 \item{kpca.train}{An object of class "kpca". It has results of \code{kpca} function. (see{\code{\link[kernlab]{kpca}}} (in package \pkg{kernlab}))}
 \item{lda.rotation.train}{The result of applying LDA, After KPCA is performed on trainData.}
 \item{LDs}{A dataframe of linear discriminants of LDA.}
 \item{label}{A vector of class label of trainData.}

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
\note{
This package is an early version and will be updated in the future.
}

\seealso{
 \code{\link[kernlab]{kpca}} (in package \pkg{kernlab})
 \code{\link[MASS]{lda}} (in package \pkg{MASS})
 \code{\link{kfda.predict}}
}
\examples{
# data input
data(iris)

# data separation
idx <- sample(1:dim(iris)[1], round(dim(iris)[1]*0.7))
trainData <- iris[idx, ]

# training KFDA model
kfda.model <- kfda(trainData = trainData, kernel.name = "rbfdot")

# structure of kfda.model
str(kfda.model)
}
\keyword{kfda}
