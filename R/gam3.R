#' 3 Stage Framework for GAM
#'
#' Executes the 3 stage framework for GAM.  This function uses a modified version of \code{cv.gglasso}
#' from the \code{gglasso} package, and GAM functions from the \code{mgcv} package.
#'
#' @param formula A GAM formula, as used in \code{\link{gam}}.
#' @param data A dataframe containing the variables in the model.
#' @param gam.function A character object that indicates which GAM function should be used.  Options are
#' \code{"gam"} and \code{"bam"}.  See the \code{\link{mgcv}} package for more details.
#' @param pred.loss See \code{\link{cv.gglasso}} for details.
#' @param nfolds A vector of length 2 providing the number of folds for cross-validtion in stage 1 and stage 2.
#' @param foldid A list of length 2 providing the folds for each observations for cross-validation in stage 1 and stage 2.
#' @param loss See \code{\link{gglasso}} for details.
#' @param nlambda A vector of length 2 providing the number of lambda values to use in stage 1 and stage 2.
#' @param lambda.factor A numeric value specifying the \code{lambda.factor}.  See \code{\link{gglasso}} for details.
#' @param lambda A list of length 2 providing the lambda sequences to use for stage 1 and stage 2.
#' @param pf A list of length 2 providing the penalty factors to use for stage 1 and stage 2.
#' @param eps A vector of length 2 for stage 1 and stage 2.  See \code{\link{gglasso}} for details.
#' @param maxit A vector a length 2 for stage 1 and stage 2. See \code{\link{gglasso}} for details.
#' @param intercept Should the intercept be included in the model.
#' @param ... Additional parameters to pass to \code{gam.function}
#'
#' @details Executes the 3 stage framework for GAM.  This function uses a modified version of \code{cv.gglasso}
#' from the \code{gglasso} package, and GAM functions from the \code{mgcv} package.
#'
#'
#' @return An object of class \code{"gam"} as described in \code{\link{gamObject}}.
#'
#' @seealso \code{\link[gglasso]{gglasso}}, \code{\link[mgcv]{gam}}, \code{\link[mgcv]{bam}}
#'
#' @import gglasso mgcv
#' @importFrom stats as.formula
#'
#' @references Wood, S.N. (2017) \emph{Generalized Additive Models: an introduction with R (2nd edition)}, CRC
#' @references Yang, Y. and Zou, H. (2015),\emph{A Fast Unified Algorithm for Computing Group-Lasso Penalized Learning Problems}. Statistics and Computing. 25(6), 1129-1141.
#'
#' @examples
#' require(mgcv)
#' require(gglasso)
#'
#' probs <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
#' simulated_data <- gamSim2(n = 1000, p = 200, test.n = 200, sigma = 0.1, probs = probs)
#' data <- simulated_data$data
#' m <- 2
#' k <- 8
#'
#' terms <- paste("s(", names(data)[-1], ", bs = 'ps', ", "m = ", m, ", k = ", k, ")", sep = "")
#' formula <- as.formula(paste("y ~", paste(terms, collapse = " + ")))
#' lambda.factor <- 0.0001
#' gam3.mod <- gam3(formula, data, gam.function = "bam", lambda.factor = lambda.factor)
#'
#' @export

gam3 <- function(formula, data, gam.function = "gam",
                 pred.loss = "L2", nfolds = c(5, 5), foldid, loss = "ls",
                 nlambda = c(100, 100), lambda.factor = ifelse(nobs < nvars, 0.05, 0.001),
                 lambda = list(NULL, NULL), pf = list(sqrt(bs), sqrt(bs)), eps = c(1e-08, 1e-08),
                 maxit = c(3e+08, 3e+08), intercept = TRUE, ...) {


  message("Starting stage 1")
  gp <- interpret.gam(formula)
  nvars <- length(gp$smooth.spec)

  Design <- NULL
  group <- NULL
  for (i in 1:length(gp$smooth.spec)) {
    sm <- smoothCon(gp$smooth.spec[[i]], data = data, absorb.cons = TRUE)[[1]]
    Design <- cbind(Design, sm$X)
    group <- c(group, rep(i, ncol(sm$X)))
  }

  bs <- as.integer(as.numeric(table(group)))

  stage1.cv <- cv.gglasso2(x = Design, y = unlist(data[gp$response]),
                           group = group, nfolds = nfolds[1],
                           pred.loss = pred.loss, loss = loss,
                           lambda.factor = lambda.factor,
                           nlambda = nlambda[1],
                           pf = pf[[1]], eps = eps[1], maxit = maxit[1],
                           intercept = intercept,
                           lambda = lambda[[1]])
  stage1.min <- gglasso(x = Design, y = unlist(data[gp$response]),
                        group = group,
                        loss = loss, pf = pf[[1]],
                        eps = eps[1], maxit = maxit[1],
                        intercept = intercept,
                        lambda = stage1.cv$lambda.min)


  message("Starting stage 2")

  weights <- stage1.min$beta
  Design2 <- lapply(1:length(gp$smooth.spec), function(i) {
    Design[, which(group == i)] * sqrt(sum(weights[which(group == i)]^2))
  })
  Design2 <- as.matrix(do.call(cbind, Design2))

  stage2.cv <- cv.gglasso2(x = Design2, y = unlist(data[gp$response]),
                           group = group, nfolds = nfolds[2],
                           pred.loss = pred.loss, loss = loss,
                           lambda.factor = lambda.factor,
                           nlambda = nlambda[2],
                           pf = pf[[2]], eps = eps[2], maxit = maxit[2],
                           intercept = intercept,
                           lambda = lambda[[2]])
  stage2.min <- gglasso(x = Design2, y = unlist(data[gp$response]),
                        group = group,
                        loss = loss, pf = pf[[2]],
                        eps = eps[2], maxit = maxit[2],
                        intercept = intercept,
                        lambda = stage2.cv$lambda.min)


  message("Starting stage 3")

  zero <- unlist(lapply(1:length(gp$smooth.spec), function(i) {
    sum(stage2.min$beta[group == i] != 0) == 0
  }))
  survived <- (1:length(gp$smooth.spec))[!zero]

  terms <- unlist(strsplit(as.character(formula)[3], split = "\\+"))[survived]
  gam.formula <- as.formula(paste(gp$response, "~", paste(terms, collapse = "+")))

  gam.fun <- get(gam.function)
  stage3 <- gam.fun(gam.formula, data = data, ...)
  return(stage3)
}



# plot(simulated_data$test[, 1], predict(gam3.mod, newdata = simulated_data$test))
# plot(simulated_data$test[, 1], simulated_data$test[, 1] - predict(gam3.mod, newdata = simulated_data$test))
# mean((simulated_data$test[, 1] - predict(gam3.mod, newdata = simulated_data$test))^2)




