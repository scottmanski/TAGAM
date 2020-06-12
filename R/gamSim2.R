#' Simulated data for GAM's
#'
#' This function creates simulated datasets to illustrate the use of \code{\link{gam3}}.
#'
#' @param n numeric value specifying the number of observations.
#' @param p numeric value specifying the number of covariates.
#' @param test.n numeric value specifying the number of observations in the test set.
#' @param sigma numeric value specifying the standard deviation of the errors.
#' @param probs numeric vector of length 6 specifying the proportion of covariates that are
#' polynomial, linear, exponential, logarithmic, sinusoidal, and zero functions.
#'
#' @details This function creates simulated datasets to illustrate the use of \code{\link{gam3}}.
#' The covariates are sampled uniformly over $[0, 1]$, and their associated functions are randomly
#' chosen from polynomial, linear, exponential, logarithmic, sinusoidal, and zero using weights provided
#' in the \code{probs} argument.
#'
#' @return A named list containing \code{data}, \code{functions}, \code{line}, and \code{test}.
#'   \item{\code{data}}{An \code{n}\eqn{\times} \code{p+1} dataframe containing the simulated data.  The first column contains
#'   the response, with all subsequent columns containing the covariates.}
#'   \item{\code{functions}}{A list of length \code{p} containing the true functions.}
#'   \item{\code{line}}{A character vector of length \code{p} containing the type of function used.}
#'   \item{\code{test}}{An \code{test.n}\eqn{\times} \code{p+1} dataframe containing the simulated test data.  The first column contains
#'   the response, with all subsequent columns containing the covariates.}
#'
#' @importFrom stats runif nobs rnorm
#'
#' @examples
#' set.seed(2018)
#' probs <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
#' simulated_data <- gamSim2(n = 100, p = 1, test.n = 20, sigma = 0.1, probs = probs)
#' # -1*2*x*sin(2.70259540737607*pi * x^2 - 1.3270146084221)
#' sin.fun <- function(x) eval(parse(text = simulated_data$functions[[1]]))
#' truth <- sin.fun(seq(0, 1, length.out = 100))
#' truth <- truth/diff(range(truth))
#' plot(simulated_data$data[, 2], simulated_data$data[, 1])
#' lines(seq(0, 1, length.out = 100), truth, col = "red")
#'
#' @export

gamSim2 <- function(n, p, test.n, sigma, probs) {
  x <- lapply(1:p, function(i) {
    runif(n = n, min = 0, max = 1)
  })
  x <- data.frame(do.call(cbind, x))
  names(x) <- paste("X", 1:p, sep = "")

  x.test <- lapply(1:p, function(i) {
    runif(n = test.n, min = 0, max = 1)
  })
  x.test <- data.frame(do.call(cbind, x.test))
  names(x.test) <- paste("X", 1:p, sep = "")

  x.seq <- seq(0.0000001, 1, length.out = 100)

  functions <- c("poly", "lin", "exp", "log", "sin", "zero")
  truth <- list()
  f <- list()
  plot.seq <- list()
  line <- vector()
  test.f <- list()
  equations <- list()
  for (i in 1:p) {
    func <- sample(functions, 1, prob = probs)
    if (func == "poly") {
      coefs <- sample(-10:10, 11, prob = c(rep(0.03, 10), 0.4, rep(0.03, 10)), replace = TRUE)
      x.order <- sample(c("*x^", "*(1-x)^"), 11, replace = TRUE)
      formula <- paste(coefs, x.order, 0:10, sep = "", collapse = " + ")
    } else if (func == "lin") {
      coefs1 <- sample(c(-10:-1, 1:10), 1, replace = TRUE)
      formula <- paste(coefs1, "*x", sep = "")
    } else if (func == "exp") {
      coefs1 <- sample(c(-10:-1, 1:10), 1, replace = TRUE)
      coefs2 <- runif(1, -2, 2)
      formula <- paste(coefs1, "*exp(x*", coefs2, ")", sep = "")
    } else if (func == "log") {
      coefs <- sample(2:10, 2, replace = TRUE)
      pos <- sign(runif(1, -1, 1))
      formula <- paste(pos, "*", coefs[1], "*log(x*", coefs[2], ")", sep = "")
    } else if (func == "sin") {
      coef1 <- sample(1:10, 1, replace = TRUE)
      coef2 <- runif(1, 1, 4)
      coef3 <- runif(1, 0, pi)
      pos <- sign(runif(1, -1, 1))
      formula <- paste(pos, "*", coef1, "*x*sin(", coef2, "*pi * x^2 - ", coef3, ")", sep = "")
    } else {
      formula <- "0*x"
    }
    equations[[i]] <- formula
    truth[[i]] <- function(x) eval(parse(text = formula))
    y.scale <- diff(range(truth[[i]](x.seq)))
    y.scale <- ifelse(y.scale == 0, 1, y.scale)
    f[[i]] <- truth[[i]](x[, i])/y.scale
    test.f[[i]] <- truth[[i]](x.test[, i])/y.scale
    plot.seq[[i]] <- truth[[i]](x.seq)/y.scale
    line[i] <- func
  }
  y <- apply(do.call(cbind, f), 1, sum) + rnorm(n, 0, sigma)
  y.test <- apply(do.call(cbind, test.f), 1, sum) + rnorm(test.n, 0, sigma)
  list("data" = cbind(y, x),
       "functions" = equations,
       "line" = line,
       "test" = cbind("y" = y.test, x.test))
}


