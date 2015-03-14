#' Convert \code{glmnet::glmnet} to \code{data.frame}
#'
#' @param model \code{glmnet::glmnet} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(glmnet::glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
fortify.glmnet <- function(model, data = NULL, ...) {
  beta <- as.matrix(model$beta)
  d <- as.data.frame(t(beta))
  post.fortify(d)
}

#' Autoplot \code{glmnet::glmnet}
#'
#' @param object \code{glmnet::glmnet} instance
#' @param colour Line colour
#' @param linetype Line type
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(glmnet::glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
autoplot.glmnet <- function (x, xvar = c("norm", "lambda", "dev"),
                             label = FALSE, norm = NULL, ...) {
  beta <- as.matrix(x$beta)
  xvar = 'norm'
  switch(xvar, norm = {
    index = if (missing(norm)) apply(abs(beta), 2, sum) else norm
    iname = "L1 Norm"
    approx.f = 1
  }, lambda = {
    index = log(x$lambda)
    iname = "Log Lambda"
    approx.f = 0
  }, dev = {
    index = x$dev.ratio
    iname = "Fraction Deviance Explained"
    approx.f = 1
  })
  plot.data <- ggplot2::fortify(x)
  cols <- colnames(plot.data)
  plot.data$index <- index
  plot.data <- tidyr::gather_(plot.data, 'variable', 'value', cols)
  plot.data$i <- as.factor(plot.data$i)

  p <- ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(aes_string(x = 'index', y = 'value', colour = 'variable'))

  # ToDo::label handling
  p
}

#' Convert \code{glmnet::cv.glmnet} to \code{data.frame}
#'
#' @param model \code{glmnet::cv.glmnet} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(glmnet::glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
fortify.cv.glmnet <- function(model, data = NULL, ...) {
  d = data.frame('lambda' = log(model$lambda), 'cvm' = model$cvm,
                 'cvup' = model$cvup, 'cvlo' = model$cvlo,
                 'nz' = model$nz)
  d$label <- rep(max(model$cvup), nrow(d))
  d
}

#' Autoplot \code{glmnet::cv.glmnet}
#'
#' @param object \code{glmnet::cv.glmnet} instance
#' @param colour Line colour
#' @param linetype Line type
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(glmnet::cv.glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
autoplot.cv.glmnet <- function (object, sign.lambda = 1, xlab = 'log(Lambda)', ...) {

  if (sign.lambda < 0)
    xlab = paste("-", xlab, sep = "")

  plot.data = ggplot2::fortify(object)
  plot.data$lambda <- sign.lambda * plot.data$lambda

  p <- ggplot2::ggplot(plot.data) +
    geom_point(aes_string('lambda', 'cvm')) +
    # ggplot2::ylim(min(object$cvlo), max(object$cvup)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(object$name)

  p <- p + ggplot2::geom_errorbar(aes_string(x = 'lambda', ymin = 'cvlo', ymax = 'cvup'))
  p <- p + ggplot2::geom_text(aes_string(x = 'lambda', y = 'label', label = 'nz'))

  p <- p +
    ggplot2::geom_vline(xintercept = sign.lambda * log(object$lambda.min), linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = sign.lambda * log(object$lambda.1se), linetype = 'dashed')

  p
}
