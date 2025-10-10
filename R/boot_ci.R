# Create Confidence Intervals from Bootstrapped Samples ##
BC_CI <- function(est, boot, i) {
  b.no <- nrow(boot)
  bX <- modsem::modsem_coef(est)  # estimated parameters
  estX <- bX[i]
  abX <- boot[,i]
  zX = stats::qnorm(sum(abX<estX)/nrow(boot))  # Bias-Corrected Factor
  probs_lo3 = stats::pnorm(2*zX+stats::qnorm(0.005))
  probs_lo2 = stats::pnorm(2*zX+stats::qnorm(0.025))
  probs_lo1 = stats::pnorm(2*zX+stats::qnorm(0.050))
  probs_hi1 = stats::pnorm(2*zX+stats::qnorm(0.950))
  probs_hi2 = stats::pnorm(2*zX+stats::qnorm(0.975))
  probs_hi3 = stats::pnorm(2*zX+stats::qnorm(0.995))
  CI <- append(stats::quantile(abX, probs = c(probs_lo3, probs_lo2, probs_lo1, probs_hi1, probs_hi2, probs_hi3)), estX, after=3)
  # Bias-Corrected p-value #
  if ((estX>0 & min(abX)>0) | (estX<0 & max(abX)<0)) {
    p_value = round(0, digits=4)
  } else if (stats::qnorm(sum(abX>0)/b.no)+2*zX<0) {
    p_value = 2*stats::pnorm((stats::qnorm(sum(abX>0)/b.no)+2*zX))
  } else {
    p_value = 2*stats::pnorm(-1*(stats::qnorm(sum(abX>0)/b.no)+2*zX))
  }
  CI <- append(CI, p_value)
}


P_CI <- function(est, boot, i) {
  b.no <- nrow(boot)
  bX <- modsem::modsem_coef(est)  # estimated parameters
  estX <- bX[i]
  abX <- boot[,i]
  CI <- append(stats::quantile(abX, probs = c(0.005, 0.025, 0.050, 0.950, 0.975, 0.995)), estX, after=3)
  # Percentile p-value #
  if (stats::quantile(abX,probs=0.5)>0) {
    p_value <- round(2*(sum(abX<0)/b.no), digits=4)
  } else {
    p_value <- round(2*(sum(abX>0)/b.no), digits=4)
  }
  CI <- append(CI, p_value)
}


#' Compute Bootstrap Confidence Intervals for modsem Estimates
#'
#' This function computes bootstrap confidence intervals for parameter estimates
#' obtained from a \code{modsem} model object. It supports both percentile and
#' bias-corrected (BC) interval types.
#'
#' @param est An object containing parameter estimates from a \code{modsem} model,
#'   typically produced by a call to a fitting function such as \code{modsem()}
#' @param boot A numeric matrix or data frame of bootstrap estimates, where each
#'   column corresponds to a bootstrap replication and each row corresponds to a
#'   model parameter.
#' @param type A character string specifying the type of confidence interval to
#'   compute. Must be one of:
#'   \itemize{
#'     \item \code{"percent"} — percentile-based bootstrap confidence intervals.
#'     \item \code{"bc"} — bias-corrected bootstrap confidence intervals.
#'   }
#'   Partial matching is supported, and the argument is case-insensitive.
#'
#' @details
#' For each parameter estimate in \code{est}, the function computes bootstrap
#' confidence intervals using either the \code{P_CI()} (percentile) or \code{BC_CI()}
#' (bias-corrected) method. The resulting object contains quantiles at
#' 0.5\%, 2.5\%, 5\%, 95\%, 97.5\%, and 99.5\%, along with the original estimate
#' and corresponding bootstrap-derived p-value.
#'
#' The function internally calls \code{modsem::modsem_coef()} to extract the vector of
#' parameter estimates from the \code{est} object.
#'
#' @return
#' A matrix of class \code{"ModsemBootCi"} with one row per parameter and the
#' following columns:
#' \itemize{
#'   \item \code{0.5\%}, \code{2.5\%}, \code{5\%} — lower-tail quantiles.
#'   \item \code{Estimate} — original model estimate.
#'   \item \code{95\%}, \code{97.5\%}, \code{99.5\%} — upper-tail quantiles.
#'   \item \code{p-value} — bootstrap-based significance level.
#' }
#' The matrix has an attribute \code{"type"} indicating the CI method used (\code{"percent"} or \code{"bc"}).
#'
#' @examples
#' \dontrun{
#' library(modsem)
#'
#' m1 <- "
#'   # Outer Model
#'   X =~ x1 + x2 +x3
#'   Y =~ y1 + y2 + y3
#'   Z =~ z1 + z2 + z3
#'
#'   # Inner model
#'   Y ~ X + Z + X:Z
#' "
#'
#' fit <- modsem(m1, oneInt, method = "lms")
#' boot <- bootstrap_modsem(fit, R = 250)
#'
#' ci <- modsem_boot_ci(fit, boot, type = "bc")
#' print(ci)
#' }
#'
#' @export
modsem_boot_ci <- function(est, boot, type = c("percent", "bc")) {
  type <- tolower(type)
  match.arg(type)

  bX <- modsem::modsem_coef(est)  # estimated parameters
  element <- 8 * NCOL(boot)

  header <- format(c("0.5%","2.5%","5%","Estimate","95%","97.5%","99.5%", "p-value"),
                   justify = "right", width = 9)

  BOOTCI <- matrix(seq_len(element),
                   nrow = length(bX),
                   dimnames = list(names(bX), header))

  FUN <- if (type == "percent") P_CI else BC_CI

  for (j in seq_len(length(bX)))
    BOOTCI[j,] <- FUN(est, boot, j)

  attr(BOOTCI, "type") <- type
  class(BOOTCI) <- "ModsemBootCi"

  BOOTCI
}


#' @export
print.ModsemBootCi <- function(x, ...) {
  type <- attr(x, "type")

  if (type == "percent") cat("Percentile Confidence Intervals:\n")
  else                   cat("Bias-Corrected Confidence Intervals:\n")

  print(format(round(x, digits = 4), nsmall = 4, scientific = FALSE), quote=FALSE, right=TRUE)
  invisible(x)
}
