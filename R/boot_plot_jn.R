boot_plot_jn <- function(x, z, y, model, boot, min_z = -3, max_z = 3, detail = 100,
                         standardized = TRUE,
                         lower.quantile = 0.05, upper.quantile = 0.95) {
  if (standardized) parTable <- modsem::standardized_estimates(model)
  else              parTable <- modsem::parameter_estimates(model)

  parTable <- getMissingLabels(parTable)

  xz <- paste(x, z, sep = ":")
  xz <- c(xz, reverseIntTerm(xz))

  if (!inherits(model, c("modsem_da", "modsem_mplus")) &&
      !isLavaanObject(model)) {
    xz <- stringr::str_remove_all(xz, ":")
  }

  getLabel <- function(lhs, rhs, op = "~") {
    cond <- parTable$lhs==lhs & parTable$op==op & parTable$rhs==rhs 
    if (!any(cond)) stop(sprintf("Could not find coefficient for `%s%s%s`!", lhs, op, rhs))
    parTable[cond, "label"][[1L]]
  }

  labx  <- getLabel(y, x)
  labxz <- getLabel(y, xz)

  getBootCoefs <- function(lab)
    if (lab %in% colnames(boot)) boot[, lab] else rep(0, NROW(boot))

  Bx  <- getBootCoefs(labx)
  Bxz <- getBootCoefs(labxz)

  valsz <- seq(min_z, max_z, length.out = detail)
  CI <- data.frame(betax = NA, betax.lower = NA, betax.upper = NA, z = valsz)

  for (i in seq_len(detail)) {
    BxGivenZ <- Bx + Bxz * valsz[i]
   
    # na.rm shouldn't be necessary, but just in case
    CI[i, "betax"]       <- mean(BxGivenZ, na.rm = TRUE)
    CI[i, "betax.lower"] <- stats::quantile(BxGivenZ, probs = lower.quantile, na.rm = TRUE)
    CI[i, "betax.upper"] <- stats::quantile(BxGivenZ, probs = upper.quantile, na.rm = TRUE)
  }

  CI$sig <- CI$betax.lower > 0 | CI$betax.upper < 0

  ggplot2::ggplot(CI, mapping = ggplot2::aes(x = valsz, y = betax,
                                             ymin = betax.lower,
                                             ymax = betax.upper, fill = sig)) +
    ggplot2::geom_line() + ggplot2::geom_ribbon(alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::theme_bw()
}
