#' Plot Bootstrap Interaction Effect Using the Johnson–Neyman Display
#'
#' Bootstrap version of the \code{modsem::plot_jn} function, using bootstrapped
#'   confidence intervals.
#' @param x,z,y Character scalars. Names of the predictor, moderator, and outcome.
#' @param model A fitted model object of class \code{modsem_da}, \code{modsem_mplus},
#'   \code{modsem_pi}, or \code{lavaan}.
#' @param boot A data.frame or matrix of bootstrap estimates where column names
#'   match parameter labels in \code{model}. At minimum the columns for
#'   \code{y ~ x} and \code{y ~ x:z} must be present (their labels are looked up
#'   from \code{model}).
#' @param min_z,max_z Numeric. Plot window for \code{z}, **relative to the mean of \code{z}**
#'   (defaults \code{-3} and \code{3}). The function internally shifts these by \code{mean(z)}.
#' @param detail Integer. Number of points in the generated \code{z}-grid (default \code{100}).
#' @param standardized Logical. If \code{TRUE} (default), use standardized parameter estimates
#'   to extract means/variances for \code{z}; otherwise use raw estimates.
#' @param lower.quantile,upper.quantile Numeric in \eqn{(0,1)}. Quantiles of the bootstrap
#'   distribution used for the lower and upper confidence limits (defaults \code{0.05} and \code{0.95}).
#' @param alpha Numeric in \eqn{[0,1]}. Alpha (opacity) of the confidence ribbon (default \code{0.2}).
#' @param sd.line Numeric. Draw a thick black segment at \eqn{\pm} \code{sd.line * sd(z)}
#'   centered at \code{mean(z)} (default \code{2}). The segment is truncated to the plotted
#'   \code{z}-range and may emit truncation warnings.
#'
#' @details
#' The bootstrap CI at each \code{z} is computed from the bootstrap draws of the simple slope
#' \eqn{\beta_x(z) = \beta_x + z\beta_{xz}}, using \code{mean} for the point estimate and
#' the requested quantiles for the bounds (unchanged from the user-provided logic).
#'
#' The (approximate) Johnson–Neyman transition(s) are drawn at \code{z}-values where the
#' bootstrap CI crosses zero. These are found by linear interpolation between adjacent grid
#' points whose significance indicators differ. This affects plotting only and does not alter
#' any inferential logic.
#'
#' @return A \code{ggplot} object.
#' @export
boot_plot_jn <- function(x, z, y, model, boot, min_z = -3, max_z = 3, detail = 1000,
                         standardized = FALSE,
                         lower.quantile = 0.05, upper.quantile = 0.95,
                         alpha = 0.2, sd.line = 2) {

  # parameter table (as in plot_jn)
  if (standardized) parTable <- modsem::standardized_estimates(model)
  else              parTable <- modsem::parameter_estimates(model)

  parTable <- getMissingLabels(parTable)

  # interaction labels (support reversed naming and non-lavaan engines)
  xz <- paste(x, z, sep = ":")
  xz <- c(xz, reverseIntTerm(xz))
  if (!inherits(model, c("modsem_da", "modsem_mplus")) &&
      !isLavaanObject(model)) {
    xz <- stringr::str_remove_all(xz, ":")
  }

  # helper to fetch the parameter label from the table
  getLabel <- function(lhs, rhs, op = "~") {
    cond <- parTable$lhs == lhs & parTable$op == op & parTable$rhs %in% rhs
    if (!any(cond)) stop(sprintf("Could not find coefficient for `%s%s%s`!", lhs, op, rhs))
    parTable[cond, "label"][[1L]]
  }

  labx  <- getLabel(y, x)
  labxz <- getLabel(y, xz)

  # pick bootstrap columns (if missing, fallback to zeros to preserve logic)
  getBootCoefs <- \(lab)
    if (lab %in% colnames(boot)) boot[, lab] else rep(0, NROW(boot))

  Bx  <- getBootCoefs(labx)
  Bxz <- getBootCoefs(labxz)

  # z mean/sd and plotting window (match plot_jn semantics)
  mean_z <- getMean(z, parTable = parTable)
  sd_z   <- sqrt(calcCovParTable(x = z, y = z, parTable = parTable))
  min_z_abs <- min_z + mean_z
  max_z_abs <- max_z + mean_z

  # grid in absolute z-scale used everywhere below
  valsz <- seq(min_z_abs, max_z_abs, length.out = detail)

  # Confidence interval computation
  CI <- data.frame(betax = NA_real_, betax.lower = NA_real_, betax.upper = NA_real_, z = valsz)
  for (i in seq_len(detail)) {
    BxGivenZ <- Bx + Bxz * valsz[i]
    CI[i, "betax"]       <- mean(BxGivenZ, na.rm = TRUE)
    CI[i, "betax.lower"] <- stats::quantile(BxGivenZ, probs = lower.quantile, na.rm = TRUE)
    CI[i, "betax.upper"] <- stats::quantile(BxGivenZ, probs = upper.quantile, na.rm = TRUE)
  }
  CI$sig <- CI$betax.lower > 0 | CI$betax.upper < 0

  # build plotting df to mirror plot_jn structure
  df_plot <- data.frame(
    z          = CI$z,
    slope      = as.numeric(CI$betax),
    lower_all  = as.numeric(CI$betax.lower),
    upper_all  = as.numeric(CI$betax.upper),
    significant = CI$sig
  )

  # unified labelling/legend like plot_jn
  siglabel <- "CI excludes 0"
  significance_chr <- ifelse(df_plot$significant, siglabel, "n.s.")
  df_plot$Significance <- factor(significance_chr, levels = c(siglabel, "n.s."))

  # split into contiguous runs (avoid polygon bleed)
  df_plot$run_id <- cumsum(c(0, diff(as.integer(df_plot$significant)) != 0))

  # SD-band segment (match plot_jn behavior and warnings)
  x_start <- mean_z - sd.line * sd_z
  x_end   <- mean_z + sd.line * sd_z
  if (x_start < min_z_abs && x_end > max_z_abs) {
    warning("Truncating SD-range on the right and left!")
  } else if (x_start < min_z_abs) {
    warning("Truncating SD-range on the left!")
  } else if (x_end > max_z_abs) {
    warning("Truncating SD-range on the right!")
  }

  x_start <- max(x_start, min_z_abs)
  x_end   <- min(x_end, max_z_abs)
  y_start <- y_end <- 0
  hline_label <- sprintf("+/- %s SDs of %s", sd.line, z)
  data_hline <- data.frame(x_start, x_end, y_start, y_end, hline_label)

  # discrete colour/fill mapping to mimic plot_jn
  breaks <- c(siglabel, "n.s.", hline_label)
  values <- structure(c("cyan3", "red", "black"), names = breaks)

  # y-limits
  y_range <- range(c(df_plot$lower_all, df_plot$upper_all, 0), na.rm = TRUE)
  if (!all(is.finite(y_range))) y_range <- c(-1, 1)

  # approximate JN transition(s) from CI-crossings (plotting-only)
  # locate indices where significance flips
  flip_idx <- which(diff(as.integer(df_plot$significant)) != 0)
  # do a simple linear interpolation for the z at which the bound touches zero
  approx_jn <- numeric(0)
  if (length(flip_idx) > 0) {
    for (k in flip_idx) {
      # Between k and k+1 we have a change. Use the "closest-to-zero" boundary.
      z0  <- df_plot$z[k];   z1  <- df_plot$z[k + 1]
      lo0 <- df_plot$lower_all[k]; lo1 <- df_plot$lower_all[k + 1]
      hi0 <- df_plot$upper_all[k]; hi1 <- df_plot$upper_all[k + 1]

      # choose which boundary crosses zero on this segment
      use_lower <- xor(lo0 > 0, lo1 > 0)  # TRUE if lower crosses zero
      if (use_lower) {
        # linear interpolation for lower boundary crossing 0
        t <- (0 - lo0) / (lo1 - lo0)
      } else {
        # linear interpolation for upper boundary crossing 0
        t <- (0 - hi0) / (hi1 - hi0)
      }
      t <- min(max(t, 0), 1)  # clamp for safety
      approx_jn <- c(approx_jn, z0 + t * (z1 - z0))
    }
  }

  # R CMD check complains that these don't have visible bindings
  # (They are lazily evaluated in df_plot)
  slope <- NULL
  lower_all <- NULL
  upper_all <- NULL
  Significance <- NULL
  run_id <- NULL

  # plot (mirror plot_jn styling)
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = z, y = slope)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_all, ymax = upper_all,
                   fill = Significance, group = run_id),
      alpha = alpha, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = Significance, group = run_id),
      linewidth = 1, na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    suppressWarnings(
      ggplot2::geom_segment(
        mapping = ggplot2::aes(x = x_start, xend = x_end, y = y_start, yend = y_end,
                               color = hline_label, fill = hline_label),
        data = data_hline, linewidth = 1.5
      )
    ) +
    ggplot2::ggtitle("Johnson-Neyman Plot (bootstrap)") +
    ggplot2::scale_discrete_manual(
      aesthetics = c("colour", "fill"),
      name = "",
      values = values,
      breaks = breaks,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(limits = y_range) +
    ggplot2::labs(x = z, y = paste("Slope of", x, "on", y)) +
    ggplot2::theme_minimal()

  # add (approximate) JN verticals if transitions occur in the window
  if (length(approx_jn) > 0) {
    top_y <- suppressWarnings(max(df_plot$slope[is.finite(df_plot$slope)], na.rm = TRUE))
    if (!is.finite(top_y)) top_y <- y_range[2]
    for (zstar in approx_jn) {
      if (is.finite(zstar) && zstar >= min_z_abs && zstar <= max_z_abs) {
        p <- p +
          ggplot2::geom_vline(xintercept = zstar, linetype = "dashed", color = "red") +
          ggplot2::annotate("text", x = zstar, y = top_y,
                            label = paste("JN point (~):", round(zstar, 2)),
                            hjust = -0.1, vjust = 1, color = "black")
      }
    }
  }

  p
}
