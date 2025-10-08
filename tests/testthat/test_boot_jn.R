devtools::load_all()
library(modsem)
library(future)

m1 <- "
  # Outer Model
  X =~ x1 + x2
  Y =~ y1 + y2
  Z =~ z1 + z2

  # Inner model
  Y ~ X + Z + X:Z
"

fit <- modsem(m1, oneInt, method = "lms")

plan(multisession, workers = 2L)
boot <- bootstrap_parallel(fit, R = 250L)

testthat::expect_warning(
  boot_plot_jn(x = "X", z = "Z", y = "Y", model = fit, boot = boot,
               min_z = -2, max_z = 1.5), regexp = "Truncating.*"
)
