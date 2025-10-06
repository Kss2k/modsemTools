devtools::load_all()
library(modsem)
library(future)

m1 <- "
  # Outer Model
  X =~ x1 + x2 +x3
  Y =~ y1 + y2 + y3
  Z =~ z1 + z2 + z3

  # Inner model
  Y ~ X + Z + X:Z
"

fit <- modsem(m1, oneInt, method = "lms")

plan(multisession, workers = 2L)
boot <- bootstrap_parallel(fit, R = 20L)

ci.bc <- modsem_boot_ci(fit, boot, type = "bc")
print(ci.bc)


ci.pct <- modsem_boot_ci(fit, boot, type = "Percent")
print(ci.pct)
