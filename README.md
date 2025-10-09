# `modsemTools` <img src="man/figures/modsemTools.png" alt="Logo" align = "right" height="139" class="logo">
[![Tests](https://github.com/Kss2k/modsemTools/actions/workflows/tests.yml/badge.svg)](https://github.com/Kss2k/modsemTools/actions/workflows/tests.yml)
[![R-CMD-CHECK](https://github.com/Kss2k/modsemTools/actions/workflows/checks.yml/badge.svg)](https://github.com/Kss2k/modsemTools/actions/workflows/checks.yml)
[![GitHub Clones](https://img.shields.io/badge/dynamic/json?color=success&label=Clone&query=count&url=https://gist.githubusercontent.com/Kss2k/f2dd3d22af6a9df290c3e4d48da1155b/raw/clone.json&logo=github)](https://github.com/MShawon/github-clone-count-badge)

Utilities and examples for the [`modsem`](https://github.com/Kss2k/modsem) package.

This repository provides helpful tools, scripts, and demonstrations for working with the `modsem` package in R.

## Installation
To install the package, run:

```r
devtools::install_github("kss2k/modsemTools")
```

## Example

```r
library(modsem)
library(modsemTools)
library(future)


m1 <- "
  # Outer Model
  X =~ x1 + x2 + x3
  Y =~ y1 + y2 + y3
  Z =~ z1 + z2 + z3

  # Inner model
  Y ~ X + Z + X:Z
"

fit <- modsem(m1, oneInt, method = "lms")

plan(multisession, workers = 6L) # run in parallel
boot <- bootstrap_parallel(fit, R = 1000L)

modsem_boot_ci(fit, boot, type = "percent")
#> Percentile Confidence Intervals:
#>             0.5%      2.5%        5%  Estimate       95%     97.5%     99.5%   p-value
#> X=~x2     0.7745    0.7797    0.7838    0.8034    0.8256    0.8302    0.8363    0.0000
#> X=~x3     0.8836    0.8898    0.8938    0.9139    0.9353    0.9393    0.9463    0.0000
#> Z=~z2     0.7810    0.7864    0.7903    0.8103    0.8332    0.8358    0.8445    0.0000
#> Z=~z3     0.8518    0.8579    0.8609    0.8811    0.9039    0.9067    0.9129    0.0000
#> Y=~y2     0.7776    0.7834    0.7858    0.7983    0.8099    0.8120    0.8175    0.0000
#> Y=~y3     0.8793    0.8837    0.8864    0.8992    0.9116    0.9148    0.9198    0.0000
#> x1~1      0.9673    0.9767    0.9820    1.0218    1.0610    1.0665    1.0793    0.0000
#> x2~1      1.1656    1.1741    1.1808    1.2146    1.2457    1.2530    1.2652    0.0000
#> x3~1      0.8607    0.8757    0.8824    0.9184    0.9540    0.9619    0.9709    0.0000
#> z1~1      0.9536    0.9649    0.9707    1.0106    1.0498    1.0556    1.0679    0.0000
#> z2~1      1.1603    1.1671    1.1732    1.2049    1.2365    1.2416    1.2540    0.0000
#> z3~1      0.8633    0.8749    0.8795    0.9149    0.9499    0.9556    0.9697    0.0000
#> y1~1      0.9512    0.9788    0.9863    1.0349    1.0907    1.1002    1.1201    0.0000
#> y2~1      1.1581    1.1717    1.1790    1.2185    1.2638    1.2719    1.2850    0.0000
#> y3~1      0.8817    0.8985    0.9083    0.9519    1.0034    1.0120    1.0315    0.0000
#> x1~~x1    0.1366    0.1415    0.1437    0.1581    0.1726    0.1765    0.1822    0.0000
#> x2~~x2    0.1445    0.1480    0.1504    0.1621    0.1738    0.1762    0.1806    0.0000
#> x3~~x3    0.1460    0.1496    0.1520    0.1645    0.1771    0.1800    0.1861    0.0000
#> z1~~z1    0.1457    0.1492    0.1518    0.1667    0.1804    0.1824    0.1896    0.0000
#> z2~~z2    0.1414    0.1461    0.1480    0.1596    0.1714    0.1735    0.1770    0.0000
#> z3~~z3    0.1403    0.1441    0.1460    0.1582    0.1698    0.1728    0.1766    0.0000
#> y1~~y1    0.1378    0.1411    0.1447    0.1596    0.1730    0.1765    0.1817    0.0000
#> y2~~y2    0.1361    0.1408    0.1429    0.1544    0.1659    0.1680    0.1715    0.0000
#> y3~~y3    0.1446    0.1484    0.1505    0.1635    0.1761    0.1781    0.1836    0.0000
#> X~~X      0.8903    0.9123    0.9251    0.9811    1.0350    1.0473    1.0659    0.0000
#> X~~Z      0.1367    0.1522    0.1610    0.1999    0.2418    0.2511    0.2668    0.0000
#> Z~~Z      0.9171    0.9372    0.9528    1.0173    1.0827    1.0963    1.1190    0.0000
#> Y~~Y      0.8882    0.9067    0.9180    0.9801    1.0417    1.0554    1.0815    0.0000
#> Y~X       0.5963    0.6108    0.6215    0.6721    0.7244    0.7319    0.7487    0.0000
#> Y~Z       0.4895    0.5122    0.5198    0.5680    0.6165    0.6240    0.6410    0.0000
#> Y~X:Z     0.6493    0.6639    0.6736    0.7181    0.7644    0.7732    0.7931    0.0000
```
