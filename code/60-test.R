### Data testing

# Reset
rm(list = ls())
gc()

# Load data
dat2 <- readRDS(file = "data/dat2.rds")
dat2[, .N, keyby = .(essround, selfcomp, edition, proddate)]

# Population weights
dat2[, .(essround, cntry, pweight, anweight, weight_est)]

dat2[, summary(pweight) |> as.list(), keyby = .(essround)]

dat2[, summary(pweight * 10e3) |> as.list(), keyby = .(essround)]

# Weight totals
dat2[,
  map(.SD, sum),
  .SDcols = c("anweight", "weight_est"),
  keyby = .(cntry, essround)
]

dat2[
  grep("LV|GB", cntry),
  map(.SD, \(x) sum(x) |> round() |> prettyNum(big.mark = ",")),
  .SDcols = c("anweight", "weight_est"),
  keyby = .(cntry, essround)
]

# Test
tab_test <- dat2[,
  .(n = .N, N = sum(weight_est)),
  by = .(essround, cntry, pweight)
]

tab_test[, pweight_test := N / n / 10e3]

tab_test[, all.equal(pweight, pweight_test)]
