# Compare results

rm(list = ls())
gc()

# options
options("openxlsx2.numFmt" = "#.000")

# Results by Peter

fn <- sort(list.files(
  path = "reports",
  pattern = "^ESS[0-9]{2} sample parameter estimates v[0-9]+",
  full.names = TRUE
))

dat_p <- openxlsx2::read_xlsx(
  file = max(fn),
  start_row = 3,
  skip_empty_rows = TRUE,
  skip_empty_cols = TRUE,
  na.strings = c("NA", "#DIV/0!"),
  check_names = TRUE
)

setDT(dat_p, check.names = TRUE)
setnames(dat_p, 1, "cntry_dom")

dat_p <- dat_p[!is.na(n_gross)]

x <- grep("^NA", names(dat_p), value = TRUE)
dat_p <- dat_p[, -x, with = FALSE]
rm(x)

x <- grep("cntry_dom|.1$", names(dat_p), value = TRUE)
dat_p <- dat_p[, x, with = FALSE]
rm(x)

setnames(dat_p, sub(".1", "", names(dat_p)))

# Remove duplicates (see IT)
dat_p <- unique(dat_p, by = "cntry_dom", fromLast = TRUE)

dat_p[, cntry := substr(cntry_dom, 1, 2)]

dat_p[, n := nchar(cntry_dom)]
dat_p[n == 2L, domain := "D0"]
dat_p[n >= 3L, domain := paste0("D", substr(cntry_dom, n, n))]
dat_p[, n := NULL]

dat_p[, cntry_dom := NULL]

names(dat_p)




# Results by Martin

fn <- sort(list.files(
  path = "results",
  pattern = "xlsx$",
  full.names = TRUE,
  recursive = TRUE
))

dat_m_c <- openxlsx2::read_xlsx(
  file = max(fn),
  sheet = "cntry",
  check_names = TRUE
)
setDT(dat_m_c)

dat_m_d <- openxlsx2::read_xlsx(
  file = max(fn),
  sheet = "domain",
  check_names = TRUE
)
setDT(dat_m_d)

latest_ess_round <- dat_m_c[, max(essround)]

dat_m_c <- dat_m_c[essround == latest_ess_round]
dat_m_d <- dat_m_d[essround == latest_ess_round]

dat_m_c[, domain := "D0"]
dat_m_d[, .N, keyby = .(domain)]

dat_m <- rbindlist(list(dat_m_c, dat_m_d), use.names = TRUE, fill = TRUE)
rm(dat_m_c, dat_m_d)

names(dat_p)
names(dat_m)

setnames(
  x = dat_p,
  old = c("neff"),
  new = c("n_eff")
)

setnames(
  x = dat_m,
  old = c("deff_p", "deff_c", "ICC", "b"),
  new = c("deffp",  "deffc",  "roh", "b_bar")
)

dat_m <- dat_m[, names(dat_p), with = FALSE]

dat_p_long <- melt.data.table(
  data = dat_p,
  id.vars = c("cntry", "domain"),
  na.rm = TRUE
) |> unique()

anyDuplicated(dat_p_long, by = c("cntry", "domain", "variable"))
dat_p_long[, n := .N, by = c("cntry", "domain", "variable")]
dat_p_long[n > 1]
dat_p_long[, n := NULL]

dat_m_long <- melt.data.table(
  data = dat_m,
  id.vars = c("cntry", "domain"),
  na.rm = TRUE
) |> unique()

anyDuplicated(dat_m_long, by = c("cntry", "domain", "variable"))

dat_long <- merge(
  x = dat_p_long,
  y = dat_m_long,
  by = c("cntry", "domain", "variable"),
  all = TRUE,
  suffixes = c("_P", "_M")
)

dat_long[, diff := value_M - value_P]
dat_long

dat_wide <- dcast.data.table(
  data = dat_long,
  formula = cntry + domain ~ variable,
  value.var = c("value_P", "value_M", "diff")
)

x <- grep("n_(gross|net)", names(dat_wide), value = TRUE)
dat_wide[, c(x) := map(.SD, as.integer), .SDcols = x]
rm(x)

# Gross sample size
dat_long[variable == "n_gross" & abs(diff) > 0][order(abs(diff))]

# Net sample size
dat_long[variable == "n_net" & abs(diff) > 0][order(abs(diff))]

# Gross sample size
tab_n_gross <- dat_wide[
  abs(diff_n_gross) > 0,
  .(cntry, domain, value_P_n_gross, value_M_n_gross, diff_n_gross)
]
tab_n_gross

# Net sample size
tab_n_net <- dat_wide[
  abs(diff_n_net) > 0,
  .(cntry, domain, value_P_n_net, value_M_n_net, diff_n_net)
]
tab_n_net

# Other parameters
dat_y <- dat_long[abs(diff) > 0.001]
x <- dat_y[, .SD, .SDcols = is.numeric] |> names()
dat_y[, c(x) := map(.SD, round, 3), .SDcols = x]
rm(x)

tab_ri <- dat_y[grep("ri", variable)]
tab_rr <- dat_y[grep("rr", variable)]
tab_deffp <- dat_y[grep("deffp", variable)]
tab_roh <- dat_y[grep("roh", variable)]
tab_b_bar <- dat_y[grep("b_bar", variable)]


# Save results

openxlsx2::write_xlsx(
  x = list(
    all_wide = dat_wide,
    all_long = dat_long,
    n_gross = tab_n_gross,
    n_net = tab_n_net,
    ri = tab_ri,
    rr = tab_rr,
    deffp = tab_deffp,
    roh = tab_roh,
    b_bar = tab_b_bar
  ),
  file = glue::glue(
    "tables/compare-ESS-{latest_ess_round}-sample-par-est-{Sys.Date()}.xlsx"
  ),
  na.strings = "",
  first_row = TRUE,
  first_col = TRUE,
)
