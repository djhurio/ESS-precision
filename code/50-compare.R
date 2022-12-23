# R10 compare results

rm(list = ls())
gc()

# Results by Peter

fn <- max(list.files(path = "reports", full.names = T))
dat_p <- openxlsx2::read_xlsx(
  xlsxFile = fn,
  skipEmptyRows = TRUE,
  startRow = 4,
  check.names = TRUE
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

dat_p[, cntry := substr(cntry_dom, 1, 2)]

dat_p[, n := nchar(cntry_dom)]
dat_p[n == 2L, domain := "D0"]
dat_p[n >= 3L, domain := paste0("D", substr(cntry_dom, n, n))]
dat_p[, n := NULL]

dat_p[, cntry_dom := NULL]

names(dat_p)




# Results by Martin

fn <- max(list.files(path = "results", pattern = "xlsx$",
                     full.names = T, recursive = T))
dat_m_c <- openxlsx2::read_xlsx(
  xlsxFile = fn,
  sheet = "cntry",
  check.names = TRUE
)
setDT(dat_m_c)

dat_m_d <- openxlsx2::read_xlsx(
  xlsxFile = fn,
  sheet = "domain",
  check.names = TRUE
)
setDT(dat_m_d)

dat_m_c <- dat_m_c[essround == "R10"]
dat_m_d <- dat_m_d[essround == "R10"]

dat_m_c[, domain := "D0"]
dat_m_d[, .N, keyby = .(domain)]

dat_m <- rbindlist(list(dat_m_c, dat_m_d), use.names = T, fill = T)
rm(dat_m_c, dat_m_d)

names(dat_p)
names(dat_m)

setnames(x = dat_p,
         old = c("neff"),
         new = c("n_eff"))

setnames(x = dat_m,
         old = c("deff_p", "deff_c", "ICC", "b"),
         new = c("deffp", "deffc", "roh", "b_bar"))

dat_m <- dat_m[, names(dat_p), with = F]

dat_p_long <- melt.data.table(data = dat_p, id.vars = c("cntry", "domain"),
                              na.rm = TRUE) |> unique()

anyDuplicated(dat_p_long, by = c("cntry", "domain", "variable"))
dat_p_long[, n := .N, by = c("cntry", "domain", "variable")]
dat_p_long[n > 1]
dat_p_long[, n := NULL]

dat_m_long <- melt.data.table(data = dat_m, id.vars = c("cntry", "domain"),
                              na.rm = TRUE) |> unique()

anyDuplicated(dat_m_long, by = c("cntry", "domain", "variable"))

dat_long <- merge(dat_p_long, dat_m_long,
                  by = c("cntry", "domain", "variable"),
                  all = TRUE,
                  suffixes = c("_P", "_M"))

dat_long[, diff := value_M - value_P]

dat_long

dat_wide <- dcast.data.table(data = dat_long,
                             formula = cntry + domain ~ variable,
                             value.var = c("value_P", "value_M", "diff"))

write_xlsx(x = dat_wide, file = "tables/compare-ess10-sample-par-est.xlsx",
           asTable = F, na.strings = "")
