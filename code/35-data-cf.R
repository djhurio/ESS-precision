# Calculation of
# gross sample size (n_gross)
# response rate (rr)
# ineligibility rate (ri)

# Reset ####
rm(list = ls())
gc()


# data from Contact forms
#
# Download Contact forms data from the ESS Data Portal
# https://ess-search.nsd.no/
# Check if new versions of the files have been published
# Update files as necessary

# delete all sav and html files
list.files(path = "data-ess/CF",
           pattern = "(csv|html)$",
           full.names = T) |> file.remove()

# unzip all data files
for (x in list.files(path = "data-ess/CF", pattern = ".zip$", full.names = T)) {
  cat(x, "\n")
  utils::unzip(zipfile = x, exdir = "data-ess/CF")
}
rm(x)


# Read data
x <- list.files(path = "data-ess/CF",
                pattern = "csv$",
                full.names = T)

dat_cf <- map(.x = x, .f = fread) |> rbindlist(use.names = T, fill = T)
rm(x)

dat_cf <- dat_cf[, .(name, essround, edition, proddate, cntry, typesamp,
                     idno, foutcod)]
gc()

dat_cf[, .N, keyby = .(name, essround, edition, proddate)]


# Self-completion
dat_cf[, selfcomp := grepl("SC", name)]
dat_cf[, .N, keyby = .(selfcomp)]
dat_cf[, name := NULL]

# Round
dat_cf[, essround := factor(sprintf(fmt = "R%02d", essround))]
dat_cf[, .N, keyby = .(essround)]

# Production date
dat_cf[, proddate := lubridate::dmy(proddate)]
dat_cf[, .N, keyby = .(proddate)]

dat_cf[, .N, keyby = .(essround, selfcomp, edition, proddate)]

setkey(dat_cf, essround, selfcomp, cntry, idno)

dcast.data.table(
  data = dat_cf,
  formula = foutcod ~ essround + selfcomp,
  fun.aggregate = length
)

# Type of the sample
dat_cf[, typesamp := factor(
  x = typesamp,
  levels = 1:3,
  labels = c("Individual person", "Household", "Address")
)]

dat_cf[, .N, keyby = .(typesamp)]

# Respondents
dat <- readRDS("data/dat2.rds")
dat_resp <- dat[, .(essround, selfcomp, cntry, idno, resp = TRUE)]
rm(dat)
gc()

setkey(dat_resp, essround, selfcomp, cntry, idno)


# Merge CF and resp
dat_cf[, cf := TRUE]

dat <- merge(dat_cf, dat_resp, all = TRUE)

dat[is.na(cf), cf := FALSE]
dat[is.na(resp), resp := FALSE]

dat[, .N, keyby = .(cf, resp)]

if (dat_cf[, .N] != dat[, .N]) stop("Error in merge")
if (dat[, any(!cf)]) stop("There is resp not in CF")


# outcome
dat[, outcome := NA_integer_]
dat[ (resp),                                 outcome := 1L]
dat[!(resp) & foutcod %in% c(43, 51, 61:67), outcome := 3L]
dat[is.na(outcome),                          outcome := 2L]

dat[, .N, keyby = .(outcome)]

dcast.data.table(
  data = dat,
  formula = outcome ~ essround + selfcomp,
  fun.aggregate = length
)


# Sample size, ri, and rr
tab_cf_cntry <- dat[, .(typesamp = paste(unique(typesamp), collapse = "|"),
                        n_gross = .N,
                        n_net = sum(resp),
                        n_ineligibles = sum(outcome == 3)),
                    keyby = .(essround, selfcomp, cntry)]
tab_cf_cntry
map_chr(tab_cf_cntry, class)

anyDuplicated(tab_cf_cntry, by = c("essround", "cntry"))

# Outcome codes are not available for all rounds
tab_cf_cntry[, flag := sum(n_ineligibles) == 0L, by = .(essround, selfcomp)]
tab_cf_cntry[(flag), n_ineligibles := NA_integer_]
tab_cf_cntry[, flag := NULL]

tab_cf_cntry[, rr := n_net / (n_gross - n_ineligibles)]
tab_cf_cntry[, ri := n_ineligibles / n_gross]


# Save
saveRDS(object = tab_cf_cntry, file = "data/tab_cf_cntry.rds")
