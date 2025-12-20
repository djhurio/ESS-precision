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
# https://ess.sikt.no/
# Check if new versions of the files have been published
# Update files as necessary

# Read data only CF data
x <- list.files(
  path = "data-tmp",
  pattern = "CF.*\\.csv$",
  full.names = T
)
x
dat_cf <- map(.x = x, .f = fread) |> rbindlist(use.names = T, fill = T)
rm(x)

dat_cf <- dat_cf[, .(
  name,
  essround,
  edition,
  proddate,
  cntry,
  typesamp,
  typsampa,
  idno,
  foutcod
)]
gc()

dat_cf[grep("^[0-9]*$", idno, invert = TRUE)]
dat_cf[grep("^[0-9]*$", idno, invert = TRUE), .N]
dat_cf[grep("^[0-9]*$", idno, invert = TRUE), .N, keyby = .(essround, cntry)]

dat_cf[
  essround == 11L & cntry %in% c("IL", "ME"),
  .N,
  keyby = .(essround, cntry, missing_idno = is.na(idno))
]

dat_cf[grep('"', idno)]
dat_cf[, idno := as.integer(sub('"', '', idno))]

dat_cf[grep('"', cntry), .N, keyby = .(cntry)]
dat_cf[, cntry := sub('"', '', cntry)]
dat_cf[grep('"', cntry), .N, keyby = .(cntry)]

dat_cf[, .N, keyby = .(essround, name, edition, proddate)]
dat_cf[name == "ESS11SCCF_CZ_e02", essround := 11L]
dat_cf[, .N, keyby = .(essround, name, edition, proddate)]

# Self-completion
dat_cf[, selfcomp := grepl("SC", name)]
dat_cf[, .N, keyby = .(selfcomp)]
dat_cf[, .N, keyby = .(essround, name, selfcomp, edition, proddate)]
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

# Type of the sample R07-R10
dat_cf[, .N, keyby = .(typesamp)]
dat_cf[, .N, keyby = .(essround, typesamp)]

# Type of the sample from R11
dat_cf[, .N, keyby = .(typsampa)]
dat_cf[, .N, keyby = .(essround, typsampa)]

# Combine both

# typesamp
# 1	Individual person
# 2	Household
# 3	Address

# typsampa
# 1	Individual person
# 2	Address

dat_cf[typsampa == 1L, typesamp := 1L]
dat_cf[typsampa == 2L, typesamp := 3L]

dat_cf[, .N, keyby = .(typesamp)]
dat_cf[, .N, keyby = .(essround, typesamp)]

dat_cf[is.na(typesamp), .N, keyby = .(essround, cntry)]
dat_cf[essround == "R11" & cntry == "CZ"]

# Labels
dat_cf[,
  typesamp := factor(
    x = typesamp,
    levels = 1:3,
    labels = c("Individual person", "Household", "Address")
  )
]

dat_cf[, .N, keyby = .(typesamp)]
dat_cf[, .N, keyby = .(essround, typesamp)]


# Respondents
dat <- readRDS("data/dat2.rds")
dat_resp <- dat[, .(essround, selfcomp, cntry, idno, resp = TRUE)]
rm(dat)
gc()

dat_resp[is.na(idno)]

setkey(dat_resp, essround, selfcomp, cntry, idno)

key(dat_resp)
key(dat_cf)

# dat_resp <- unique(dat_resp)
# dat_cf <- unique(dat_cf)

if (anyDuplicated(dat_resp, by = key(dat_resp))) {
  dat_resp[, n := .N, keyby = key(dat_resp)]
  print(dat_resp[n > 1L])
  stop("Duplicated keys in RESP data")
}

if (anyDuplicated(dat_cf, by = key(dat_cf))) {
  dat_cf[, n := .N, keyby = key(dat_cf)]
  print(dat_cf[n > 1L])
  stop("Duplicated keys in CF data")
}


# Merge CF and resp
dat_cf[, cf := TRUE]

dat <- merge(dat_cf, dat_resp, all = TRUE)

dat[is.na(cf), cf := FALSE]
dat[is.na(resp), resp := FALSE]

dat[is.na(idno), .N, keyby = .(cf, resp)]

dat[
  essround == "R11" & cntry %in% c("IL", "ME"),
  .N,
  keyby = .(essround, cntry, cf, resp, missing_idno = is.na(idno))
]

dat[!(essround == "R10" & cntry == "UA"), .N, keyby = .(cf, resp)]

dat[, .N, keyby = .(essround)]
dat[, .N, keyby = .(cntry)]

if (dat_cf[, .N] != dat[!(essround == "R10" & cntry == "UA"), .N]) {
  warning("Error in CF and RESP merge")
}
if (dat[!(essround == "R10" & cntry == "UA"), any(!cf)]) {
  warning("There are respondents which are not in CF")
}


# Labels for Final Outcome Code of Contact Attempts
dat[,
  foutcod_label := factor(
    x = foutcod,
    levels = c(
      0,
      10,
      11,
      12,
      20,
      30,
      31,
      32,
      33,
      34,
      41,
      42,
      43,
      44,
      45,
      46,
      51,
      52,
      53,
      54,
      61,
      62,
      63,
      64,
      65,
      67,
      88
    ),
    labels = c(
      "Contact forms missing",
      "Valid interview",
      "Partial interview: break off",
      "Invalid interview",
      "Non-contact",
      "Refusal because of opt-out list",
      "Broken appointment",
      "Refusal by respondent",
      "Refusal by proxy",
      "Household refusal, before selection",
      "Respondent not available, away",
      "Respondent mentally/physical unable/ill/sick (short term)",
      "Respondent deceased",
      "Language barrier",
      "Contact but no interview, other",
      "Respondent mentally/physical unable/ill/sick (long term)",
      "Respondent moved out of country",
      "Respondent moved to unknown destination",
      "Respondent has moved, still in country",
      "Address not traceable",
      "Derelict or demolished house",
      "Not yet built, not ready for occupation",
      "Not occupied",
      "Address not residential: business",
      "Address not residential: institution",
      "Other ineligible",
      "Undefined"
    )
  )
]

# outcome
dat[, outcome := NA_integer_]
dat[(resp), outcome := 1L]
dat[!(resp) & foutcod %in% c(43, 51, 61:67), outcome := 3L]
dat[is.na(outcome), outcome := 2L]

dat[, .N, keyby = .(outcome)]

dcast.data.table(
  data = dat,
  formula = outcome ~ essround + selfcomp,
  fun.aggregate = length
)


# # Test case for HR R10
# tmp <- dat[essround == "R10" & cntry == "HR",
#            .N,
#            keyby = .(essround, selfcomp, cntry, edition, proddate, typesamp,
#                      foutcod, foutcod_label, resp, outcome)]
# write.xlsx(x = tmp, file = "tables/R10-HR-CF-foutcod.xlsx", colWidths = "auto")
# rm(tmp)

# Sample size, ri, and rr

dat[, .N, keyby = .(foutcod)]

tab_cf_cntry <- dat[,
  .(
    typesamp = paste(unique(typesamp), collapse = "|"),
    n_gross = .N,
    n_net = sum(resp),
    n_ineligibles = sum(outcome == 3),
    flag = all(is.na(foutcod))
  ),
  keyby = .(essround, selfcomp, cntry)
]

# Cases with no ineligibles recorded
tab_cf_cntry[n_ineligibles == 0L & !flag]
tab_cf_cntry[n_ineligibles == 0L & !flag, .(essround, selfcomp, cntry)]

dat[
  tab_cf_cntry[n_ineligibles == 0L & !flag, .(essround, selfcomp, cntry)],
  .N,
  keyby = .(essround, cntry, outcome, foutcod, foutcod_label)
]

tab_cf_cntry
map_chr(tab_cf_cntry, class)

anyDuplicated(tab_cf_cntry, by = c("essround", "cntry"))

# Outcome codes are not available for all rounds
tab_cf_cntry[cntry == "UA"]
tab_cf_cntry[(flag), n_ineligibles := NA_integer_]
tab_cf_cntry[, flag := NULL]

tab_cf_cntry[, rr := n_net / (n_gross - n_ineligibles)]
tab_cf_cntry[, ri := n_ineligibles / n_gross]
tab_cf_cntry[cntry == "UA"]


# Gross sample size is not available
tab_cf_cntry[n_gross == n_net]
tab_cf_cntry[n_ineligibles == 0L]


# Save
saveRDS(object = tab_cf_cntry, file = "data/tab_cf_cntry.rds")
