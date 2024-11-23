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

# Read data
x <- list.files(path = "data-tmp",
                pattern = "csv$",
                full.names = T)

dat_cf <- map(.x = x, .f = fread) |> rbindlist(use.names = T, fill = T)
rm(x)

dat_cf <- dat_cf[, .(
  name, essround, edition, proddate, cntry, typesamp, typsampa, idno, foutcod
)]
gc()

dat_cf[, .N, keyby = .(essround, name, edition, proddate)]


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

# Labels
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



# Labels for Final Outcome Code of Contact Attempts
dat[, foutcod_label := factor(
  x = foutcod,
  levels = c(0, 10, 11, 12, 20, 30, 31, 32, 33, 34, 41, 42, 43, 44, 45, 46,
             51, 52, 53, 54, 61, 62, 63, 64, 65, 67, 88),
  labels = c("Contact forms missing",
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
             "Undefined")
)]

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



# # Test case for HR R10
# tmp <- dat[essround == "R10" & cntry == "HR",
#            .N,
#            keyby = .(essround, selfcomp, cntry, edition, proddate, typesamp,
#                      foutcod, foutcod_label, resp, outcome)]
# write.xlsx(x = tmp, file = "tables/R10-HR-CF-foutcod.xlsx", colWidths = "auto")
# rm(tmp)



# Sample size, ri, and rr
tab_cf_cntry <- dat[
  ,
  .(typesamp = paste(unique(typesamp), collapse = "|"),
    n_gross = .N,
    n_net = sum(resp),
    n_ineligibles = sum(outcome == 3)
  ),
  keyby = .(essround, selfcomp, cntry)
]

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
