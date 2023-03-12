# Validation ####

# Reset ####
rm(list = ls())
gc()


# Load data ####
dat <- readRDS(file = "data/dat.rds")

# domain
# str(dat$domain)
dat[, domain := as.integer(domain)]
dat[, .N, keyby = .(domain)]
dat[is.na(domain), domain := 0L]
dat[, .N, keyby = .(domain)]

# count of domains
dat[, dom_n := length(unique(domain)), by = .(essround, cntry)]
dat[, .N, keyby = .(dom_n)]
dat[, .N, keyby = .(dom_n, domain)]
dat[, .N, keyby = .(dom_n, domain, essround)]

dat[dom_n == 1L, domain := 0L]
dat[, .N, keyby = .(domain)]
dat[, dom_n := NULL]

dcast.data.table(dat, essround ~ domain, fun.aggregate = length)

if (anyDuplicated(dat, by = c("essround", "cntry", "idno"))) {
    stop("Duplicated records in data")
}

# dat[, n := .N, by = .(essround, cntry, domain, idno)]
# dat[n > 1][order(essround, cntry, domain, idno)]
# dat[, n := NULL]


dat[, .N, keyby = domain]
dat[, .N, keyby = .(essround, cntry, domain)]

dat[is.na(stratum), .N, keyby = .(essround, cntry, stratum)]

m <- dat[, min(stratum, na.rm = T)]
m

# # Recode missing stratum values
# dat[is.na(stratum), stratum := m - 1L]

# Create strata variable from country, domain and stratum
m <- dat[, nchar(max(stratum))]
m

dat[, STR := paste(essround, cntry, paste0("D", domain),
                   stringr::str_pad(string = stratum, width = m, pad = "0"),
                   sep = "_")]
dat[, .N, keyby = .(essround, cntry, domain, stratum, STR)]
dat[, .N, keyby = .(essround, cntry, domain, stratum)]
dat[, .N, keyby = .(STR)]

# Create PSU variable from STR and psu
m <- dat[, nchar(max(psu))]
m

dat[, PSU := paste(STR,
                   stringr::str_pad(string = psu, width = m, pad = "0"),
                   sep = "_")]

dat[, .N, keyby = .(essround, cntry, domain, stratum, psu, PSU)]
dat[, .N, keyby = .(essround, cntry, domain, stratum, psu)]
dat[, .N, keyby = .(PSU)]
dat[, .N, keyby = .(STR, PSU)]

dat[is.na(psu), .(stratum, psu, STR, PSU)]

tab_cntry <- dat[, .(n_strat = sum(!duplicated(STR)),
                     n_psu   = sum(!duplicated(PSU)),
                     n_resp  = .N), keyby = .(essround, cntry, domain)]
tab_cntry

tab_strata <- dat[, .(n_psu  = sum(!duplicated(PSU)),
                      n_resp = .N),
                  keyby = .(essround, cntry, domain, STR)]
tab_strata

tab_psu <- dat[, .(n_resp = .N),
               keyby = .(essround, cntry, domain, STR, PSU)]
tab_psu

tabl <- list(tab_cntry, tab_strata, tab_psu)
names(tabl) <- c("cntry", "strata", "psu")

write.xlsx(tabl, file = "tables/SDDF-tables.xlsx",
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))


# Make a copy
dat2 <- copy(dat)

# Net sample size
nrow(dat2)

dat2[, .N, keyby = .(cntry, essround)]
dat2[, .N, keyby = .(cntry, essround, domain)]

dat2[cntry == "LT", .N, keyby = .(cntry, essround)]
dat2[cntry == "LT", .N, keyby = .(cntry, essround, domain)]


# Weights
# str(dat2[, .(essround, cntry, dweight, pspwght, pweight, anweight, prob)])
dat2[, .(essround, cntry, dweight, pspwght, pweight, anweight, prob)]
dat2[, .(prob, as.numeric(prob))]
dat2[, prob := as.numeric(prob)]

dat2[, .N, keyby = .(na_prob = is.na(prob), na_dweight = is.na(dweight))]
dat2[is.na(prob), .N, keyby = .(essround, cntry)]

dat2[, .N, keyby = .(essround,
                     prob     = !is.na(prob),
                     dweight  = !is.na(dweight),
                     pspwght  = !is.na(pspwght),
                     pweight  = !is.na(pweight),
                     anweight = !is.na(anweight))]

dat2[is.na(dweight), .(essround, cntry, dweight, pspwght, pweight, anweight)]


# Weight testing
dat2[is.na(anweight), anweight := pspwght * pweight]
tab_weight <- dat2[, c(.(n_resp = .N), lapply(.SD, sum)),
                   .SDcols = c("dweight", "pspwght", "anweight"),
                   keyby = .(essround, cntry)]
tab_weight[, anweight := anweight * 10e3]
tab_weight[, diff_d := abs(n_resp - dweight)]
tab_weight[, diff_p := abs(n_resp - pspwght)]

openxlsx::write.xlsx(x = tab_weight, file = "tables/tab_weight_sums.xlsx")


# Design weights computed from sampling probabilities
dat2[, summary(prob)]

dat2[prob == 0, .N, keyby = .(essround, cntry)]
dat2[prob >  1, .N, keyby = .(essround, cntry)]

dat2[prob >  0, dw := 1 / prob]
dat2[prob == 0 | prob > 1, dw := 1]

tmp <- dat2[, .(essround, cntry, dweight, pspwght, pweight, anweight, prob, dw)]

tmp[, c(.(n = .N), lapply(.SD, sum)),
    .SDcols = c("dweight", "pspwght", "pweight", "anweight"),
    keyby = .(essround, cntry)]

tmp[, dweight2 := .N * dw / sum(dw), by = .(essround, cntry)]

tmp[is.na(dweight2)]

tmp[!is.na(prob), all.equal(dweight, dweight2, check.attributes = F)]
tmp[!is.na(prob)][order(abs(dweight - dweight2))]

tmp[is.na(dweight)]
tmp[is.na(dweight2)]
tmp[prob == 0]

tmp[, diff := dweight2 - dweight]

pl <- ggplot(tmp[!is.na(prob)],
             aes(x = dweight, y = dweight2, colour = diff)) +
    geom_point() +
    scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
    facet_grid(essround ~ cntry) +
    theme_bw()

ggsave(filename = "plots/plot_dweight.png", plot = pl, width = 16, height = 9)
# Extreme design weights are being cut



tmp[, .N, keyby = .(abs(dweight - dweight2) > .1)]

tmp[abs(dweight - dweight2) > .1][order(abs(dweight - dweight2))]

tmp[abs(dweight - dweight2) > .1,
    .(essround, cntry, prob, dw, dweight, dweight2)]

tmp[abs(dweight - dweight2) > .1 & dweight2 > dweight,
    .(essround, cntry, prob, dw, dweight, dweight2)]
tmp[abs(dweight - dweight2) > .1 & dweight2 < dweight,
    .(essround, cntry, prob, dw, dweight, dweight2)]

tmp[abs(dweight - dweight2) > .1, .N, keyby = .(essround, cntry)]
tmp[abs(dweight - dweight2) > .1, .N, keyby = .(dweight2 > dweight)]


# dat2[, weight_des := dw]
dat2[, dw := NULL]

dat2[!is.na(anweight), .(anweight, pspwght * pweight)]
dat2[!is.na(anweight), all.equal(anweight, pspwght * pweight)]

dat2[, .N, keyby = .(is.na(pspwght))]

# Design wights used for the deff_p estimation
dat2[, weight_des := dweight * pweight * 10e3]

# Estimation weights used for the pop size and ratio linearisation
dat2[!is.na(pspwght), weight_est := pspwght * pweight * 10e3]
dat2[ is.na(pspwght), weight_est := weight_des]

dat2[, lapply(.SD, sum), .SDcols = c("weight_des", "weight_est"),
     keyby = .(essround)][, all.equal(weight_des, weight_est)]
dat2[, lapply(.SD, sum), .SDcols = c("weight_des", "weight_est"),
     keyby = .(essround, cntry)][, all.equal(weight_des, weight_est)]


# Save ####
saveRDS(object = dat2, file = "data/dat2.rds")
names(dat2)
