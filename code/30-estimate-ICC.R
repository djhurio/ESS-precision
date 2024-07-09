# ICC estimation ####

# Reset
rm(list = ls())
gc()

# Load data
dat2 <- readRDS(file = "data/dat2.rds")
dat2[, .N, keyby = .(essround, selfcomp, edition, proddate)]

# dat2[cntry == "BG", as.list(summary(dweight)), keyby = .(essround)]
# dat2[cntry == "BG", .(deff_p = .N * sum(dweight ^ 2) / sum(dweight) ^ 2),
#      keyby = .(essround)]
#
# ggplot(data = dat2[cntry == "BG"], mapping = aes(x = dweight)) +
#   geom_density() +
#   facet_wrap(facets = vars(essround))

variables <- readRDS(file = "data/variables.rds")

round.labels <- unique(dat2$essround) |> sort()
round.labels

# Average number of respondents per PSU
dat_b <- dat2[, .N, keyby = .(essround, cntry, domain, PSU)]
dat_b <- dat_b[, .(b = mean(N)), keyby = .(essround, cntry, domain)]

dat_b
dat_b[essround == "R09" & cntry == "HU"]

dat_b[order(b)]
dat_b[, summary(b)]

dat_b[b == 1]


# Transfer data to long format
dat3 <- melt.data.table(
  data = dat2,
  id.vars = c("essround", "selfcomp", "cntry", "domain",
              "STR", "PSU", "idno",
              "weight_des", "weight_est"),
  measure.vars = intersect(names(dat2), variables$varname),
  na.rm = F,
  variable.name = "varname",
  variable.factor = F
)


# Add type of variable
setkey(dat3, varname)
dat3 <- merge(dat3, variables[, .(varname, type)],
              by = "varname", all.x = T, sort = F)
dat3[, .N, keyby = .(type)]

# Add flag, if variable should be excluded from the ICC estimation
variables_long <- melt.data.table(
  data = variables,
  id.vars = "varname",
  measure.vars = patterns("^ESS"),
  variable.name = "round",
  value.name = "flag",
  variable.factor = F,
  value.factor = F
)

variables_long[, .N, keyby = .(flag)]
variables_long[!(flag)]

variables_long[, essround := factor(sub("ESS", "R", sub("SC", "", round)))]
variables_long[, selfcomp := grepl("SC", round)]
variables_long[, round := NULL]

setkey(dat3, varname, essround, selfcomp)
dat3 <- merge(dat3, variables_long,
              by = c("varname", "essround", "selfcomp"),
              all.x = T)

dat3[, .N, keyby = .(flag)]
dat3[!(flag), .N, keyby = .(essround, selfcomp, varname)]

unique(dat3[, .(essround, selfcomp,
                varname)])[, .(n = .N),
                           keyby = .(essround, selfcomp)]
unique(dat3[, .(essround, selfcomp,
                varname, flag)])[, .(n = sum(flag)),
                                 keyby = .(essround, selfcomp)]

# Keep only variables used for the ICC estimation
dat3 <- dat3[(flag)]
dat3[, flag := NULL]
unique(dat3[, .(essround, selfcomp,
                varname)])[, .(n = .N),
                           keyby = .(essround, selfcomp)]

# Ratio of two totals is used for the estimation
# Calculate Y and Z values for ratio estimation

# Y
dat3[type == "Binary", value_y := as.numeric(value == 1)]
dat3[type != "Binary", value_y := value]
dat3[is.na(value), value_y := 0]

# Z
dat3[, value_z := as.integer(!is.na(value))]


# Test
dat3[, lapply(.SD, function(x) round(mean(x), 3)),
     .SDcols = c("value_y", "value_z"), by = .(varname)][order(value_y)]
dat3[, lapply(.SD, function(x) round(mean(x), 3)),
     .SDcols = c("value_y", "value_z"), by = .(varname)][order(-value_z)]

tab_mean_y <- dcast.data.table(
  data = dat3,
  formula = varname ~ essround,
  value.var = "value_y",
  fun.aggregate = function(x) round(mean(x), 3)
) |> setnames(
  old = as.character(round.labels),
  new = paste0(round.labels, "_Y")
)

tab_mean_z <- dcast.data.table(
  data = dat3,
  formula = varname ~ essround,
  value.var = "value_z",
  fun.aggregate = function(x) round(mean(x), 3)
) |> setnames(
  old = as.character(round.labels),
  new = paste0(round.labels, "_Z")
)

variables[, id := .I]
variables <- Reduce(f = merge, x = list(variables, tab_mean_y, tab_mean_z))
setorder(variables, id)
setcolorder(variables, "id")

# Write out table about the target variables
write.xlsx(
  x = variables,
  file = "tables/variables-2.xlsx",
  colWidths = "auto", firstRow = T,
  headerStyle = createStyle(textDecoration = "italic", halign = "center")
)

fwrite(x = variables, file = "tables/variables-2.csv", quote = T)





# Variable name extended with round, country, and domain
dat3[, varname_ext := paste(essround, cntry, paste0("D", domain),
                            varname, sep = "_")]
setkey(dat3, varname_ext)
dat3[, .N, keyby = .(varname_ext)]


# Taylor linearisation of ratio of two totals
# using design weights
dat3[, total_Y := sum(value_y * weight_est), by = .(varname_ext)]
dat3[, total_Z := sum(value_z * weight_est), by = .(varname_ext)]
dat3[
  total_Z > 0,
  lin_val := (value_y - total_Y / total_Z * value_z) / total_Z
]


# PSU variance
tab_psu <- dat3[
  !is.na(value),
  .(n = .N, sd_y_psu = sd(value_y)),
  keyby = .(varname_ext, PSU)
]
tab_psu[n == 1L, sd_y_psu := 0]
tab_psu <- tab_psu[, .(max_sd_y_psu = max(sd_y_psu)), keyby = .(varname_ext)]
tab_psu[max_sd_y_psu == 0]



# Summary table
tab_variables <- dat3[, .(
  n_resp = .N,
  n_na = sum(is.na(value)),
  sd_y = sd(value_y),
  pop_size = sum(weight_est),
  total_Y  = sum(weight_est * value_y),
  total_Z  = sum(weight_est * value_z)
), keyby = .(varname_ext, essround, cntry, domain, varname, type)]

tab_variables <- merge(
  x = tab_variables,
  y = dat_b,
  by = c("essround", "cntry", "domain"),
  all.x = T
)

tab_variables <- merge(
  x = tab_variables,
  y = tab_psu,
  by = c("varname_ext"),
  all.x = T
)

tab_variables[total_Z > 0, ratio := total_Y / total_Z]


# Cases where esimtation is not possible
tab_variables[n_na == n_resp] # All cases missing
tab_variables[n_resp - n_na == 1L] # Only 1 respondent
tab_variables[n_na < n_resp & total_Y == 0] # All answers are 0
tab_variables[n_na < n_resp & total_Y > 0 & total_Y == total_Z] # All answers are 1
tab_variables[sd_y == 0] # No variation in variable

# No variation in PSUs
tab_variables[max_sd_y_psu == 0]
tab_variables[b > 1 & sd_y > 0 & ratio != 1 & max_sd_y_psu == 0 & n_resp - n_na > 1L]

# Mark varibales where estimation of effective sample size is not possible:
# 1) variable is a constant (sd_y == 0)
# 2) mean estimate is 1 (total_Y == total_Z)
# 3) There is only one respondent ((n_resp - n_na) == 1L)
# 4) There is no variance in PSUs (max_sd_y_psu == 0)
tab_variables[, flag := (b > 1) & (sd_y == 0 | total_Y == total_Z |
                (n_resp - n_na) == 1L | max_sd_y_psu == 0)]
tab_variables[, .N, keyby = .(flag)]

# tab_variables[("R5_LT_D1_emplno")]


# Number of variables by country, domain, round
dcast.data.table(
  data = tab_variables[!(flag)],
  formula = cntry + domain ~ essround,
  fun.aggregate = length
)

saveRDS(object = tab_variables, file = "data/tab_variables.rds")


# List of extended variable names to be used in the ICC estimation
varname_list <- tab_variables[(b > 1) & !(flag), varname_ext]
length(varname_list)


# Estimate ICC ####

# R package
#
# ICC: Facilitating Estimation of the Intraclass Correlation Coefficient
#
# Assist in the estimation of the Intraclass Correlation Coefficient (ICC) from
# variance components of a one-way analysis of variance and also estimate the
# number of individuals or groups necessary to obtain an ICC estimate with a
# desired confidence interval width.
#
# https://cran.r-project.org/package=ICC
# https://github.com/matthewwolak/ICC

# Why is the equation for Intra-class correlation aligned with design effect of
# cluster sampling?
# https://stats.stackexchange.com/q/436816/3330

# Methods in Sample Surveys
# Cluster Sampling
# https://ocw.jhsph.edu/courses/StatMethodsForSampleSurveys/PDFs/Lecture5.pdf


# Order by
setkey(dat3, varname_ext, PSU)


# Test - compare ICC with samplesize4surveys

# set.seed(1)
# tmp <- sort(sample(varname_list, 10))
# dat3[(tmp), ICC::ICCbare(x = factor(PSU), y = lin_val, data = .SD),
#      keyby = .(varname_ext)]
# dat3[(tmp), samplesize4surveys::ICC(y = lin_val, cl = PSU),
#      keyby = .(varname_ext)]
#
# tmp <- c("R6_IS_D1_dscrna", "R6_IS_D1_dscrref", "R6_IS_D1_pdjobev",
#          "R9_HU_D1_ctzcntr", "R9_PT_D1_dscrdsb")
# dat3[(tmp), ICC::ICCbare(x = factor(PSU), y = lin_val, data = .SD),
#      keyby = .(varname_ext)]
# dat3[(tmp), samplesize4surveys::ICC(y = lin_val, cl = PSU),
#      keyby = .(varname_ext)]

# tab_variables[varname_ext == "R8_HU_D2_wkhtotp"]
# dat3[varname_ext == "R8_HU_D2_wkhtotp"][order(value)]

estimICC <- function(x) {
  cat(which(x == varname_list), "/", length(varname_list), ":", x, "\n")
  data.table(varname_ext = x,
             ICC = max(0, ICC::ICCbare(x = factor(PSU),
                                       y = lin_val,
                                       data = dat3[.(x)])))
}

estimICC(sample(varname_list, 1))

# dat_ICC_test <- lapply(sample(varname_list, 1e3), estimICC)
# dat_ICC_test <- rbindlist(dat_ICC_test)
# dat_ICC_test
#
# dat_ICC_test[, cor(ICC, ICCd)]
# qplot(x = ICC, y = ICCd, data = dat_ICC_test)

# R9_HU_D2
# dat_ICC_R9_HU_D2 <- lapply(grep("R9_HU_D2", varname_list, value = T), estimICC)
# dat_ICC_R9_HU_D2 <- rbindlist(dat_ICC_R9_HU_D2)
# dat_ICC_R9_HU_D2
#
# dat_ICC_R9_HU_D2[, .N]
# dat_ICC_R9_HU_D2[, mean(ICC)]
# dat_ICC_R9_HU_D2[, median(ICC)]
#
# x <- dat_ICC_R9_HU_D2[, ICC]
# x <- sort(c(x, rep(0, 75 - length(x))))
# x
#
# mean(x)
# median(x)
# x[round(75 / 2)]


# Real estimation for all rounds and countries

# Options (stop at warning)
options(warn = 2)
gc()

t1 <- Sys.time()
dat_ICC <- lapply(varname_list, estimICC)
t2 <- Sys.time()

print("Time difference:")
print(t2 - t1)
# Time difference of 42.20912 mins
# Time difference of 35.21384 mins  (2020-12-10)
# Time difference of 1.332371 hours (2021-02-05)
# Time difference of 31.49445 mins  (2022-09-17)
# Time difference of 33.56024 mins  (2022-12-08)
# Time difference of 18.51974 mins  (2022-12-11)
# Time difference of 17.67596 mins  (2022-12-22)
# Time difference of 21.22778 mins  (2023-05-14)
# Time difference of 18.59350 mins  (2023-07-13)
# Time difference of 21.96192 mins  (2024-07-08) R11


# Options (stop at error - default)
options(warn = 1)

dat_ICC[1:3]

dat_ICC <- rbindlist(dat_ICC, use.names = T, fill = T)

setkey(dat_ICC, varname_ext)

saveRDS(object = dat_ICC, file = "data/dat_ICC.rds")


# Load
dat_ICC <- readRDS(file = "data/dat_ICC.rds")

dat_ICC

dat_ICC[, summary(ICC)]

dat_ICC[is.na(ICC)]


dat_ICC[, essround := substring(varname_ext, 1, 3)]
dat_ICC[, cntry    := substring(varname_ext, 5, 6)]
dat_ICC[, domain   := substring(varname_ext, 8, 9)]
dat_ICC[, varname  := substring(varname_ext, 11)]


# pl0 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
#   geom_point(alpha = .5) +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
#   theme_bw()
#
# pl1 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
#   geom_point(alpha = .5) +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   facet_wrap(~ essround) +
#   ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
#   theme_bw()
#
# pl2 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2, colour = domain)) +
#   geom_point(alpha = .5) +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   facet_wrap(~ cntry) +
#   ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
#   theme_bw()
#
# pl3 <- ggplot(data = dat_ICC, mapping = aes(x = ICC1, y = ICC2)) +
#   geom_point(alpha = .5) +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   facet_wrap(~ varname) +
#   ggtitle("Intraclass correlation coefficient (ICC or ρ)") +
#   theme_bw()
#
# dat_ICC3 <- melt.data.table(data = dat_ICC,
#                             id.vars = c("varname_ext", "essround", "cntry",
#                                         "domain", "varname"),
#                             measure.vars = c("ICC1", "ICC2"))
#
# pl4 <- ggplot(data = dat_ICC3, mapping = aes(x = value, colour = variable)) +
#   geom_density() +
#   facet_wrap(~ varname, scales = "free") +
#   ggtitle("Density of intraclass correlation coefficient (ICC or ρ)") +
#   theme_bw()
#
# cairo_pdf(filename = "results/ICC1_ICC2.pdf", width = 16, height = 9, onefile = T)
# pl0
# pl1
# pl2
# pl3
# pl4
# dev.off()
#
# dat_ICC[, lapply(.SD, sd), .SDcols = c("ICC1", "ICC2"),
#         keyby = .(varname)][, .N, keyby = .(ICC1 > ICC2)]
#
# fwrite(x = dat_ICC, file = "results/ICC1_ICC2.csv")

