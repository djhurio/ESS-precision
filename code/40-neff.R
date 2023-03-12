# Calculate effective sample size (n_eff)

# Reset
rm(list = ls())
gc()

# Set ggplot2 theme
theme_set(theme_bw())

# Load data
dat2 <- readRDS(file = "data/dat2.rds")
tab_variables <- readRDS(file = "data/tab_variables.rds")
dat_ICC <- readRDS(file = "data/dat_ICC.rds")
tab_cf_cntry <- readRDS("data/tab_cf_cntry.rds")

dat2[, domain := paste0("D", domain)]
dat2[, .N, keyby = .(domain)]

tab_variables[, domain := paste0("D", domain)]
tab_variables[, .N, keyby = .(domain)]



# # Test ICC estimation
# tab_variables[essround == 9, .N, keyby = .(essround, cntry, domain, flag)]
#
# dcast.data.table(data = tab_variables[essround == 9],
#                  formula = essround + cntry + domain ~ flag,
#                  fun.aggregate = length)[order(`FALSE`)]
#
#
# x <- tab_variables[b > 1 & essround == 9 & (flag) & cntry %in% c("HU", "PL"),
#                    .(essround, cntry, domain, varname)]
#
# foo <- function(i) {
#   tab <- dat2[essround == x[i, essround] &
#                 cntry == x[i, cntry] & domain == x[i, domain],
#               .N, keyby = c("essround", "cntry", "domain", x[i, varname])]
#   tab[, varname := x[i, varname]]
#   setnames(tab, x[i, varname], "value")
#   setcolorder(tab, c("essround", "cntry", "domain", "varname", "value"))
#   tab
# }
#
# foo(1)
#
# rbindlist(lapply(x[, .I], foo))
#
# dat2[essround == 9 & cntry == "PL" & domain == 1, .N, keyby = .(crpdwk)]
# dat2[essround == 9 & cntry == "PL" & domain == 2, .N, keyby = .(crpdwk)]




# Label rounds
rounds <- sort(unique(dat2$essround))

# str(dat2$essround)
# str(tab_variables$essround)

dat2[, .N, keyby = .(essround)]
tab_variables[, .N, keyby = .(essround)]

# Average number of respondnets per PSU
dat_b <- dat2[, .N, keyby = .(essround, edition, proddate, cntry,
                              selfcomp, domain, PSU)]
dat_b <- dat_b[, .(b = mean(N)),
               keyby = .(essround, edition, proddate, cntry, selfcomp, domain)]

dat_b
dat_b[order(b)]
dat_b[, summary(b)]

pl_b <- ggplot(dat_b) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = b, fill = domain, alpha = b),
           colour = "black", position = "dodge") +
  ggtitle(label = "ESS avearge cluster size (b)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)


# deff_p
dat_deff_p <- dat2[, .(deff_p = .N * sum(weight_des ^ 2) / sum(weight_des) ^ 2),
                   keyby = .(essround, edition, proddate, cntry,
                             selfcomp, domain)]

dat_deff_p
dat_deff_p[, as.list(summary(deff_p))]
dat_deff_p[, as.list(summary(deff_p)), keyby = .(essround)]
dat_deff_p[order(deff_p)]

dcast.data.table(data = dat_deff_p, formula = cntry + domain ~ essround,
                 value.var = "deff_p",
                 fun.aggregate = round, digits = 3, fill = NA)

# tmp <- import_sddf_country(country = "Germany", rounds = 5)
# setDT(tmp)
# tmp[order(prob)]
# tmp[, summary(prob)]
# hist(tmp$prob)

# tmp <- import_sddf_country(country = "Israel", rounds = 1)
# setDT(tmp)
# tmp[order(prob)]
# tmp[, summary(prob)]
# hist(tmp$prob)

pl_deff_p <- ggplot(dat_deff_p) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = deff_p, fill = domain, alpha = deff_p),
           colour = "black", position = "dodge") +
  ggtitle(label = paste("ESS design effect due to differencies",
                        "in sampling probabilities (deff_p)"),
          subtitle = Sys.time()) +
  facet_wrap(~ cntry)


# Merge
dat_deff <- merge(tab_variables, dat_ICC,
                  by = "varname_ext", all.x = T)

dat_deff[, varname_ext := NULL]

dat_deff[, .N, keyby = .(flag, b_is_1 = (b == 1), naICC = is.na(ICC))]

dat_deff <- merge(dat_deff_p, dat_deff,
                  by = c("essround", "cntry", "domain"), all = T)

setcolorder(
  x = dat_deff,
  neworder = c("essround", "edition", "proddate", "cntry", "selfcomp", "domain")
)

dat_deff[, summary(b)]

dat_deff[, summary(ICC)]

dat_deff[b == 1, summary(ICC)]
dat_deff[(flag), summary(ICC)]
dat_deff[!(flag) & b > 1, summary(ICC)]

dat_deff[(flag) | b == 1, ICC := 0]

dat_deff[, summary(ICC)]
dat_deff[b > 1, summary(ICC)]

dat_deff[, deff_c := 1 + (b - 1) * ICC]

dat_deff[, summary(deff_c)]
dat_deff[, as.list(summary(deff_c)), keyby = .(flag)]

dat_deff[, deff := deff_p * deff_c]

dat_deff

dat_deff[, as.list(summary(deff))]
dat_deff[, as.list(summary(deff)), keyby = .(essround)]
dat_deff[, as.list(summary(deff)), keyby = .(essround, selfcomp)]
dat_deff[, as.list(summary(deff)), keyby = .(cntry)][order(Mean)]
dat_deff[, as.list(summary(deff)), keyby = .(varname)][order(Mean)]

dcast.data.table(data = dat_deff,
                 formula = cntry + domain ~ essround,
                 fun.aggregate = function(x) round(mean(x), 3),
                 value.var = "deff")

dat_deff[is.na(deff), .N]

names(dat_deff)



# Effective sample size
dat_deff[, n_eff := n_resp / deff]


# plot_ICC_varname_domain <- function(x) {
#   ggplot(data = dat_deff[cntry == x],
#          mapping = aes(x = essround, y = ICC, fill = ICC, linetype = domain)) +
#     geom_col(colour = "black", position = "dodge") +
#     geom_hline(yintercept = 0) +
#     scale_fill_gradient2(low  = scales::muted("blue"),
#                          mid  = "white",
#                          high = scales::muted("red")) +
#     facet_wrap(~ varname) +
#     ggtitle(label = paste("Intraclass correlation coefficient (ICC or ρ)",
#                           "by variable for", x),
#             subtitle = Sys.time())
# }
#
# plot_ICC_varname_domain("FI")
# plot_ICC_varname_domain("BG")
# plot_ICC_varname_domain("LT")
# plot_ICC_varname_domain("LV")


# # Aggregate to round / cntry
# dat_deff_2 <- dat_deff[, c(.(n_domains = .N), lapply(.SD, sum)),
#                        .SDcols = c("n_resp", "n_na",
#                                    "total_Y", "total_Z", "pop_size", "n_eff"),
#                        keyby = .(essround, cntry, varname)]
# dat_deff_2
#
# # Min effective sample size
# dat_deff_2[, min_n_eff := ifelse(pop_size < 2e6, 800, 1500)]
#
# # Evaluation
# dat_deff_2[, assessment := (n_eff >= min_n_eff)]
#
# plot_neff_varname <- function(x) {
#   ggplot(data = dat_deff_2[cntry == x],
#          mapping = aes(x = essround, y = n_eff, fill = assessment)) +
#     geom_col(alpha = .5) +
#     geom_hline(yintercept = dat_deff_2[cntry == x, unique(min_n_eff)]) +
#     scale_fill_manual(values = c("TRUE"  = "blue",
#                                  "FALSE" = "red")) +
#     facet_wrap(~ varname) +
#     ggtitle(paste("Effective sample size by variable for", x))
# }
#
# plot_neff_varname("FI")
# plot_neff_varname("BG")
# plot_neff_varname("LT")
# plot_neff_varname("LV")


# Aggregate to round / cntry / domain
# ICC aggregation by median!
tab_deff <- dat_deff[
  , c(.(n_variables = as.numeric(.N)),
      lapply(.SD, mean),
      .(ICC = median(ICC))),
  .SDcols = c("n_resp", "pop_size", "b", "deff_p"),
  keyby = .(essround, cntry, selfcomp, edition, proddate, domain)
]

tab_deff <- merge(
  x = tab_cf_cntry[, .(essround, cntry, selfcomp, typesamp)],
  y = tab_deff,
  by = c("essround", "cntry", "selfcomp"),
  all = TRUE
)

tab_deff
tab_deff[, mean(n_variables), keyby = .(essround, selfcomp)]
tab_deff[, sum(n_variables)]

# tab_deff[, all.equal(deff_c, 1 + (b - 1) * ICC_mean)]
# tab_deff[, all.equal(deff, deff_p * deff_c)]
tab_deff[, deff_c := 1 + (b - 1) * ICC]
tab_deff[, deff := deff_p * deff_c]

# Effective sample
tab_deff[, n_eff := n_resp / deff]


tab_deff[, summary(ICC)]

# m <- tab_deff[, max(ICC_mean, ICC_median)]
#
# # ICC mean vs ICC median
# pl_ICC_test1 <- ggplot(data = tab_deff[b > 1],
#                        mapping = aes(x = ICC_mean, y = ICC_median)) +
#   geom_point(mapping = aes(colour = essround)) +
#   geom_abline(slope = 1, intercept = 0) +
#   coord_equal() + xlim(c(0, m)) + ylim(c(0, m)) +
#   ggtitle(label = "ICC: mean vs median", subtitle = Sys.time())
# pl_ICC_test1
#
#
# id_vars <- c("essround", "cntry", "selfcomp", "domain")
# tab_ICC_test2 <- melt.data.table(
#   data = tab_deff[b > 1],
#   id.vars = id_vars,
#   measure.vars = patterns("^ICC_")
# )
# setkeyv(tab_ICC_test2, c(id_vars, "variable"))
# key(tab_ICC_test2)
# tab_ICC_test2[, flag := value[1] < value[2], by = id_vars]
# tab_ICC_test2[(flag)]
# pl_ICC_test2 <- ggplot(data = tab_ICC_test2,
#                        mapping = aes(x = essround,
#                                      y = value,
#                                      fill = flag,
#                                      linetype = variable)) +
#   geom_col(colour = "black", position = "dodge") +
#   facet_wrap(facets = vars(paste(cntry, domain))) +
#   ggtitle(label = "ICC: mean vs median", subtitle = Sys.time())
# pl_ICC_test2
#
# ggsave(filename = "plots/pl_ICC_test1.png", plot = pl_ICC_test1,
#        width = 8, height = 4.5)
# ggsave(filename = "plots/pl_ICC_test2.png", plot = pl_ICC_test2,
#        width = 16, height = 9)

pl_ICC <- ggplot(tab_deff) +
  geom_col(aes(x = essround, y = ICC, fill = domain, alpha = ICC),
           colour = "black", position = "dodge") +
  ggtitle(label = "ESS Intraclass correlation coefficient (ICC or ρ)",
          subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_deff_c <- ggplot(tab_deff) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = deff_c, fill = domain, alpha = deff_c),
           colour = "black", position = "dodge") +
  ggtitle(label = "ESS design effect due to clustering (deff_c)",
          subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_deff_by_dom <- ggplot(tab_deff) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = deff, fill = domain, alpha = deff),
           colour = "black", position = "dodge") +
  ggtitle(label = "ESS design effect (deff) by domains",
          subtitle = Sys.time()) +
  facet_wrap(~ cntry)

# tab_deff[, sapply(.SD, class)]
# tab_deff_long <- melt.data.table(data = tab_deff, id.vars = key(tab_deff))
#
# plot_cntry_dashboard <- function(x) {
#   ggplot(data = tab_deff_long[cntry == x],
#          mapping = aes(x = essround, y = value, fill = essround,
#                        linetype = domain)) +
#     geom_col(colour = "black", alpha = .5, position = "dodge") +
#     facet_wrap(~ variable, scales = "free_y") +
#     ggtitle(paste("Sample design dashboard for", x, "by domain"))
# }

# plot_cntry_dashboard("FI")
# plot_cntry_dashboard("BG")
# plot_cntry_dashboard("LT")
# plot_cntry_dashboard("LV")
# plot_cntry_dashboard("HU")



# Aggregate to cntry
tab_deff_2 <- tab_deff[
  , c(.(n_domains = as.numeric(.N),
        n_variables = mean(n_variables)),
      lapply(.SD, sum)),
  .SDcols = c("n_resp", "pop_size", "n_eff"),
  keyby = .(essround, cntry, selfcomp, edition, proddate)]

key(tab_cf_cntry)
key(tab_deff_2)

tab_deff_2 <- merge(tab_cf_cntry, tab_deff_2,
                    by = c("essround", "cntry", "selfcomp"),
                    all = T)

if (tab_deff_2[, !isTRUE(all.equal(n_net, n_resp))]) {
  stop("Number of respondents do not match")
}

tab_deff_2[, n_resp := NULL]
setnames(tab_deff, "n_resp", "n_net")


# Aggregated dessign effect
tab_deff_2[, deff := n_net / n_eff]

# Min effective sample size
tab_deff_2[, min_n_eff := ifelse(pop_size < 2e6, 800L, 1500L)]

# Evaluation
tab_deff_2[, assessment := (n_eff >= min_n_eff)]
tab_deff_2[, rr_assess := (rr > 0.70)]

pl_n_gross <- ggplot(tab_deff_2) +
  geom_col(aes(x = essround, y = n_gross, alpha = n_gross), colour = "black") +
  ggtitle(label = "ESS gross sample size (n_gross)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_n_net <- ggplot(tab_deff_2) +
  geom_col(aes(x = essround, y = n_net, alpha = n_net), colour = "black") +
  ggtitle(label = "ESS net sample size (n_net)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_rr <- ggplot(tab_deff_2[!is.na(rr)]) +
  geom_hline(yintercept = 0.70, colour = "red") +
  geom_col(aes(x = essround, y = rr, fill = rr_assess, alpha = rr),
           colour = "black") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  ggtitle(label = "ESS response rate (rr)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_ri <- ggplot(tab_deff_2[!is.na(ri)]) +
  geom_col(aes(x = essround, y = ri, alpha = ri), colour = "black") +
  ggtitle(label = "ESS ineligibility rate (ri)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_deff <- ggplot(tab_deff_2) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = deff, alpha = deff), colour = "black") +
  ggtitle(label = "ESS design effect (deff)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)

pl_neff <- ggplot(tab_deff_2) +
  geom_hline(mapping = aes(yintercept = min_n_eff), colour = "red") +
  geom_col(aes(x = essround, y = n_eff, fill = assessment, alpha = n_eff),
           colour = "black") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  ggtitle(label = "ESS effective sample size (n_eff)", subtitle = Sys.time()) +
  facet_wrap(~ cntry)


# Aggregate to round
tab_deff_3 <- tab_deff_2[, c(.(n_cntries = .N), lapply(.SD, sum)),
                         .SDcols = c("pop_size", "n_gross", "n_net",
                                     "n_eff", "min_n_eff"),
                         keyby = .(essround)]

# Aggregated dessign effect
tab_deff_3[, deff := n_net / n_eff]

# Evaluation
tab_deff_3[, assessment := (n_eff >= min_n_eff)]

# pl_deff_ess <- ggplot(tab_deff_3) +
#   geom_col(aes(x = essround, y = deff, fill = essround),
#            colour = "black", position = "dodge") +
#   ggtitle("ESS total design effect (deff)")
#
# pl_neff_ess <- ggplot(tab_deff_3) +
#   geom_col(aes(x = essround, y = n_eff, fill = essround),
#            colour = "black", position = "dodge") +
#   geom_hline(mapping = aes(yintercept = min_n_eff, colour = essround)) +
#   ggtitle("ESS total effective sample size (n_eff)")

tab_deff_3[, sapply(.SD, class)]
x <- names(tab_deff_3)[map_lgl(tab_deff_3, is.integer)]
tab_deff_3[, c(x) := map(.SD, as.double), .SDcols = x]
rm(x)
tab_deff_3[, sapply(.SD, class)]

tab_deff_long_3 <- melt.data.table(
  data = tab_deff_3,
  id.vars = c("essround", "assessment"),
  measure.vars = c("n_gross", "n_net", "n_eff", "min_n_eff")
)

tab_deff_3
tab_deff_long_3

round(seq(0, 1, .1))


formatC(0.999999, digits = 3)
sprintf("%.3f", 0.999999)


pl_round <- ggplot(
  data = tab_deff_long_3,
  mapping = aes(x = essround, y = value, fill = variable, alpha = value)
) +
  geom_col(colour = "black", position = "dodge") +
  geom_label(mapping = aes(x = essround, y = max(n_net),
                           label = (sprintf("%.3f", deff))),
             data = tab_deff_3,
             vjust = -1,
             inherit.aes = F) +
  scale_y_continuous(labels = \(x) formatC(x, big.mark = ",", format = "d")) +
  ggtitle(label = "ESS total sample size (bars) and deff (numbers)")


# tab_deff_2[, sapply(.SD, class)]
# tab_deff_long_2 <- melt.data.table(
#   data = tab_deff_2,
#   id.vars = c("cntry", "essround", "selfcomp", "min_n_eff"),
#   measure.vars = c("pop_size", "n_variable", "n_domains",
#                    "n_net", "deff", "n_eff")
# )
#
# tab_deff_2
# tab_deff_long_2
#
# plot_cntry_dashboard_2 <- function(x) {
#   ggplot(data = tab_deff_long_2[cntry == x],
#          mapping = aes(x = essround, y = value, fill = essround)) +
#     geom_col(colour = "black", alpha = .5, position = "dodge") +
#     geom_hline(mapping = aes(yintercept = min_n_eff),
#                data = tab_deff_long_2[cntry == x & variable == "n_eff"]) +
#     facet_wrap(~ variable, scales = "free_y") +
#     ggtitle(label = paste("Sample design dashboard for", x),
#             subtitle = Sys.time())
# }

# plot_cntry_dashboard_2("FI")
# plot_cntry_dashboard_2("BG")
# plot_cntry_dashboard_2("LT")
# plot_cntry_dashboard_2("LV")


# cntry_list <- sort(unique(dat_deff$cntry))
# length(cntry_list)





# ICC exploring
dat_deff[, mean(ICC)]
dat_deff[, mean(ICC), keyby = .(type)]

dcast.data.table(data = dat_deff, formula = essround ~ type,
                 fun.aggregate = mean, value.var = "ICC")

dcast.data.table(data = dat_deff, formula = cntry ~ type,
                 fun.aggregate = mean, value.var = "ICC")

tab <- dat_deff[, .(ICC = median(ICC)),
                keyby = .(essround, cntry, domain, type)]

pl_ICC_var_type <- ggplot(data = tab,
                          mapping = aes(x = type, y = ICC,
                                        fill = type, linetype = domain)) +
  geom_col(colour = "black", alpha = 1, position = "dodge") +
  facet_grid(essround ~ cntry) +
  theme(axis.text.x = element_blank()) +
  ggtitle(label = "Mean ICC by variable type", subtitle = Sys.time())

ggsave(filename = "plots/plot_ICC_by_variable_type.pdf",
       plot = pl_ICC_var_type, width = 16, height = 9)
ggsave(filename = "plots/plot_ICC_by_variable_type.png",
       plot = pl_ICC_var_type, width = 16, height = 9)



# Save for all countries ###

dir.create(file.path("results", Sys.Date()), showWarnings = F)

setkey(tab_deff_2, essround, cntry, edition, proddate, typesamp, selfcomp)
setkey(tab_deff,   essround, cntry, edition, proddate, typesamp, selfcomp, domain)
setkey(dat_deff,   essround, cntry, edition, proddate, selfcomp, domain, type, varname)

setcolorder(tab_deff_2)
setcolorder(tab_deff)
setcolorder(dat_deff)

map_chr(tab_deff_2, class)
map_chr(tab_deff,   class)
map_chr(dat_deff,   class)

# x <- c("n_domains", "n_variable", "n_net")
# tab_deff_2[, c(x) := map(.SD, as.integer), .SDcols = x]
#
# x <- c("n_variable", "n_net")
# tab_deff[, c(x) := map(.SD, as.integer), .SDcols = x]

# options("openxlsx.numFmt" = "0.000")
# write.xlsx(
#   x = list("deff_round" = tab_deff_3,
#            "deff_cntry" = tab_deff_2,
#            "deff_domain" = tab_deff,
#            "deff_variable" = dat_deff),
#   file = glue::glue("results/{Sys.Date()}/ESS_dat_deff_{Sys.Date()}.xlsx"),
#   headerStyle = createStyle(textDecoration = "italic",
#                             halign = "center"),
#   firstRow = T,
#   withFilter = T
# )
#
# options("openxlsx2.numFmt" = "0.000")
# openxlsx2::write_xlsx(
#   x = list("deff_round" = tab_deff_3,
#            "deff_cntry" = tab_deff_2,
#            "deff_domain" = tab_deff,
#            "deff_variable" = dat_deff),
#   file = glue::glue("results/{Sys.Date()}/ESS_dat_deff_{Sys.Date()}.xlsx"),
#   headerStyle = createStyle(textDecoration = "italic",
#                             halign = "center"),
#   firstRow = T,
#   withFilter = T
# )


cell_formats <- list(
  proddate      = list(numfmt = "YYYY-MM-DD", widths = 10),
  pop_size      = list(numfmt = "#,##0", widths = "auto"),
  total_Y       = list(numfmt = "#,##0", widths = "auto"),
  total_Z       = list(numfmt = "#,##0", widths = "auto"),
  n_cntries     = list(numfmt = "0", widths = 9),
  n_domains     = list(numfmt = "0", widths = 9),
  n_variables   = list(numfmt = "0", widths = 9),
  n_gross       = list(numfmt = "0", widths = 9),
  n_ineligibles = list(numfmt = "0", widths = 9),
  n_resp        = list(numfmt = "0", widths = 9),
  n_na          = list(numfmt = "0", widths = 9),
  n_net         = list(numfmt = "0", widths = 9),
  n_eff         = list(numfmt = "0", widths = 9),
  min_n_eff     = list(numfmt = "0", widths = 9),
  sd_y          = list(numfmt = "0.000", widths = 9),
  max_sd_y_psu  = list(numfmt = "0.000", widths = 9),
  ratio         = list(numfmt = "0.000", widths = 9),
  rr            = list(numfmt = "0.000", widths = 9),
  ri            = list(numfmt = "0.000", widths = 9),
  b             = list(numfmt = "0.000", widths = 9),
  ICC           = list(numfmt = "0.000", widths = 9),
  deff_c        = list(numfmt = "0.000", widths = 9),
  deff_p        = list(numfmt = "0.000", widths = 9),
  deff          = list(numfmt = "0.000", widths = 9),
  assessment    = list(numfmt = "BOOLEAN", widths = 9),
  selfcomp      = list(numfmt = "BOOLEAN", widths = 9),
  rr_assess     = list(numfmt = "BOOLEAN", widths = 9),
  varname       = list(numfmt = "@", widths = "auto"),
  typesamp      = list(numfmt = "@", widths = "auto"),
  type          = list(numfmt = "@", widths = "auto"),
  flag          = list(numfmt = "@", widths = "auto")
)

# cell_formats[["flag"]]

wb_data <- list(round    = tab_deff_3,
                cntry    = tab_deff_2,
                domain   = tab_deff,
                variable = dat_deff)

wb <- wb_workbook(creator = Sys.info()[["user"]])

for (x in names(wb_data)) {
  dat_x <- wb_data[[x]]
  n <- dat_x[, .N]
  wb$
    add_worksheet(sheet = x, zoom = ifelse(x == "variable", 100, 110))$
    add_data_table(sheet = x, dat_x)$
    freeze_pane(sheet = x, firstRow = TRUE)
  for (i in seq_along(dat_x)) {
    varnam <- names(dat_x)[i]
    if (varnam %in% names(cell_formats)) {
      wb$
        add_numfmt(sheet = x,
                   dims = paste0(int2col(i), c(1, n) + 1, collapse = ":"),
                   numfmt = cell_formats[[varnam]]$numfmt)$
        set_col_widths(sheet = x,
                       cols = i,
                       widths = cell_formats[[varnam]]$widths)
    }
  }
}

wb_save(
  wb = wb,
  path = glue::glue("results/{Sys.Date()}/ESS_dat_deff_{Sys.Date()}.xlsx"),
  overwrite = T
)


cairo_pdf(glue::glue("results/{Sys.Date()}/ESS_plot_deff_{Sys.Date()}.pdf"),
          width = 16, height = 9, onefile = T)
print(pl_round)
print(pl_rr)
print(pl_ri)
print(pl_b)
print(pl_ICC)
print(pl_deff_c)
print(pl_deff_p)
print(pl_deff_by_dom)
print(pl_deff)
print(pl_neff)
dev.off()


# export_cntry_results <- function(x) {
#   cat(x, "\n")
#
#   fname.xlsx <- paste0("results/cntry/ESS-results-", x, ".xlsx")
#   fname.pdf <- sub("xlsx", "pdf", fname.xlsx)
#
#   write.xlsx(list("deff_cntry" = tab_deff_2[cntry == x],
#                   "deff_cntry_domain" = tab_deff[cntry == x],
#                   "deff_cntry_domain_variable" = dat_deff[cntry == x],
#                   "deff_cntry_domain_variable_all" = tab_variables[cntry == x]),
#              file = fname.xlsx,
#              colWidths = "auto", firstRow = T,
#              headerStyle = createStyle(textDecoration = "italic",
#                                        halign = "center"))
#
#   cairo_pdf(fname.pdf, width = 16, height = 9, onefile = T)
#   print(plot_cntry_dashboard_2(x))
#   print(plot_cntry_dashboard(x))
#   print(plot_ICC_varname_domain(x))
#   dev.off()
# }
#
# # export_cntry_results("LV")
#
# dir.create(path = "results/cntry", showWarnings = F)
#
# for (i in cntry_list) {
#   cat(i, ": ")
#   export_cntry_results(i)
# }
#
# # list.files(path = "results/cntry", full.names = T)
#
# fname <- "results/ESS-cntry-results.zip"
# if (file.exists(fname)) file.remove(fname)
# zip::zip(zipfile = fname,
#          files = list.files(path = "results/cntry", full.names = T),
#          mode = "cherry-pick")
