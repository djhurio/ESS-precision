# Calculate effective sample size (n_eff)

# Reset
rm(list = ls())
gc()

# Set ggplot2 theme
theme_set(theme_bw())

# Functions
est_deff_p = function(x) {
  length(x) * sum(x^2) / (sum(x)^2)
}

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

# Average number of respondents per PSU
dat_b <- dat2[,
  .N,
  keyby = .(essround, edition, proddate, cntry, selfcomp, domain, PSU)
]
dat_b <- dat_b[,
  .(b = mean(N)),
  keyby = .(essround, edition, proddate, cntry, selfcomp, domain)
]

dat_b
dat_b[cntry == "CZ"]
dat_b[cntry == "UA"]
dat_b[order(b)]
dat_b[, summary(b)]

dcast.data.table(
  data = dat_b,
  formula = cntry + domain ~ essround,
  value.var = "b",
  fun.aggregate = round,
  digits = 3,
  fill = NA
)

pl_b <- ggplot(dat_b) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(
    aes(x = essround, y = b, fill = domain, alpha = b),
    colour = "black",
    position = "dodge"
  ) +
  ggtitle(label = "ESS avearge cluster size (b)", subtitle = Sys.time()) +
  facet_wrap(~cntry)


# deff_p
dat_deff_p <- dat2[,
  .(deff_p = est_deff_p(weight_des)),
  keyby = .(essround, edition, proddate, cntry, selfcomp, domain)
]

dat_deff_p
dat_deff_p[cntry == "CZ"]
dat_deff_p[cntry == "UA"]
dat_deff_p[, as.list(summary(deff_p))]
dat_deff_p[, as.list(summary(deff_p)), keyby = .(essround)]
dat_deff_p[order(deff_p)]

dcast.data.table(
  data = dat_deff_p,
  formula = cntry + domain ~ essround,
  value.var = "deff_p",
  fun.aggregate = round,
  digits = 3,
  fill = NA
)

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
  geom_col(
    aes(x = essround, y = deff_p, fill = domain, alpha = deff_p),
    colour = "black",
    position = "dodge"
  ) +
  ggtitle(
    label = paste(
      "ESS design effect due to differencies",
      "in sampling probabilities (deff_p)"
    ),
    subtitle = Sys.time()
  ) +
  facet_wrap(~cntry)


# Merge
dat_deff <- merge(
  x = tab_variables,
  y = dat_ICC,
  by = "varname_ext",
  all.x = T
)

dat_deff[, varname_ext := NULL]

dat_deff[, .N, keyby = .(flag, b_is_1 = (b == 1), naICC = is.na(ICC))]

dat_deff <- merge(
  x = dat_deff_p,
  y = dat_deff,
  by = c("essround", "cntry", "domain"),
  all = T
)

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

dat_deff[, .(cntry, domain, essround)]

dcast.data.table(
  data = dat_deff,
  formula = cntry + domain ~ essround,
  value.var = "deff_c",
  fun.aggregate = function(x) round(median(x), 3),
  fill = NA
)

dat_deff[, deff := deff_p * deff_c]

dat_deff

dat_deff[, as.list(summary(deff))]
dat_deff[, as.list(summary(deff)), keyby = .(essround)]
dat_deff[, as.list(summary(deff)), keyby = .(essround, selfcomp)]
dat_deff[, as.list(summary(deff)), keyby = .(cntry)][order(Mean)]
dat_deff[, as.list(summary(deff)), keyby = .(varname)][order(Mean)]

dcast.data.table(
  data = dat_deff,
  formula = cntry + domain ~ essround,
  fun.aggregate = function(x) round(mean(x), 3),
  value.var = "deff"
)

dat_deff[is.na(deff), .N]

names(dat_deff)


# Effective sample size
dat_deff[, n_eff := n_resp / deff]


# Aggregate to round / cntry / domain
# ICC aggregation by median!
tab_deff <- dat_deff[,
  c(.(n_variables = as.numeric(.N)), lapply(.SD, mean), .(ICC = median(ICC))),
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
tab_deff[cntry == "CZ"]
tab_deff[cntry == "UA"]
tab_deff[, mean(n_variables), keyby = .(essround, selfcomp)]
tab_deff[, sum(n_variables)]

# tab_deff[, all.equal(deff_c, 1 + (b - 1) * ICC_mean)]
# tab_deff[, all.equal(deff, deff_p * deff_c)]
tab_deff[, deff_c := 1 + (b - 1) * ICC]
tab_deff[, deff := deff_p * deff_c]

# Effective sample
tab_deff[, n_eff := n_resp / deff]

tab_deff[, summary(ICC)]

pl_ICC <- ggplot(tab_deff) +
  geom_col(
    aes(x = essround, y = ICC, fill = domain, alpha = ICC),
    colour = "black",
    position = "dodge"
  ) +
  ggtitle(
    label = "ESS Intraclass correlation coefficient (ICC or ρ)",
    subtitle = Sys.time()
  ) +
  facet_wrap(~cntry)

pl_deff_c <- ggplot(tab_deff) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(
    aes(x = essround, y = deff_c, fill = domain, alpha = deff_c),
    colour = "black",
    position = "dodge"
  ) +
  ggtitle(
    label = "ESS design effect due to clustering (deff_c)",
    subtitle = Sys.time()
  ) +
  facet_wrap(~cntry)

pl_deff_by_dom <- ggplot(tab_deff) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(
    aes(x = essround, y = deff, fill = domain, alpha = deff),
    colour = "black",
    position = "dodge"
  ) +
  ggtitle(
    label = "ESS design effect (deff) by domains",
    subtitle = Sys.time()
  ) +
  facet_wrap(~cntry)


# Aggregate to cntry
tab_deff_2 <- tab_deff[,
  c(
    .(n_domains = as.numeric(.N), n_variables = mean(n_variables)),
    lapply(.SD, sum)
  ),
  .SDcols = c("n_resp", "pop_size", "n_eff"),
  keyby = .(essround, cntry, selfcomp, edition, proddate)
]

key(tab_cf_cntry)
key(tab_deff_2)

tab_deff_2 <- merge(
  x = tab_cf_cntry,
  y = tab_deff_2,
  by = c("essround", "cntry", "selfcomp"),
  all = T
)

if (tab_deff_2[, !isTRUE(all.equal(n_net, n_resp))]) {
  print(tab_deff_2[n_net != n_resp | is.na(n_net) | is.na(n_resp)])
  warning("Number of respondents do not match")
}

tab_deff_2[, n_net := NULL]
setnames(tab_deff_2, "n_resp", "n_net")
setnames(tab_deff, "n_resp", "n_net")

# Aggregated design effect
tab_deff_2[, deff := n_net / n_eff]

# Min effective sample size
tab_deff_2[, min_n_eff := ifelse(pop_size < 2e6, 800L, 1500L)]

# Evaluation
tab_deff_2[, assessment := (n_eff >= min_n_eff)]
tab_deff_2[, rr_assess := (rr > 0.70)]

pl_n_gross <- ggplot(tab_deff_2) +
  geom_col(aes(x = essround, y = n_gross, alpha = n_gross), colour = "black") +
  ggtitle(label = "ESS gross sample size (n_gross)", subtitle = Sys.time()) +
  facet_wrap(~cntry)

pl_n_net <- ggplot(tab_deff_2) +
  geom_col(aes(x = essround, y = n_net, alpha = n_net), colour = "black") +
  ggtitle(label = "ESS net sample size (n_net)", subtitle = Sys.time()) +
  facet_wrap(~cntry)

pl_rr <- ggplot(tab_deff_2[!is.na(rr)]) +
  geom_hline(yintercept = 0.70, colour = "red") +
  geom_col(
    aes(x = essround, y = rr, fill = rr_assess, alpha = rr),
    colour = "black"
  ) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  ggtitle(label = "ESS response rate (rr)", subtitle = Sys.time()) +
  facet_wrap(~cntry)

pl_ri <- ggplot(tab_deff_2[!is.na(ri)]) +
  geom_col(aes(x = essround, y = ri, alpha = ri), colour = "black") +
  ggtitle(label = "ESS ineligibility rate (ri)", subtitle = Sys.time()) +
  facet_wrap(~cntry)

pl_deff <- ggplot(tab_deff_2) +
  geom_hline(yintercept = 1, colour = "red") +
  geom_col(aes(x = essround, y = deff, alpha = deff), colour = "black") +
  ggtitle(label = "ESS design effect (deff)", subtitle = Sys.time()) +
  facet_wrap(~cntry)

pl_neff <- ggplot(tab_deff_2) +
  geom_hline(mapping = aes(yintercept = min_n_eff), colour = "red") +
  geom_col(
    aes(x = essround, y = n_eff, fill = assessment, alpha = n_eff),
    colour = "black"
  ) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  ggtitle(label = "ESS effective sample size (n_eff)", subtitle = Sys.time()) +
  facet_wrap(~cntry)


# Aggregate to round
tab_deff_3 <- tab_deff_2[,
  c(.(n_cntries = .N), lapply(.SD, sum)),
  .SDcols = c("pop_size", "n_gross", "n_net", "n_eff", "min_n_eff"),
  keyby = .(essround)
]

# Aggregated design effect
tab_deff_3[, deff := n_net / n_eff]

# Evaluation
tab_deff_3[, assessment := (n_eff >= min_n_eff)]

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

pl_round <- ggplot(
  data = tab_deff_long_3[!is.na(value)],
  mapping = aes(x = essround, y = value, fill = variable)
) +
  geom_col(colour = "black", position = "dodge") +
  geom_label(
    mapping = aes(
      x = essround,
      y = max(n_net),
      label = (sprintf("%.3f", deff))
    ),
    data = tab_deff_3,
    vjust = -1,
    inherit.aes = F
  ) +
  scale_y_continuous(labels = \(x) formatC(x, big.mark = ",", format = "d")) +
  ggtitle(label = "ESS total sample size (bars) and deff (numbers)")


# ICC exploring
dat_deff[, mean(ICC)]
dat_deff[, mean(ICC), keyby = .(type)]

dcast.data.table(
  data = dat_deff,
  formula = essround ~ type,
  fun.aggregate = mean,
  value.var = "ICC"
)

dcast.data.table(
  data = dat_deff,
  formula = cntry ~ type,
  fun.aggregate = mean,
  value.var = "ICC"
)

tab <- dat_deff[,
  .(ICC = median(ICC)),
  keyby = .(essround, cntry, domain, type)
]

pl_ICC_var_type <- ggplot(
  data = tab,
  mapping = aes(x = type, y = ICC, fill = type, linetype = domain)
) +
  geom_col(colour = "black", alpha = 1, position = "dodge") +
  facet_grid(essround ~ cntry) +
  theme(axis.text.x = element_blank()) +
  ggtitle(label = "Mean ICC by variable type", subtitle = Sys.time())

ggsave(
  filename = "plots/plot_ICC_by_variable_type.pdf",
  plot = pl_ICC_var_type,
  width = 16,
  height = 9
)
ggsave(
  filename = "plots/plot_ICC_by_variable_type.png",
  plot = pl_ICC_var_type,
  width = 16,
  height = 9
)


# Save for all countries ###
dir.create(file.path("results", Sys.Date()), showWarnings = F)

setkey(
  tab_deff_2,
  essround,
  cntry,
  edition,
  proddate,
  typesamp,
  selfcomp
)
setkey(
  tab_deff,
  essround,
  cntry,
  edition,
  proddate,
  typesamp,
  selfcomp,
  domain
)
setkey(
  dat_deff,
  essround,
  cntry,
  edition,
  proddate,
  selfcomp,
  domain,
  type,
  varname
)

setcolorder(tab_deff_2)
setcolorder(tab_deff)
setcolorder(dat_deff)

map_chr(tab_deff_2, class)
map_chr(tab_deff, class)
map_chr(dat_deff, class)

cell_formats <- list(
  proddate = list(numfmt = "YYYY-MM-DD", widths = 10),
  pop_size = list(numfmt = "#,##0", widths = "auto"),
  total_Y = list(numfmt = "#,##0", widths = "auto"),
  total_Z = list(numfmt = "#,##0", widths = "auto"),
  n_cntries = list(numfmt = "0", widths = 9),
  n_domains = list(numfmt = "0", widths = 9),
  n_variables = list(numfmt = "0", widths = 9),
  n_gross = list(numfmt = "0", widths = 9),
  n_ineligibles = list(numfmt = "0", widths = 9),
  n_resp = list(numfmt = "0", widths = 9),
  n_na = list(numfmt = "0", widths = 9),
  n_net = list(numfmt = "0", widths = 9),
  n_eff = list(numfmt = "0", widths = 9),
  min_n_eff = list(numfmt = "0", widths = 9),
  sd_y = list(numfmt = "0.000", widths = 9),
  max_sd_y_psu = list(numfmt = "0.000", widths = 9),
  ratio = list(numfmt = "0.000", widths = 9),
  rr = list(numfmt = "0.000", widths = 9),
  ri = list(numfmt = "0.000", widths = 9),
  b = list(numfmt = "0.000", widths = 9),
  ICC = list(numfmt = "0.000", widths = 9),
  deff_c = list(numfmt = "0.000", widths = 9),
  deff_p = list(numfmt = "0.000", widths = 9),
  deff = list(numfmt = "0.000", widths = 9),
  assessment = list(numfmt = "General", widths = 9),
  selfcomp = list(numfmt = "General", widths = 9),
  rr_assess = list(numfmt = "General", widths = 9),
  varname = list(numfmt = "@", widths = "auto"),
  typesamp = list(numfmt = "@", widths = "auto"),
  type = list(numfmt = "@", widths = "auto"),
  flag = list(numfmt = "@", widths = "auto")
)

# cell_formats[["flag"]]

wb_data <- list(
  round = tab_deff_3,
  cntry = tab_deff_2,
  domain = tab_deff,
  variable = dat_deff
)

wb <- wb_workbook(creator = Sys.info()[["user"]])

# Slow
t1 <- Sys.time()
for (x in names(wb_data)) {
  dat_x <- wb_data[[x]]
  n <- dat_x[, .N]
  wb$add_worksheet(sheet = x, zoom = 90)$add_data_table(
    sheet = x,
    dat_x
  )$freeze_pane(sheet = x, firstRow = TRUE)
  for (i in seq_along(dat_x)) {
    varnam <- names(dat_x)[i]
    if (varnam %in% names(cell_formats)) {
      wb$add_numfmt(
        sheet = x,
        dims = paste0(int2col(i), c(1, n) + 1, collapse = ":"),
        numfmt = cell_formats[[varnam]]$numfmt
      )$set_col_widths(
        sheet = x,
        cols = i,
        widths = cell_formats[[varnam]]$widths
      )
    }
  }
}
t2 <- Sys.time()
print(t2 - t1)

wb_save(
  wb = wb,
  file = glue::glue("results/{Sys.Date()}/ESS_dat_deff_{Sys.Date()}.xlsx"),
  overwrite = T
)

cairo_pdf(
  filename = glue::glue("results/{Sys.Date()}/ESS_plot_deff_{Sys.Date()}.pdf"),
  width = 16,
  height = 9,
  onefile = T
)
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


# CZ
dat2[cntry == "CZ", .(essround, prob, weight_des, dweight)]

dat2[
  cntry == "CZ",
  map(.(1 / prob, weight_des, dweight), est_deff_p),
  keyby = .(essround, selfcomp)
]
