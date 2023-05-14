# ESS survey data
#
# Data is downloaded from the ESS Data Portal
# https://ess-search.nsd.no/
#
# Data is download since round 7 (2014)
# Data should be downloaded manualy and saved in the "data-ess" folder
#
# For rounds 1-6 sample design variables are not available anymore
# For rounds 7-8 integrated and SDDF files should be saved
# For rounds since round 9 only integrated files should be saved
# For round 10 self-completion data file should be saved aditionaly
#
# All data is saved in SAV (SPSS format as ZIP files)

# Reset ####
rm(list = ls())
gc()


# Load 75 variable names to be used for ICC estimation
# Taken from the
# DELIVERABLE NUMBER: 3.6
# DELIVERABLE TITLE: Report on Sample Quality for Round 8
# WORK PACKAGE Number: 3
# SUBMITTED BY: University of Essex
# AUTHOR(S): Peter LYNN, University of Essex
# page 9
# Appendix: Variables used for estimating the intra-cluster correlation, ρ

variables <- openxlsx::read.xlsx(xlsxFile = "variables/ICC-variables.xlsx")
setDT(variables)

variables <- melt.data.table(data = variables,
                             measure.vars = names(variables),
                             na.rm = T,
                             variable.name = "type",
                             value.name = "varname")

variables[, varname := tolower(varname)]

# Corrections for the variable list
# https://myess.upf.edu/portal/g/:spaces:sampling/cst_sampling_weighting/ForumPortlet/topic/topicaa8906cc7f00010138aa844b7c289ac3/2
#
# Add variables
# lvgptnea is replacing lvgptne since the round 5
# dvrcdeva is replacing dvrcdev since the round 5
# scrlgblg is replacing lgblg for the round 10 SC data
x <- data.table(type = "Binary", varname = c("lvgptnea", "dvrcdeva", "scrlgblg"))
variables <- rbind(variables, x)
rm(x)

setorder(variables, type, varname)
variables

if (length(variables$varname) != 75L + 3L) stop("Check ICC variables")


# ESS data
#
# Download ESS data from the ESS Data Portal
# https://ess-search.nsd.no/
# Check if new versions of the files have been published
# Update files as necessary

# delete all temp files
list.files(path = "data-tmp", full.names = T) |> file.remove()

# unzip all data files
for (x in list.files(path = "data-ess", pattern = ".zip$", full.names = T)) {
  cat(x, "\n")
  utils::unzip(zipfile = x, exdir = "data-tmp")
}
rm(x)


# SDDF is seperate for rounds 7-8
# Function to combine main and sddf data files
read.ess <- function(r) {

  # Survey data
  dat_surv <- list.files(
    path = "data-tmp",
    pattern = glue::glue("^ESS{r}e.*sav$"), full.names = T
  ) |> haven::read_sav()
  setDT(dat_surv)

  # SDDF data
  dat_sddf <- list.files(
    path = "data-tmp",
    pattern = glue::glue("^ESS{r}SDDFe.*sav$"), full.names = T
  ) |> haven::read_sav()
  setDT(dat_sddf)

  if (dat_surv[, .N] != dat_sddf[, .N]) stop("Wrong number of rows")

  # Rename
  x <- c("name", "edition", "proddate")
  setnames(x = dat_sddf, old = x, new = paste0(x, "_sddf"))

  dat <- merge(x = dat_surv,
               y = dat_sddf,
               by = c("essround", "cntry", "idno"))

  if (dat_surv[, .N] != dat[, .N]) stop("Wrong merge")

  return(dat)

}

# R7-R8
dat_r7r8 <- map(.x = c(ESS07 = 7L, ESS08 = 8L), .f = read.ess)
names(dat_r7r8)
map_df(dat_r7r8, class)


# R9
dat_r9 <- read_sav(file = list.files(
  path = "data-tmp", pattern = "^ESS9.*sav$", full.names = T
)) |> as.data.table()

# R10
files_r10 <- list.files(
  path = "data-tmp", pattern = "^ESS10.*sav$", full.names = T
)
names(files_r10) <- basename(path = files_r10) |>
  sub(pattern = ".sav$", replacement = "")
names(files_r10)

dat_r10 <- map(.x = files_r10, .f = haven::read_sav) |>
  map(.f = as.data.table)
names(dat_r10)
map_df(dat_r10, class)

# bind and remove
dat <- c(dat_r7r8, list(ESS09 = dat_r9), dat_r10)
names(dat)
map_df(dat, class)
rm(dat_r7r8, dat_r9, dat_r10)


# Save names of the variables
dat.names <- map(dat, names)
map_int(dat.names, length)

# Rounds since R7
# sprintf("%02d", 6:10)
# round.labels <- paste0("R", sprintf("%02d", 6L + seq_along(dat)))
round.labels <- names(dat)
round.labels <- factor(round.labels, round.labels)
print(round.labels)


# Mark which variables are available in each round
variables[, c(as.character(round.labels)) := lapply(
  X = dat.names, FUN = function(x) (variables$varname %in% x))
]

variables[, flag := !all(unlist(.SD)),
          .SDcols = as.character(round.labels),
          by = .(varname)]
variables[, .N, keyby = .(flag)]

variables[(flag)]
#      type  varname ESS07 ESS08 ESS09 ESS10 ESS10SC flag
# 1: Binary   dscrdk  TRUE  TRUE  TRUE  TRUE   FALSE TRUE
# 2: Binary  dscrref  TRUE  TRUE  TRUE  TRUE   FALSE TRUE
# 3: Binary  dvrcdev FALSE FALSE FALSE FALSE   FALSE TRUE
# 4: Binary  lvgptne FALSE FALSE FALSE FALSE   FALSE TRUE
# 5: Binary   rlgblg  TRUE  TRUE  TRUE  TRUE   FALSE TRUE
# 6: Binary  rlgblge  TRUE  TRUE  TRUE  TRUE   FALSE TRUE
# 7: Binary scrlgblg FALSE FALSE FALSE FALSE    TRUE TRUE

variables[, map(.SD, sum), .SDcols = as.character(round.labels)]
#    ESS07 ESS08 ESS09 ESS10 ESS10SC
# 1:    75    75    75    75      72

# Some of the variables are not available in all rounds
# Those variables will be excluded for the ICC estimation


# Variable selection to reduce the size of a data.table

# Survey design variables and weights
varnames.design <- c("name", "essround", "edition", "proddate",
                     "cntry", "idno",
                     "domain", "stratum", "psu", "prob",
                     "dweight", "pspwght", "pweight", "anweight")


# Helper function to subselect necessary variables
foo <- function(dt) {
  name_sel <- intersect(c(varnames.design, variables$varname), names(dt))
  dt[, c(name_sel), with = F]
}

# Keep only necessary variables
dat <- lapply(dat, foo)

# dat <- dat[, c(varnames.design, variables$varname), with = F]
# # dim(dat)
# gc()



# Combine data from all rounds in one data.table
dat <- rbindlist(dat, use.names = T, fill = T)
gc()

setcolorder(dat, intersect(c(varnames.design, variables$varname), names(dat)))
class(dat)
names(dat)


# Remove all extra attributes (from the SPSS data file)
# str(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_labels(dat)
dat <- haven::zap_missing(dat)
dat <- haven::zap_widths(dat)
# str(dat)


# # Create variables which are missing for all rounds
# x <- setdiff(variables$varname, names(dat))
# if (length(x) > 0) dat[, c(x) := as.list(rep(NA_real_, length(x)))]
# x <- setdiff(variables$varname, names(dat))
# if (length(x) > 0) stop("Not all variables available")
# rm(x)

# Check values for the target variables

# Function returns a character of max 20 smalles values from a variable
foo <- function(x) {
  if (x %in% names(dat)) {
    paste(sort(head(unique(dat[[x]]), n = 20)), collapse = ",")
  } else {
    NA_character_
  }
}

# foo("vote")
# foo("asd")

variables[, values := foo(varname), by = varname]

# Write out table about the target variables
write.xlsx(variables, file = "tables/variables.xlsx",
           colWidths = "auto",
           firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(variables, file = "tables/variables.csv", quote = T)


# intersect(names(dat), variables$varname)
# head(names(dat), 10)
# tail(names(dat), 10)



# Check the edition and production dates
dat[, .N, keyby = .(name, essround, edition, proddate)]


# Self-completion
dat[, selfcomp := grepl("SC", name)]
dat[, .N, keyby = .(selfcomp)]
dat[, name := NULL]

# Round
dat[, essround := factor(sprintf(fmt = "R%02d", essround))]
dat[, .N, keyby = .(essround)]

# Production date
dat[, proddate := lubridate::dmy(proddate)]
dat[, .N, keyby = .(proddate)]

dat[, .N, keyby = .(essround, selfcomp, edition, proddate)]
#    essround selfcomp edition   proddate     N
# 1:      R07    FALSE     2.2 2018-12-01 40185
# 2:      R08    FALSE     2.2 2020-12-10 44387
# 3:      R09    FALSE     3.1 2021-02-17 49519
# 4:      R10    FALSE     3.0 2023-05-08 37611
# 5:      R10     TRUE     2.0 2023-05-11 21199

# Number of respondents by country and round
table_cntry_essround <- dcast.data.table(
  data = dat, formula = cntry ~ essround, fun.aggregate = length
)
table_cntry_essround[R10 > 0]
fwrite(x = table_cntry_essround, file = "tables/table_cntry_essround.csv")


# Save data files for the next step
saveRDS(object = dat, file = "data/dat.rds")
saveRDS(object = variables, file = "data/variables.rds")
