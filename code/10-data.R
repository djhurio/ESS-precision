# ESS survey data
#
# Data is downloaded from the ESS Data Wizard
# https://ess-search.nsd.no/CDW/RoundCountry
#
# Data is download since round 7 (2014)
# Data should be downloaded manualy and saved in the "data" folder
#
# For rounds 7-8 integrated and SDDF files should be saved
# For rounds since round 9 only integrated files should be saved
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

variables <- read.xlsx(xlsxFile = "variables/ICC-variables.xlsx")
setDT(variables)

variables <- melt(data = variables,
                  measure.vars = names(variables),
                  na.rm = T)

setnames(x = variables, new = c("type", "varname"))

variables[, varname := tolower(varname)]
variables

# Corrections for the variable list
# https://myess.upf.edu/portal/g/:spaces:sampling/cst_sampling_weighting/ForumPortlet/topic/topicaa8906cc7f00010138aa844b7c289ac3/2
#
# Add variables
x <- data.table(type = "Binary", varname = c("lvgptnea", "dvrcdeva", "scrlgblg"))
variables <- rbind(variables, x)
rm(x)

setorder(variables, type, varname)
variables


# Survey design variables and weights
varnames.design <- c("name", "essround", "edition", "proddate",
                     "cntry", "idno",
                     "domain", "stratum", "psu", "prob",
                     "dweight", "pspwght", "pweight", "anweight")

# ESS data

data_ess_folder <- "~/Documents/data-ess"

file_csv <- list.files(path = data_ess_folder,
                       pattern = "csv$",
                       full.names = T,
                       recursive = T)

dat_csv <- fread(file = file_csv,
                 select = c(varnames.design, variables$varname))
dat_csv

# Self-completion
dat_csv[, selfcomp := grepl("SC", name)]

# Round
# dat_csv[, .N, keyby = .(round = sprintf(fmt = "R%02d", essround))]
dat_csv[, essround := factor(sprintf(fmt = "R%02d", essround))]
dat_csv[, .N, keyby = .(essround)]

# Production date
# dat_csv[, .(proddate, lubridate::dmy(proddate))]
dat_csv[, proddate := lubridate::dmy(proddate)]

dat_csv[, .N, keyby = .(essround, selfcomp, edition, proddate, name)]
dat_csv[, name := NULL]


# Check design variables
dat_csv[, map(.SD, \(x) any(!is.na(x))),
        .SDcols = c("domain", "stratum", "psu", "prob"),
        keyby = .(essround, selfcomp)]
# Design variables are available only since the round 9.
#
#     essround selfcomp domain stratum   psu  prob
#  1:      R01    FALSE  FALSE   FALSE FALSE FALSE
#  2:      R02    FALSE  FALSE   FALSE FALSE FALSE
#  3:      R03    FALSE  FALSE   FALSE FALSE FALSE
#  4:      R04    FALSE  FALSE   FALSE FALSE FALSE
#  5:      R05    FALSE  FALSE   FALSE FALSE FALSE
#  6:      R06    FALSE  FALSE   FALSE FALSE FALSE
#  7:      R07    FALSE  FALSE   FALSE FALSE FALSE
#  8:      R08    FALSE  FALSE   FALSE FALSE FALSE
#  9:      R09    FALSE   TRUE    TRUE  TRUE  TRUE
# 10:      R10    FALSE   TRUE    TRUE  TRUE  TRUE
# 11:      R10     TRUE   TRUE    TRUE  TRUE  TRUE

# Check weight variables
dat_csv[, map(.SD, \(x) any(!is.na(x))),
        .SDcols = c("dweight", "pspwght", "pweight", "anweight"),
        keyby = .(essround, selfcomp)]

stop()

# # delete all sav files
# list.files(path = "data", pattern = ".sav$", full.names = T) |> file.remove()
#
# # unzip all data files
# for (x in list.files(path = "data", pattern = ".zip$", full.names = T)) {
#   cat(x, "\n")
#   unzip(zipfile = x, exdir = "data")
# }
# rm(x)

# SDDF is seperate for rounds 7-8
read.ess <- function(r) {

  # Survey data
  dat_surv <- list.files(
    path = "data", pattern = glue::glue("^ESS{r}e.*sav$"), full.names = T
  ) |> haven::read_sav()
  setDT(dat_surv)

  # SDDF data
  dat_sddf <- list.files(
    path = "data", pattern = glue::glue("^ESS{r}SDDFe.*sav$"), full.names = T
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
  path = "data", pattern = "^ESS9.*sav$", full.names = T
)) |> as.data.table()

# R10
files_r10 <- list.files(
  path = "data", pattern = "^ESS10.*sav$", full.names = T
)
names(files_r10) <- basename(path = files_r10) |>
  sub(pattern = "e.*sav$", replacement = "")
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

variables[, is.available := all(unlist(.SD)),
          .SDcols = as.character(round.labels),
          by = .(varname)]
variables[, .N, keyby = .(is.available)]
variables[!(is.available)]
# Some of the 75 variables are not available in all rounds
# ICC will be assumed to be 0 in rounds when a variable is not available

#      type varname ESS07 ESS08 ESS09 ESS10 ESS10SC is.available
# 1: Binary  rlgblg  TRUE  TRUE  TRUE  TRUE   FALSE        FALSE
# 2: Binary rlgblge  TRUE  TRUE  TRUE  TRUE   FALSE        FALSE
# 3: Binary  dscrdk  TRUE  TRUE  TRUE  TRUE   FALSE        FALSE
# 4: Binary dscrref  TRUE  TRUE  TRUE  TRUE   FALSE        FALSE
# 5: Binary lvgptne FALSE FALSE FALSE FALSE   FALSE        FALSE
# 6: Binary dvrcdev FALSE FALSE FALSE FALSE   FALSE        FALSE

# rlgblg:  Belonging to particular religion or denomination
# rlgblge: Ever belonging to particular religion or denomination
# dscrdk:  Discrimination of respondent's group: don't know
# dscrref: Discrimination of respondent's group: refusal

variables[grep("dscr", varname)]

# lvgptne: Ever lived with a partner, without being married
# dvrcdev: Ever been divorced/had civil union dissolved
grep("lvgptne|dvrcdev", names(dat$ESS10SC), value = T)

# Variable selection to reduce the size of a data.table


# Helper function to subselect necessary variables
foo <- function(x) {
  name_sel <- intersect(names(x), c(varnames.design, variables$varname))
  x[, c(name_sel), with = F]
}

# Keep only necessary variables
dat <- lapply(dat, foo)

# dat <- dat[, c(varnames.design, variables$varname), with = F]
# # dim(dat)
# gc()



# Combine data from all rounds in one data.table
dat <- rbindlist(dat, use.names = T, fill = T)
class(dat)
gc()


# Remove all extra attributes (from the SPSS data file)
# str(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_labels(dat)
dat <- haven::zap_missing(dat)
dat <- haven::zap_widths(dat)
# str(dat)


# Create variables which are missing for all rounds
x <- setdiff(variables$varname, names(dat))
if (length(x) > 0) dat[, c(x) := as.list(rep(NA_real_, length(x)))]
x <- setdiff(variables$varname, names(dat))
if (length(x) > 0) stop("Not all variables available")
rm(x)

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
           colWidths = "auto", firstRow = T,
           headerStyle = createStyle(textDecoration = "italic",
                                     halign = "center"))

fwrite(variables, file = "tables/variables.csv", quote = T)


# intersect(names(dat), variables$varname)
# head(names(dat), 10)
# tail(names(dat), 10)



# Check the edition and production dates
dat[, .N, keyby = .(essround, edition, proddate)]

#    essround edition   proddate     N
# 1:        7     2.2 01.12.2018 40185
# 2:        8     2.2 10.12.2020 44387
# 3:        9     3.1 17.02.2021 49519
# 4:       10     2.0 08.12.2022 33351

# # Test
# dat[, .N]
# dat[, .N, keyby = .(cntry)]




# Number of respondents by country and round
dat[, essround := factor(essround, sort(unique(essround)), round.labels)]
table_cntry_essround <- dcast.data.table(
  data = dat, formula = cntry ~ essround, fun.aggregate = length
)
table_cntry_essround[R10 > 0]
fwrite(x = table_cntry_essround, file = "tables/table_cntry_essround.csv")


# Save data files for the next step
saveRDS(object = dat, file = "data/dat.rds")
saveRDS(object = variables, file = "data/variables.rds")
