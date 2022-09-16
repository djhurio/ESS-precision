# ESS survey data
#
# Data is downloaded from the ESS Data Portal
# https://ess-search.nsd.no/
#
# Data is download since round 7 (2014)
# Data should be downloaded manualy and saved in the "data" folder
#
# For rounds 7-8 integrated and SDDF files should be saved
# For rounds since round 9 only integrated files should be saved
#
# All data is saved in SAV (SPSS format as ZIP files

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

variables$varname
if (length(variables$varname) != 75L) stop("Check ICC variables")


# ESS data

# delete all sav files
list.files(path = "data", pattern = ".sav$", full.names = T) |> file.remove()

# unzip all data files
for (x in list.files(path = "data", pattern = ".zip$", full.names = T)) {
  cat(x, "\n")
  unzip(zipfile = x, exdir = "data")
}
rm(x)

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

dat_r7r8 <- lapply(7:8, read.ess)
# lapply(dat_r7r8, class)

# Data since round 9 contains SDDF variables
dat_r9pl <- list.files(
  path = "data", pattern = "^ESS[19].*sav$", full.names = T
) |> lapply(FUN = haven::read_sav) |> lapply(FUN = as.data.table)
# lapply(dat_r9pl, class)

# bind and remove
dat <- c(dat_r7r8, dat_r9pl)
rm(dat_r7r8, dat_r9pl)
# lapply(dat, class)


# Save names of the variables
dat.names <- lapply(dat, names)
sapply(dat.names, length)

# Rounds since R7
# sprintf("%02d", 6:10)
round.labels <- paste0("R", sprintf("%02d", 6L + seq_along(dat)))
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



# Variable selection to reduce the size of a data.table

# Survey design variables and weights
varnames.design <- c("essround", "edition", "proddate",
                     "cntry", "idno",
                     "dweight", "pspwght", "pweight", "anweight",
                     "domain", "prob", "stratum", "psu")


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

# # Test
# dat[, .N]
# dat[, .N, keyby = .(cntry)]


# Production date
# str(dat$proddate)
dat[, proddate := lubridate::dmy(proddate)]
dat[, .N, keyby = .(proddate)]


# Number of respondents by country and round
dat[, essround := factor(essround, sort(unique(essround)), round.labels)]
table_cntry_essround <- dcast.data.table(
  data = dat, formula = cntry ~ essround, fun.aggregate = length
)
fwrite(x = table_cntry_essround, file = "tables/table_cntry_essround.csv")

# Save data files for the next step
saveRDS(object = dat, file = "data/dat.rds")
saveRDS(object = variables, file = "data/variables.rds")
