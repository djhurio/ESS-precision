# Install packages

# pak
if (!"pak" %in% installed.packages()[, "Package"]) {
  install.packages("pak")
}

pak::pkg_install(
  pkg = c(
    "purrr",
    "data.table",
    "openxlsx",
    "openxlsx2",
    "haven",
    "ggplot2",
    "lubridate",
    "ICC"
  )
)
