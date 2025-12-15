# Options
options(max.print = 10e3)

# Repository
options(
  repos = c(
    CRAN = sprintf(
      "https://p3m.dev/cran/latest/bin/linux/noble-%s/%s",
      R.version["arch"],
      substr(getRversion(), 1, 3)
    )
  )
)

# Packages
require(purrr)
require(data.table)
require(openxlsx)
require(openxlsx2)
require(haven)
require(ggplot2)
