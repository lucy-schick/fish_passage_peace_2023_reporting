pkgs_cran <- c(
  'rmarkdown',
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'kableExtra',
  'httr',
  'RPostgres',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'rayshader',
  'exifr',
  'english',
  'leaflet.extras',
  'ggdark',
  'pdftools',
  'pagedown',
  'crosstalk',
  'DT',
  'fishbc',
  'chron',
  'remotes',
  'roxygen2',
  'devtools'
)
pkgs_gh <- c(#'Envirometrix/plotKML',  #plot kml needed to go ahead of other packages for some reason and wants to reinstall everytime.... not sure why. hash out for nowpoissonconsulting/fwapgr",
             'poissonconsulting/poisspatial',
             'poissonconsulting/fwapgr',
             "newgraphenvironment/fpr"
             )

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
lapply(pkgs_all,
       pak::pkg_install,
       ask = FALSE)

# load all the packages

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

