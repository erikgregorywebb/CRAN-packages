# https://r-hub.github.io/cranlogs/

library(tidyverse)
library(cranlogs)

# import
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-overview.csv'
cran = read_csv(url)

# define robust function
getDownloads <- function(package_name) {
  out <- tryCatch(
    {
      downloads = cran_downloads(when = "last-month", package = package)
    },
    error=function(cond) {
      message(cond)
      message(paste("ERROR PACKAGE:", package_name))
      beepr::beep(sound = 1)
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      message(paste("WARNING PACKAGE:", url))
      beepr::beep(sound = 2)
      return(NULL)
    },
    finally={
      message(paste("PROCESSED PACKAGE:", package_name))
    }
  )    
  return(out)
}

# collect download log data
log_datalist = list()
for (i in 1:nrow(cran)) {
  Sys.sleep(0.1)
  package = cran$name[i]
  downloads = getDownloads(package)
  if(is.na(page) == F & is.null(page) == F) {
    log_datalist[[i]] = downloads
  }
  print(paste('DONE: ', package, ' (', i, ' of ', nrow(cran), ')', sep = ''))
}
package_logs_raw = do.call(rbind, log_datalist)

# export copy
setwd("~/projects/cran")
write_csv(package_logs_raw, paste('cran-packages-downloads.csv', sep = ''))
