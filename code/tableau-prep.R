library(tidyverse)
library(tidytext)
library(tidystringdist)

# https://community.rstudio.com/t/identifying-fuzzy-duplicates-from-a-column/35207

### IMPORT

setwd("~/Files/6 - Personal/Projects/cran")

# import package details 
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-detail.csv'
download.file(url, 'cran-packages-detail.csv')
cran = read_csv('cran-packages-detail.csv')
glimpse(cran)

# import package downloads 
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-downloads.csv'
download.file(url, 'cran-packages-downloads.csv')
downloads = read_csv('cran-packages-downloads.csv')
glimpse(downloads)

# aggregate monthly downloads, merge
cran = left_join(
  x = cran,
  y = downloads %>%
    group_by(package) %>% summarise(last_month_downloads = sum(count)),
  by = c('name' = 'package')
)

# export
write_csv(cran, 'cran.csv')
