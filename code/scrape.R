library(tidyverse)
library(rvest)
library(stringr)
library(beepr)

# scrape list of packages
url = 'https://cran.r-project.org/web/packages/available_packages_by_name.html'
page = read_html(url)
packages_raw = tibble(urls = page %>% html_nodes('a') %>% html_attr('href'))

# clean dataframe
packages = packages_raw %>% 
  filter(str_detect(urls, '/web/packages')) %>% 
  mutate(urls = paste('https://cran.r-project.org', substring(urls, 6), sep = ''))

# define robust function
readUrl <- function(url) {
  out <- tryCatch(
    {
      page = read_html(url)
    },
    error=function(cond) {
      message(cond)
      message(paste("ERROR URL:", url))
      beepr::beep(sound = 1)
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      message(paste("WARNING URL:", url))
      beepr::beep(sound = 2)
      return(NULL)
    },
    finally={
      message(paste("PROCESSED URL:", url))
    }
  )    
  return(out)
}

# loop, scrape data
datalist = list()
for (i in 1:nrow(packages)) {
  Sys.sleep(.1)
  url = packages$urls[i]
  page = readUrl(url)
  if(is.na(page) == F & is.null(page) == F) {
    # table
    table = page %>% html_table() %>% first() %>% mutate(package_url = url) %>% remove_rownames()
    # title
    package_title = page %>% html_node('h2') %>% html_text()
    # description
    package_description = page %>% html_node('p') %>% html_text()
    # save
    table = table %>% mutate(package_title = package_title, package_description = package_description)
    datalist[[i]] = table
  }
  print(paste(i, ' of ', nrow(packages), sep = ''))
}
raw = do.call(rbind, datalist)
beepr::beep(sound = 3)

# clean
cran_packages_overview = raw %>% distinct(package_url, package_title, package_description) %>%
  rename(url = package_url, title = package_title, description = package_description) %>%
  mutate(name = gsub("https://cran.r-project.org/web/packages/(.+)/index.html", "\\1", url)) %>%
  select(name, title, description, url)

cran_packages_detail = raw %>% select(variable = X1, value = X2, package_url) %>%
  mutate(name = gsub("https://cran.r-project.org/web/packages/(.+)/index.html", "\\1", package_url)) %>%
  select(name, variable, value)

# export copy
setwd("~/projects/cran")
write_csv(raw, paste('raw-all-packages-', Sys.Date(), '.csv', sep = ''))
write_csv(cran_packages_overview, paste('cran-packages-overview.csv', sep = ''))
write_csv(cran_packages_detail, paste('cran-packages-detail.csv', sep = ''))
