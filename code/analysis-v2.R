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

### INSIGHT 1
# The top 250 packages (1.5% of ~15,000+ total) account for ~80% of downloads

# aggregate
agg_counts = downloads %>%
  group_by(package) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  mutate(percent_of_total = total_count / sum(total_count)) %>%
  mutate(cum_percent = cumsum(percent_of_total)) %>%
  mutate(row = row_number())

# plot
agg_counts %>%
  filter(row <= 1000) %>%
  ggplot(., aes(x = row, y = cum_percent)) +
  geom_line(size = 2)

agg_counts %>% filter(cum_percent <= .8) %>% tail()
agg_counts %>% filter(row <= 250) %>% tail()
250/nrow(agg_counts) * 100

top_250 = agg_counts %>% filter(row <= 250) %>% pull(package)

### INSIGHT 2
# clean
authors = cran %>% 
  filter(variable == 'Author:') %>%
  mutate(value = gsub("\\[.*?\\]", "", value)) %>% # remove brackets
  mutate(value = gsub("\\(.*\\)", "", value)) %>% # remove parenthesis
  mutate(value = gsub("[\r\n]", "", value)) %>% # remove line breaks
  mutate(value = str_replace(value, 'with contributions from', ',')) %>%
  mutate(value = str_replace(value, 'with contributions by', ',')) %>%
  separate_rows(value, sep = ',') %>% #unnest
  separate_rows(value, sep = '&') %>% #unnest
  separate_rows(value, sep = ' and ') %>% #unnest
  mutate(value = str_trim(value)) %>% 
  filter(value != '') %>%
  rename(package = name, author = value) %>% select(-variable)
glimpse(authors)

# detect duplicates
top_authors = authors %>% filter(package %in% top_250)

match <- top_authors %>% 
  tidy_comb_all(author) %>% 
  tidy_stringdist() %>% 
  filter(soundex == 0) %>% # Set a threshold
  gather(x, match, starts_with("V")) %>% 
  distinct(match) %>%
  .$match

top_authors %>% filter(author %in% match) %>% arrange(author)

### INSIGHT 3
cran %>% group_by(variable) %>% count(sort = T) %>% View()
cran %>% filter(grepl('view', variable)) %>% group_by(value) %>% count(sort = T)

### INSIGHT 4
cran %>% 
  filter(variable == 'Published:' | variable == 'Version:') %>%
  spread(variable, value) %>% rename(published = `Published:`, version = `Version:`) %>%
  mutate(simple_version = str_sub(version, start = 1, end = 3)) %>%
  separate(simple_version, into = c('major', 'minor')) %>%
  filter(is.na(major) == F & is.na(minor) == F) %>%
  group_by(major, minor) %>% count() %>%
  rename(count = n) %>% ungroup() %>%
  mutate(percent = count/sum(count)) %>% select(-count) %>%
  spread(minor, percent, fill = 0) %>%
  mutate(major = as.numeric(major)) %>% arrange(major)
