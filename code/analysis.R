# https://kbroman.org/pkg_primer/pages/depends.html

library(tidyverse)
library(tidytext)
library(visNetwork)

# import package details 
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-detail.csv'
cran = read_csv(url)
glimpse(cran)

# import package downloads 
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-downloads.csv'
downloads = read_csv(url)
glimpse(downloads)

# aggregate monthly downloads, merge
cran = left_join(
  x = cran,
  y = downloads %>%
    group_by(package) %>% summarise(last_month_downloads = sum(count)),
  by = c('name' = 'package')
)

# define cutoff
top_packages = cran %>%
  filter(last_month_downloads >= quantile(cran$last_month_downloads, .995)) %>%
  distinct(name) %>% pull(name)

# define edges
edges = cran %>%
  filter(variable == 'Depends:') %>%
  select(name, value) %>%
  unnest_tokens(output = word, input = value, token = 'words') %>%
  filter(word != 'r') %>%
  filter(!str_detect(word, '^[0-9]')) %>%
  rename(from = name, to = word) %>%
  filter(from %in% top_packages | to %in% top_packages)

# define nodes
nodes = bind_rows(edges %>% select(id = from), 
                  edges %>% select(id = to)) %>% distinct(id) %>%
  mutate(label = id)

# build visual
visNetwork(nodes = nodes, edges = edges)
