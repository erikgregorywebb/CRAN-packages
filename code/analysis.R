# https://kbroman.org/pkg_primer/pages/depends.html

library(tidyverse)
library(tidytext)
library(visNetwork)

# import
url = 'https://raw.githubusercontent.com/erikgregorywebb/CRAN-packages/master/data/cran-packages-detail.csv'
cran = read_csv(url)
glimpse(cran)

# define edges
edges = cran %>%
  filter(variable == 'Depends:') %>%
  select(name, value) %>%
  unnest_tokens(output = word, input = value, token = 'words') %>%
  filter(word != 'r') %>%
  filter(!str_detect(word, '^[0-9]')) %>%
  rename(from = name, to = word)

# define nodes
nodes = bind_rows(edges %>% select(package = from), edges %>% select(package = to)) %>% distinct(package)

# build visual
visNetwork(nodes = nodes, edges = edges)
