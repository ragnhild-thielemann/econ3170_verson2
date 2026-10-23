library(tidyverse)
library(xml2)


url <- "https://data.stortinget.no/eksport/sporretimesporsmal?sesjonid=2010-2011"


library(tidyverse)
library(xml2)

url <- "https://data.stortinget.no/eksport/sporretimesporsmal?sesjonid=2010-2011"

data <- read_xml(url)

# Vis XML-strukturen
data

# Vis navnene på rotelementets barn
xml_name(xml_children(data))

# Vis de første elementene med innhold
xml_find_all(data, ".//*") |>
  head(20) |>
  as_list()