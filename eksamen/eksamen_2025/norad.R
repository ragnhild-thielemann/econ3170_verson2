library(tidyverse)
library(rnaturalearth)
library(countrycode)
library(sf)
#a)
norad <- read_csv("eksamen/eksamen_2025/econ3170_4170_191225_norad.csv (2).csv")
rows <- nrow(norad)
sprintf("Datasettet inneholder %s rader",rows)
antall_prosjekter <- length(unique(norad$agreement_number))
sprintf("Det er %s unike prosjekter",antall_prosjekter)


antall_per_land <- norad |>
  summarise(year = mean(as.numeric(year)),
            .by = c(recipient_country,agreement_number))|>
  summarise(antall = n(), .by = recipient_country)|>
  arrange(desc(antall))

#b)

norad_ed <- norad |>
  filter(disbursements_1000_nok>=0)

a <- nrow(norad) - nrow(norad_ed)
print(paste("Det er",a,"negative verdier"))

#c)

quant <- norad |>
  select(recipient_country,agreement_number,disbursements_1000_nok,year)|>
  summarise(q50 = median(disbursements_1000_nok),
            q25 = quantile(disbursements_1000_nok,0.25),
            q75 = quantile(disbursements_1000_nok,0.75),
            .by = c(year))|>
  pivot_longer(cols = -year,
               names_prefix = "q",
               names_to = "quantile",
               values_to = "value")


p1 <- quant |>
  ggplot(aes(x = year,y = value, color = quantile) )+
           geom_line()

p1

#d)

regional <- norad |>
  mutate(r = if_else(str_to_lower(word(recipient_country,-1))=="regional",1,0))|>
  mutate(iso = countrycode(recipient_country,
                           "country.name",
                           "iso3c"))|>
  filter(year == 2024)|>
  filter(r == 0)|>
  summarise(total = sum(disbursements_1000_nok),
            .by = iso)

nrow(regional)

#f) 

kart <- world_map |>
  select(iso_a3,continent)|>
  left_join(regional,by = join_by (iso_a3 == iso))|>
  filter(continent=="Africa")



ggplot(kart) +
  geom_sf(aes(fill = total)) + 
  theme_void()


#f) 
yapstile <- norad |>
  mutate(r = if_else(str_to_lower(word(recipient_country,-1))=="regional",1,0))|>
  filter(r == 0)|>
  mutate(iso = countrycode(recipient_country,
                           "country.name",
                           "iso3c"))|>
  filter(year == 2024)|>
  mutate(year = as.numeric(year))|>
  summarise(koef = summary(lm(disbursements_1000_nok ~ year))$coef[,1], .by = recipient_country)


