# Paquets necessaris
library(tidyverse)
library(xlsx)
library(readxl)
library(maps)

#############
### World ###
#############

world <- map_data("world") %>% 
  # Es corregeixen i/o s'actualitzen els noms dels països 
  mutate_at(.vars = vars(region),
            .funs = list(~ case_when(
              str_detect(., pattern = "Antigua") ~ "Antigua and Barbuda",
              str_detect(., pattern = "Barbuda") ~ "Antigua and Barbuda",
              str_detect(., pattern = "Trinidad|Tobago") ~ "Trinidad and Tobago",
              str_detect(., pattern = "UK") ~ "United Kingdom",
              str_detect(., pattern = "USA") ~ "United States",
              str_detect(., pattern = "Serbia|Montenegro") ~ "Serbia and Montenegro",
              TRUE ~ .
            ))) %>% 
  # Es converteixen a classe factor algunes variables 
  mutate_at(.vars = vars(region),
            .funs = list(factor)) %>% 
  select(-subregion)


################
### Capitals ###
################

# Importació de les dades
data("world.cities")

capitals <- world.cities %>% filter(capital == 1) %>% 
  # Es corregeixen i/o s'actualitzen els noms dels països 
  mutate_at(.vars = vars(country.etc),
            .funs = list(~ case_when(
              str_detect(., pattern = "Pitcairn") ~ "Pitcairn Islands",
              str_detect(., pattern = "Kitts") ~ "Saint Kitts",
              str_detect(., pattern = "Congo") ~ "Democratic Republic of the Congo", # assumim que és de la República Democràtica del Congo
              str_detect(., pattern = "Turks") ~ "Turks and Caicos Islands",
              str_detect(., pattern = "Timor") ~ "Timor-Leste",
              str_detect(., pattern = "Vincent") ~ "Saint Vincent",
              str_detect(., pattern = "UK") ~ "United Kingdom",
              str_detect(., pattern = "Korea North") ~ "North Korea",
              str_detect(., pattern = "Guernsey") ~ "Guernsey",
              str_detect(., pattern = "Korea South") ~ "South Korea",
              str_detect(., pattern = "Vatican") ~ "Vatican",
              str_detect(., pattern = "USA") ~ "United States",
              str_detect(., pattern = "Antilles") ~ "Netherlands",
              TRUE ~ .
            ))) %>%  
  select(name, country.etc, lat, long) %>% 
  # Afegim Kosovo
  add_row(name = "Pristina", country.etc = "Kosovo", lat = 42.65, long = 21.17) %>% 
  rename(capital = name,
         country = country.etc) %>% 
  # Es converteixen a classe factor algunes variables
  mutate_at(.vars = vars(capital, 
                         country),
            .funs = list(factor)) %>% 
  # Hi ha 5 països duplicats, així que es selecciona el primer cas de cadascun dels països
  group_by(country) %>% 
  top_n(n = 1)


#############
### Waste ###
#############

# Importació de les dades
waste <- read.csv("../data/waste.csv", header = TRUE, sep=";", check.names = FALSE, dec = ",") %>% 
  tibble(.name_repair = "universal")

# Traducció de les categories de la variable Codi d'eliminació i recuperació
eu_wasteI <- read_excel("../data/codification/codification_eu_waste_trans.xlsx", sheet = "I_disposal_operations")
eu_wasteII <- read_excel("../data/codification/codification_eu_waste_trans.xlsx", sheet = "II_recovery_operations")
eu_waste <- rbind(eu_wasteI, eu_wasteII)

# Traducció de les categories de la variable Conveni general de Basilea
basel_convI.1 <- read_excel("../data/codification/codification_basel_convention_trans.xlsx", sheet = "I_waste_streams")
basel_convI.2 <- read_excel("../data/codification/codification_basel_convention_trans.xlsx", sheet = "I_waste_having_as_constituents")
basel_convII <- read_excel("../data/codification/codification_basel_convention_trans.xlsx", sheet = "II_requiring_special_consid")
basel_conv <- rbind(basel_convI.1, basel_convI.2, basel_convII)


# Funció per a recodificar NAs
replace_factor_na <- function(x){
  x <- as.character(x)
  x <- if_else(is.na(x), "Sense descripció", x)
  x <- as.factor(x)
}


waste <- waste %>%
  # Es corregeixen i/o s'actualitzen els noms dels països 
  mutate_at(.vars = vars(Country.reporting, To.or.from.country),
            .funs = list(~ case_when(
              str_detect(., pattern = "Brunei") ~ "Brunei",
              str_detect(., pattern = "China") ~ "China",
              str_detect(., pattern = "Congo") ~ "Democratic Republic of the Congo",
              str_detect(., pattern = "Côte") ~ "Ivory Coast",
              str_detect(., pattern = "Czechia") ~ "Czech Republic",
              str_detect(., pattern = "Macedonia") ~ "Macedonia",
              str_detect(., pattern = "Antilles") ~ "Netherlands",
              str_detect(., pattern = "Serbia|Montenegro") ~ "Serbia and Montenegro",
              str_detect(., pattern = "Sao Tome And Principe") ~ "Sao Tome and Principe",
              str_detect(., pattern = "World") ~ "Not specified",
              TRUE ~ .
            ))) %>% 
  # Es recodifiquen les categories de la variable d"importacions/exportacions
  mutate_at(.vars = vars(Import.export),
            recode, "import" = "Import", "export" = "Export") %>% 
  # Es converteixen a classe factor algunes variables
  mutate_at(.vars = vars(Import.export, 
                         Country.reporting, 
                         To.or.from.country,
                         Disposal.and.recovery.code,
                         General.Basel.Convention.code..Y.code, 
                         European.List.of.Waste.code,
                         Detailed.Basel.Convention.code.or.OECD.decison.code, 
                         Basis.for.classification.to.hazardous.and.non.hazardous, 
                         UN.hazardous.code.properties.which.render.the.waste.hazardous),
            .funs = list(factor)) %>% 
  # S'arregla i s'assignen etiquetes a la variable Codi d’eliminació i recuperació
  mutate_at(.vars = vars(Disposal.and.recovery.code),
            recode, "D9 " = "D9") %>%
  mutate_at(.vars = vars(Disposal.and.recovery.code),
            .funs = list(~ factor(., levels = eu_waste$code, labels = eu_waste$description))) %>% 
  # S'arregla i s'assignen etiquetes a la variable Conveni general de Basilea
  mutate_at(.vars = vars(General.Basel.Convention.code..Y.code),
            .funs = list(~ factor(., levels = basel_conv$code, labels = basel_conv$description))) %>% 
  # S'afegeix la variable de perillositat
  mutate(hazardous = )
  # Es recodifiquen els NAs que falten
  mutate_if(is.factor, replace_factor_na)


# S'extrauen els noms dels països de world
world.countries <- data.frame(region = unique(world$region))

# S'extrauen els noms dels països de waste
waste.countries <- data.frame(country = union(waste$Country.reporting, waste$To.or.from.country))

# Es miren quins són els noms dels països que estan en waste però no en world
anti_join(x = waste.countries, y = world.countries, by = c("country" = "region"))

# Es miren quins són els noms dels països que estan en capitals però no en world
anti_join(x = capitals, y = world.countries, by = c("country" = "region")) # British Virgin Islands, US Virgin Islands, Tokelau, Gibraltar, Svalbard and Jan Mayen, Tuvalu: no existeixen en world (no són països com a tal)


# Coordenades per país 
world_coord <- left_join(x = waste.countries, y = capitals, by = "country") %>%
  select(-capital) %>%
  # Es converteixen a classe factor algunes variables
  mutate_at(.vars = vars(country),
            .funs = list(factor)) 

# S'assignen les coordenades 0,0 a la categoria "Not specified"
world_coord[world_coord$country == "Not specified",][,c(2, 3)] <- 0
  

# Es guarden els noms dels països en fitxers Excel
# write.xlsx(levels(world$region), file="../data/countries/world_countries.xlsx", row.names = FALSE)
# write.xlsx(levels(waste$Country.reporting), file="../data/countries/waste_countries_reporting.xlsx", row.names = FALSE)
# write.xlsx(levels(waste$To.or.from.country), file="../data/countries/waste_countries_tofrom.xlsx", row.names = FALSE)
# write.xlsx(levels(world_coord$country), file="../data/countries/world_coord_countries.xlsx", row.names = FALSE)


# Es tradueixen de l'anglès al català els noms dels països amb el Google Traductor i llegim de nou els fitxers
world.countries.trans <- read.csv("../data/countries/world_countries_trans.csv", header = TRUE, sep = ";")
levels(world$region) <- world.countries.trans$catalan

waste.countries.reporting.trans <- read.csv("../data/countries/waste_countries_reporting_trans.csv", header = TRUE, sep = ";")
levels(waste$Country.reporting) <- waste.countries.reporting.trans$catalan

waste.countries.tofrom.trans <- read.csv("../data/countries/waste_countries_tofrom_trans.csv", header = TRUE, sep = ";")
levels(waste$To.or.from.country) <- waste.countries.tofrom.trans$catalan

world_coord.trans <- read.csv("../data/countries/world_coord_countries_trans.csv", header = TRUE, sep = ";")
levels(world_coord$country) <- world_coord.trans$catalan


# Es genera un dataset per les importacions
import <- waste %>%
  filter(Import.export == "Import") %>%
  rename(From = To.or.from.country,
         To = Country.reporting,
         Pop_to = Population,
         Quantity.in.tonnes_to = Quantity.in.tonnes,
         Quantity.in.kg.per.capita_to = Quantity.in.kg.per.capita) %>%
  select(Year, From, To, Pop_to, Quantity.in.tonnes_to, Quantity.in.kg.per.capita_to,
         Disposal.and.recovery.code, General.Basel.Convention.code..Y.code) %>%
  left_join(y = world_coord, by = c("From" = "country")) %>%
  rename(start_lat = lat,
         start_long = long) %>% 
  left_join(y = world_coord, by = c("To" = "country")) %>% 
  rename(end_lat = lat,
         end_long = long) %>% 
  relocate(Year, 
           From, start_lat, start_long, 
           To, end_lat, end_long, Pop_to, Quantity.in.tonnes_to, Quantity.in.kg.per.capita_to, 
           Disposal.and.recovery.code, General.Basel.Convention.code..Y.code) %>% 
  left_join(y = basel_conv %>% select(-code), by = c("General.Basel.Convention.code..Y.code" = "description"))

# Es genera un dataset per les exportacions
export <- waste %>%
  filter(Import.export == "Export") %>%
  rename(From = Country.reporting,
         To = To.or.from.country,
         Pop_from = Population,
         Quantity.in.tonnes_from = Quantity.in.tonnes,
         Quantity.in.kg.per.capita_from = Quantity.in.kg.per.capita) %>%
  select(Year, From, To, Pop_from, Quantity.in.tonnes_from, Quantity.in.kg.per.capita_from,
         Disposal.and.recovery.code, General.Basel.Convention.code..Y.code) %>% 
  left_join(y = world_coord, by = c("From" = "country")) %>%
  rename(start_lat = lat,
         start_long = long) %>% 
  left_join(y = world_coord, by = c("To" = "country")) %>% 
  rename(end_lat = lat,
         end_long = long) %>% 
  relocate(Year,
           From, start_lat, start_long, Pop_from, Quantity.in.tonnes_from, Quantity.in.kg.per.capita_from, 
           To, end_lat, end_long, 
           Disposal.and.recovery.code, General.Basel.Convention.code..Y.code) %>% 
  left_join(y = basel_conv %>% select(-code), by = c("General.Basel.Convention.code..Y.code" = "description"))



# Es mira quin dataset conté més dades agrupant per país i any
a1 <- import %>% 
  group_by(Year, From, To) %>% 
  summarise(n = n())

a2 <- export %>% 
  group_by(Year, From, To) %>% 
  summarise(n = n())

# Dades que estan en importacions però no en exportacions
anti_join(x = a1, y = a2, by = c("To" = "From", "From" = "To"))

# Dades que estan en exportacions però no en importacions
anti_join(x = a2, y = a1, by = c("From" = "To", "To" = "From"))


# Es guarden tots els objectes creats en un objecte .RData
save.image("preprocessing.RData")
