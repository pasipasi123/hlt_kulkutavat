# autoilun päästöt, vyöhyke

library(tidyverse)
library(sf)

isoksi <- function(x) {
   y <- str_sub(x, 1, 1)
   paste0(toupper(y), str_sub(x, 2, nchar(x)))
}

vyoh <- list.files("data/km", full.names = TRUE) %>%
   str_subset("Luokitus.shp$") %>% 
   map(st_read) %>% 
   st_set_crs(3067)

seutukunnat <- st_read("seutukunnat.shp")

ouluseutu <- seutukunnat %>% 
   filter(nimi == "Oulu")

nimet <- c("alue", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki")

vaes16 <- readRDS("data/vyoh2016.rds") %>% mutate(vyohyke = fct_collapse(vyohyke, "Koko seutu" = "KOKO VÄESTÖ"))
paasdata <- readxl::read_excel("C:/Users/pasih/Documents/analyysit/kulkutavatvyoh/data/hlt/Oulun seutu.xlsx", 
                   sheet = "D121", skip = 28, n_max = 9, col_names = FALSE)

aluepaasto <- paasdata %>% 
   set_names(nimet) %>% 
   filter(!is.na(ha1)) %>% 
   mutate(alue = isoksi(alue)) %>% 
   select(alue, ha1) %>% 
   mutate(paasto = ha1 * 0.170 * 365) %>% 
   left_join(vaes16, by = c("alue" = "vyohyke")) %>% 
   mutate(paasto_pop = paasto*pop/1000/1000) %>% 
   filter(!is.na(pop))

vyoh %>% 
   st_intersection(ouluseutu) %>% 
   left_join(aluepaasto, by = c("Nimi" = "alue")) %>% 
   st_write("data/paastot_oulu.shp")
   
