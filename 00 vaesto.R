install.packages("pxweb")

library(pxweb)

d <- interactive_pxweb(api = "pxnet2.stat.fi")

ouluvyoh <- get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/vaerak/statfin_vaerak_pxt_023.px",
                  dims = list(Alue = c('564'),
                              "Kaupunki-maaseutu-luokitus" = c('*'),
                              Sukupuoli = c('S', '1', '2'),
                              Ikä = c('*'),
                              Vuosi = c('*'),
                              Tiedot = c('*')),
                  clean = TRUE)

ouluvyoh <- ouluvyoh %>% 
   as_tibble() %>% 
   set_names("alue", "vyohyke", "tieto", "vuosi", "ika", "sp", "n")

saveRDS(ouluvyoh, "data/ouluvyoh.rds")

vyoh2016 <- ouluvyoh %>% 
   set_names("alue", "vyohyke", "tieto", "vuosi", "ika", "sp", "n") %>% 
   select(-alue) %>% 
   filter(vuosi == 2016) %>% 
   filter(str_detect(sp, "yhteensä")) %>% 
   filter(!ika %in% (0:5 %>% as.character()), !str_detect(ika, "yhteensä")) %>% 
   group_by(vyohyke) %>% 
   summarise(pop = sum(n))

saveRDS(vyoh2016, "data/vyoh2016.rds")      