library(sf)
library(tidyverse)

list.files("data/km", full.names = TRUE) %>%
   enframe() %>% 
   filter(str_detect(value, ".shp$")) %>% 
   mutate(data = map(value, st_read)) %>% 
   pwalk(~ assign("vyoh", ..3, .GlobalEnv))

grid_list <- list(
   theme(panel.grid.major = element_line(color = "transparent"),
         axis.text = element_blank()) 
)

#
#muu ----
vyoh

# gdalUtils::ogrinfo("WFS:http://geo.stat.fi/geoserver/tilastointialueet/wfs")
# gdalUtils::ogr2ogr("WFS:http://geo.stat.fi/geoserver/tilastointialueet/wfs", "seutukunnat.shp", "seutukunta1000k_2018")

kunnat <- st_read("data/kunta2018.shp")
seutukunnat <- st_read("seutukunnat.shp")

oulusf <- kunnat %>% 
   filter(nimi == "Oulu")

ouluvyoh <- st_intersection(oulusf, vyoh) %>% 
   select(kunta = nimi, vyohyke = Nimi)

seutuvyoh <- st_intersection(seutukunnat %>% filter(nimi == "Oulu"), vyoh)
ggplot(seutuvyoh) + geom_sf(aes(fill = Nimi))

ggplot(ouluvyoh) + geom_sf(aes(fill = vyohyke))

iouluvyoh <- ouluvyoh %>% mutate_if(is.factor, as.character)

vyohykkeet <- ouluvyoh %>% pull(vyohyke)

isoksi <- function(x) {
   y <- str_sub(x, 1, 1)
   paste0(toupper(y), str_sub(x, 2, nchar(x)))
}

ppha <- osuudet %>% 
   group_by(vyohyke) %>% 
   mutate(osuus = matkaluku / sum(matkaluku)) %>% 
   ungroup() %>% 
   filter(kulkutapa %in% c("pp", "ha")) %>% 
   mutate(vyohyke = isoksi(vyohyke))

ppoulu <- ouluvyoh %>%  left_join(ppha %>% filter(kulkutapa == "pp"))
haoulu <- ouluvyoh %>%  left_join(ppha %>% filter(kulkutapa == "ha"))

p1 <- ppoulu %>% 
   ggplot(aes(fill = osuus)) +
   geom_sf(show.legend = FALSE) +
   coord_sf(expand = FALSE) +
   scale_fill_distiller(direction = 1, labels = function(x) paste(x * 100, "%")) +
   labs(title = "Pyöräilyn osuus") +
   theme_void() +
   grid_list

p2 <- p1 %+% haoulu + labs(title = "Autoilun osuus")

labelit <- ouluvyoh %>% 
   st_centroid %>% 
   st_coordinates() %>% 
   as_tibble() %>% 
   bind_cols(ouluvyoh %>% as_tibble() %>% select(vyohyke)) %>%
   mutate(nudgey = c(-20000, 30000, -25000, 0, 0)) %>% 
   mutate(nudgex = c(0, -4000, 14000, 0, 0)) %>% 
   mutate(vyohnudge_x = c(0, 2500, 6000, 10000, -8000)) %>% 
   mutate(vyohnudge_y = c(0, 0, -2000, -6000, 12000))

pplab <- ppoulu %>% left_join(labelit)
halab <- haoulu %>% left_join(labelit)

p1_lab <- p1 + ggrepel::geom_text_repel(data =  pplab, 
                              aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = paste(round(osuus * 100, 1), "%")), nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)
p2_lab <- p2 + ggrepel::geom_text_repel(data =  halab, 
                                        aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = paste(round(osuus * 100, 1), "%")), nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)

cowplot::plot_grid(p1_lab, p2_lab, nrow = 1)

ggsave("osuudet.png", h = 5, w = 10)

vaesto <- vaes %>% 
   as_tibble() %>% 
   filter(Alue == "Oulu") %>%
   mutate_if(is.factor, as.character) %>% 
   select(-Alue) %>% 
   set_names("vuosi", "ika", "sp", "vyohyke", "pop") %>% 
   filter(vyohyke %in% vyohykkeet) %>% 
   filter(sp == "Sukupuolet yhteensä") %>% 
   select(vuosi, vyohyke, pop) %>% 
   arrange(vuosi) 

# vaes16 <- vaesto %>% filter(vuosi == 2016)
vaes16 <- readRDS("data/vyoh2016.rds")
pa <- ouluvyoh %>% st_area() %>% as.numeric() %>% enframe(value = "pa") %>% mutate(pa = pa / 1000^2)

# p3 <- p1 %+% (ppoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit) %>% bind_cols(pa) %>% mutate(osuus = osuus/pa)) +
p3 <- p1 %+% (ppoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit)) +
   geom_sf(show.legend = FALSE) +
   scale_fill_distiller(direction = 1, labels = scales::number_format(), name = NULL) +
   coord_sf(expand = FALSE) +
   # labs(title = bquote("Pyörämatkoja vuorokaudessa per km"^2)) +
   labs(title = "Pyörämatkoja vuorokaudessa") +
   ggrepel::geom_text_repel(aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = scales::number(osuus, accuracy = 1)), 
                            nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)

# p4 <- p2 %+% (haoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit) %>% bind_cols(pa) %>% mutate(osuus = osuus/pa)) +
p4 <- p2 %+% (haoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit)) +
   geom_sf(show.legend = FALSE) +
   scale_fill_distiller(direction = 1, labels = scales::number_format(), name = NULL) +
   coord_sf(expand = FALSE) +
   # labs(title = bquote("Automatkoja vuorokaudessa per km"^2)) +
   labs(title = "Automatkoja vuorokaudessa") +
   ggrepel::geom_text_repel(aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = scales::number(osuus, accuracy = 1)), 
                            nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)

cowplot::plot_grid(p3, p4, nrow = 1)

ggsave("matkoja.png", h = 5, w = 10)

pa <- ouluvyoh %>% st_area() %>% as.numeric() %>% enframe(value = "pa") %>% mutate(pa = pa / 1000^2)

p5 <- p1 %+% (ppoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit) %>% bind_cols(pa) %>% mutate(osuus = osuus/pa)) +
# p3 <- p1 %+% (ppoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit)) +
   geom_sf(show.legend = FALSE) +
   scale_fill_distiller(direction = 1, labels = scales::number_format(), name = NULL) +
   coord_sf(expand = FALSE) +
   labs(title = bquote("Pyörämatkoja vuorokaudessa per km"^2)) +
   # labs(title = "Pyörämatkoja vuorokaudessa") +
   ggrepel::geom_text_repel(aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = scales::number(osuus, accuracy = 1)), 
                            nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)

p6 <- p2 %+% (haoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit) %>% bind_cols(pa) %>% mutate(osuus = osuus/pa)) +
# p4 <- p2 %+% (haoulu %>% left_join(vaes16) %>% mutate(osuus = pop * matkaluku) %>% left_join(labelit)) +
   geom_sf(show.legend = FALSE) +
   scale_fill_distiller(direction = 1, labels = scales::number_format(), name = NULL) +
   coord_sf(expand = FALSE) +
   labs(title = bquote("Automatkoja vuorokaudessa per km"^2)) +
   # labs(title = "Automatkoja vuorokaudessa") +
   ggrepel::geom_text_repel(aes(X + vyohnudge_x, y = Y + vyohnudge_y, label = scales::number(osuus, accuracy = 1)), 
                            nudge_x = pplab$nudgex, nudge_y = pplab$nudgey)

cowplot::plot_grid(p5, p6, nrow = 1)

ggsave("pinta-ala.png", h = 5, w = 10)
