?readxl::read_excel

library(tidyverse)

matkat <- readxl::read_excel("C:/Users/pasih_000/Documents/rprojektit/kulkutavatvyoh/data/Oulun_seutu_tulokset.xlsx", "D121", 
                   skip = 11, n_max = 10)

suoritteet <- readxl::read_excel("C:/Users/pasih_000/Documents/rprojektit/kulkutavatvyoh/data/Oulun_seutu_tulokset.xlsx", "D121", 
                             skip = 25, n_max = 10)

matkat <- matkat %>% 
   set_names("vyohyke", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki") %>% 
   filter(!is.na(vyohyke)) %>% 
   mutate_at(vars("ha1", "ha2"), as.numeric) %>% 
   transmute(vyohyke, kk, pp, jk, ha = ha1 + ha2, muu, kaikki)

suoritteet <- suoritteet %>% 
   set_names("vyohyke", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki") %>% 
   filter(!is.na(vyohyke)) %>% 
   mutate_at(vars("ha1", "ha2"), as.numeric) %>% 
   transmute(vyohyke, kk, pp, jk, ha = ha1 + ha2, muu, kaikki)

oulukuva <- matkat %>% 
   mutate(vyohyke = isoksi(vyohyke) %>% as_factor()) %>% 
   gather(kt, osuus, kk:muu) %>% 
   mutate(kt = as_factor(kt)) %>% 
   mutate(osuus = osuus / kaikki) %>% 
   ggplot(aes(vyohyke %>% fct_rev(), osuus, fill = kt %>% fct_rev())) +
   geom_col() +
   coord_flip() +
   geom_hline(yintercept = c(0.25, 0.5, 0.75), lty = 3, color = "gray50") +
   geom_text(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
             position = position_stack(vjust = 0.5), size = 3) +
   scale_fill_brewer(palette = "Set2", name = NULL, labels = rev(c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu"))) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme(legend.position = "top") +
   labs(x = NULL, y = NULL)

leg1 <- cowplot::get_legend(oulukuva)

ggsave("kto_vyohyke.png", h = 4, w = 7)

# tehtailu ----

files <- list.files("data/hlt/", full.names = TRUE)

nimet <- c("vyohyke", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki")

data_files <- map(files, ~ readxl::read_excel(., sheet = "D121", skip = 14, n_max = 10, col_names = FALSE))

data_nest <- map(data_files, as_tibble) %>% 
   map(set_names, nimet) %>%
   set_names(tools::file_path_sans_ext(basename(files))) %>% 
   map(~ mutate(., vyohyke = isoksi(vyohyke))) %>% 
   map(~ filter(., !is.na(kaikki))) %>% 
   map(~ transmute(., vyohyke, kk, pp, jk, ha = ha1 + ha2, muu, kaikki)) %>% 
   enframe() %>% 
   mutate(name = isoksi(name))

kuvafun <- function(data, title) {
   data %>% 
     mutate(vyohyke = as_factor(vyohyke)) %>% 
     gather(kt, osuus, kk:muu) %>% 
     mutate(kt = as_factor(kt)) %>% 
     mutate(osuus = osuus / kaikki) %>% 
     ggplot(aes(vyohyke %>% fct_rev(), osuus, fill = kt %>% fct_rev())) +
     geom_col() +
     coord_flip() +
     geom_hline(yintercept = c(0.25, 0.5, 0.75), lty = 3, color = "gray50") +
     geom_text(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
               position = position_stack(vjust = 0.5), size = 3) +
     scale_fill_brewer(palette = "Set2", name = NULL, labels = rev(c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu"))) +
     scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
     guides(fill = guide_legend(reverse = TRUE)) +
     theme(legend.position = "none") +
     labs(x = NULL, y = NULL, title = title)
}

kuva_nest <- data_nest %>% 
   mutate(kuva = map2(value, name, kuvafun)) 

nestp1 <- data_nest %>% 
   unnest() %>% 
   mutate(name = str_replace(name, "_tulokset", ""),
          name = str_replace(name, "_", " "),
          name = str_replace(name, "ät Hä", "ät-Hä")) %>% 
   mutate(vyohyke = as_factor(vyohyke)) %>% 
   gather(kt, osuus, kk:muu) %>% 
   mutate(kt = as_factor(kt)) %>% 
   mutate(osuus = osuus / kaikki) %>% 
   filter(!str_detect(vyohyke, "oko")) %>% 
   ggplot(aes(vyohyke %>% fct_rev(), osuus, fill = kt %>% fct_rev())) +
   facet_wrap(~ name) +
   geom_col() +
   coord_flip() +
   geom_hline(yintercept = c(0.25, 0.5, 0.75), lty = 3, color = "gray50") +
   # geom_text(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
   #           position = position_stack(vjust = 0.5), size = 3) +
   ggrepel::geom_text_repel(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
             position = position_stack(vjust = 0.5), size = 3, 
             direction = "x", box.padding = 0, point.padding = 0) +
   scale_fill_brewer(palette = "Set2", name = NULL, labels = rev(c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu"))) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme_minimal() +
   theme(legend.position = "top") +
   labs(x = NULL, y = NULL) 

ggsave("nest_kuva.png", h = 10, w = 13)

kuvat <- kuva_nest %>% pull(kuva)

grid1 <- cowplot::plot_grid(plotlist = kuva_nest$kuva)
grid2 <- cowplot::plot_grid(leg1, grid1, ncol = 1, rel_heights = c(1, 10))

ggsave("isokuva.png", h = 15, w = 20)
