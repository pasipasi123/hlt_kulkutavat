library(tidyverse)

isoksi <- function(x) {
   y <- str_sub(x, 1, 1)
   paste0(toupper(y), str_sub(x, 2, nchar(x)))
}

# tehtailu ----

files <- list.files("data/hlt/", full.names = TRUE)

nimet <- c("vyohyke", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki")

data_files <- map(files, readxl::read_excel, sheet = "D121", skip = 14, n_max = 10, col_names = FALSE)

hlt_seutu <- map(data_files, as_tibble) %>% 
   map(set_names, nimet) %>%
   set_names(tools::file_path_sans_ext(basename(files))) %>% 
   map(mutate, vyohyke = isoksi(vyohyke)) %>% 
   map(filter, !is.na(kaikki)) %>% 
   map(transmute, vyohyke, kk, pp, jk, ha = ha1 + ha2, muu, kaikki) %>% 
   enframe() %>% 
   mutate(name = isoksi(name)) %>% 
   unnest() %>% 
   mutate(name = str_replace(name, "_tulokset", ""),
          name = str_replace(name, "_", " "),
          name = str_replace(name, "ät Hä", "ät-Hä")) %>% 
   mutate(vyohyke = as_factor(vyohyke)) %>% 
   mutate(vyohyke = fct_relevel(vyohyke, "Koko seutu", after = 7)) %>% 
   gather(kt, osuus, kk:muu) %>% 
   mutate(kt = as_factor(kt)) %>% 
   mutate(osuus = osuus / kaikki)


nestp1 <- hlt_seutu %>%
   # filter(!str_detect(vyohyke, "oko")) %>%
   ggplot(aes(vyohyke %>% fct_rev(), osuus, fill = kt %>% fct_rev())) +
   facet_wrap(~ name, ncol = 2, scales = "free_y") +
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

ggsave("nest_kuva.png", h = 11, w = 7)

# seutu, pelkkä pyöräily ----

jarj_d <- hlt_seutu %>% 
   # unnest() %>% 
   # mutate(name = str_replace(name, "_tulokset", ""),
   #        name = str_replace(name, "_", " "),
   #        name = str_replace(name, "ät Hä", "ät-Hä")) %>% 
   # mutate(vyohyke = as_factor(vyohyke)) %>% 
   # gather(kt, osuus, kk:muu) %>% 
   # mutate(kt = as_factor(kt)) %>% 
   # mutate(osuus = osuus / kaikki) %>% 
   filter(!str_detect(vyohyke, "oko")) %>%
   filter(kt == "pp") %>% 
   # complete(name, vyohyke, fill = list(osuus = 0)) %>%
   mutate(x_lab = osuus) %>% 
   filter(!is.na(x_lab)) %>% 
   arrange(vyohyke, x_lab) %>% 
   mutate(x_lab = as.character(x_lab)) %>% 
   filter(!str_detect(vyohyke, "oko")) %>% 
   group_by(name) %>% 
   mutate(label_pos = case_when(osuus < 0.15 * max(osuus) ~ osuus + 0.05 * max(osuus), TRUE ~ osuus - 0.05 * max(osuus))) %>% 
   mutate(label_hjust = case_when(osuus < 0.15 * max(osuus) ~ 0, TRUE ~ 1)) %>% 
   ungroup() %>% 
   mutate(col = case_when(str_detect(name, "Oulu") ~ "#66C2A5", TRUE ~ "#E78AC3"))

jarj_d %>% 
   ggplot(aes(x_lab, osuus, fill = "heh")) +
   facet_wrap(~ vyohyke, ncol = 3, scales = "free") +
   scale_x_discrete(labels = jarj_d$name, breaks = jarj_d$x_lab) +
   geom_col(show.legend = FALSE, fill = jarj_d$col) +
   coord_flip() +
   scale_fill_manual(values = pp_vari) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%"), expand = expand_scale(c(0.05, 0.1))) +
   # geom_text(aes(y = osuus + 0.02, label = format(round(osuus * 100, 1), decimal.mark = ",")), size = 2.5) +
   geom_text(aes(y = label_pos, label = format(round(osuus * 100, 1), decimal.mark = ","), hjust = label_hjust), size = 2.5, 
             vjust = 0.5, color = "black") +
   theme_minimal() +
   theme(panel.background = element_rect(color = "gray90", fill = "white"),
         panel.grid.major.y = element_blank(),
         text = element_text(size = 10)) +
   labs(x = NULL, y = NULL, title = "Pyöräilyn kulkutapaosuus kaupunki–maaseutu-alueen mukaan") 

ggsave("km-alue_pp_only.png", h = 6, w = 10)

