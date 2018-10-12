# matkan tarkoitus
library(tidyverse)

nimet <- c("tarkoitus", "kk", "pp", "jk", "ha1", "ha2", "muu", "kaikki")

kt_lab <- c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu")
kt_col <- RColorBrewer::brewer.pal(5, "Set2") %>% set_names(kt_lab)

isoksi <- function(x) {
   y <- str_sub(x, 1, 1)
   paste0(toupper(y), str_sub(x, 2, nchar(x)))
}

data_tar <- readxl::read_excel("C:/Users/pasih/Documents/analyysit/kulkutavatvyoh/data/Oulun_seutu_tulokset.xlsx",
                   sheet = "D183", skip = 12, n_max = 9, col_names = FALSE) %>% 
   as_tibble() %>% 
   select(1:8) %>% 
   set_names(nimet) %>% 
   transmute(tarkoitus, kk, pp, jk, ha = ha1 + ha2, muu, kaikki) %>% 
   mutate(tarkoitus = isoksi(tarkoitus)) %>% 
   gather(kt, matkat, kk:muu) %>% 
   mutate(osuus = matkat / kaikki) %>% 
   group_by(tarkoitus) %>% 
   mutate(jarj = osuus[kt == "pp"]) %>% 
   ungroup() %>% 
   mutate(kt = as_factor(kt) %>% factor(labels = kt_lab))


data_tar %>% 
   ggplot(aes(tarkoitus %>% fct_reorder(jarj), osuus, fill = kt %>% fct_relevel("Pyöräily", after = 0) %>% fct_rev())) +
   geom_col() +
   coord_flip() +
   scale_fill_manual(values = kt_col, name = NULL) +
   theme(legend.position = "top") +
   guides(fill = guide_legend(reverse = TRUE)) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   ggrepel::geom_text_repel(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
                            position = position_stack(vjust = 0.5), size = 3, 
                            direction = "x", box.padding = 0, point.padding = 0) +
   labs(x = NULL, y = NULL, title = "Kulkutapa Oulun seudulla matkan tarkoituksen mukaan")

ggsave("matkan_tarkoitus_oulu.png", h = 4, w = 7)

data_tar %>% 
   filter(!str_detect(tarkoitus, "Kaikki")) %>%
   group_by(kt) %>% 
   mutate(osuus_tar = matkat/sum(matkat)) %>% 
   ggplot(aes(kt %>% fct_rev(), osuus_tar, fill = tarkoitus)) +
   geom_col(position = "stack") +
   coord_flip() +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   scale_fill_brewer(palette = "Set3", name = "Matkan\ntarkoitus") +
   ggrepel::geom_text_repel(aes(label = format(round(osuus_tar * 100, 1), decimal.mark = ",")),
                            position = position_stack(vjust = 0.5), size = 3,
                            direction = "x", box.padding = 0, point.padding = 0) +
   labs(x = NULL, y = NULL) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme(legend.position = "top")

ggsave("matkan_tarkoitus_2_oulu.png", h = 4, w = 8)

# koko seutu ----

files <- list.files("data/hlt/", full.names = TRUE)

data_files <- map(files, readxl::read_excel, sheet = "D183", skip = 12, n_max = 9, col_names = FALSE)

seutu_tar <- map(data_files, as_tibble) %>% 
   map(select, 1:8) %>% 
   map(set_names, nimet) %>%
   set_names(tools::file_path_sans_ext(basename(files))) %>% 
   map(mutate, tarkoitus = isoksi(tarkoitus)) %>% 
   map(transmute, tarkoitus, kk, pp, jk, ha = ha1 + ha2, muu, kaikki) %>% 
   enframe() %>% 
   mutate(name = isoksi(name)) %>% 
   unnest() %>% 
   mutate(name = str_replace(name, "_tulokset", ""),
          name = str_replace(name, "_", " "),
          name = str_replace(name, "ät Hä", "ät-Hä")) %>% 
   mutate_at(vars(name, tarkoitus), as_factor) %>% 
   mutate(tarkoitus = fct_relevel(tarkoitus, "Työ", "Työasia", "Koulu, opiskelu", "Ostos", 
                                  "Asiointi, muu henkilökohtainen", "Saattaminen, kyyditseminen", "Vapaa-aika")) %>% 
   gather(kt, osuus, kk:muu) %>% 
   mutate(kt = as_factor(kt)) %>% 
   filter(!str_detect(tarkoitus, "Kaikki")) %>% 
   group_by(name, tarkoitus) %>% 
   mutate(p_tar = osuus / sum(osuus)) %>% 
   group_by(name, kt) %>% 
   mutate(p_kt = osuus / sum(osuus)) %>% 
   ungroup()

seutu_tar %>%
   # filter(!str_detect(vyohyke, "oko")) %>%
   ggplot(aes(tarkoitus %>% fct_rev(), p_tar, fill = kt %>% fct_rev())) +
   facet_wrap(~ name, ncol = 3) +
   geom_col() +
   coord_flip() +
   geom_hline(yintercept = c(0.25, 0.5, 0.75), lty = 3, color = "gray50") +
   # geom_text(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
   #           position = position_stack(vjust = 0.5), size = 3) +
   ggrepel::geom_text_repel(aes(label = format(round(p_tar * 100, 1), decimal.mark = ",")), 
                            position = position_stack(vjust = 0.5), size = 3, 
                            direction = "x", box.padding = 0, point.padding = 0, segment.colour = "transparent") +
   scale_fill_brewer(palette = "Set2", name = NULL, labels = rev(c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu"))) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme_minimal() +
   theme(legend.position = "top") +
   labs(x = NULL, y = NULL) 

# ggsave("seutu_matkan_tarkoitus.png", h = 11, w = 7)
ggsave("seutu_matkan_tarkoitus.png", h = 7, w = 10)

seutu_tar %>%
   mutate(kt = as_factor(kt) %>% factor(labels = kt_lab)) %>%
   # filter(!str_detect(vyohyke, "oko")) %>%
   ggplot(aes(kt %>% fct_rev(), p_kt, fill = tarkoitus %>% fct_rev())) +
   facet_wrap(~ name, ncol = 3) +
   geom_col() +
   coord_flip() +
   geom_hline(yintercept = c(0.25, 0.5, 0.75), lty = 3, color = "gray50") +
   # geom_text(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")), 
   #           position = position_stack(vjust = 0.5), size = 3) +
   ggrepel::geom_text_repel(aes(label = format(round(p_kt * 100, 1), decimal.mark = ",")), 
                            position = position_stack(vjust = 0.5), size = 2.5, 
                            direction = "x", box.padding = 0, point.padding = 0, segment.colour = "transparent") +
   scale_fill_brewer(palette = "Set3", name = "Matkan\ntarkoitus") +
   # scale_fill_brewer(palette = "Set2", name = NULL, labels = rev(c("Jalankulku", "Pyöräily", "Joukkoliikenne", "Henkilöauto", "Muu"))) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme_minimal() +
   theme(legend.position = "top") +
   labs(x = NULL, y = NULL)

ggsave("seutu_matkan_tarkoitus_2.png", h = 7, w = 10)

# koko seutu pelkkä pyöräily järjestyksessä ----

# värien vilkaisu
# RColorBrewer::brewer.pal(7, "Set3")

tar_jarj <- seutu_tar %>% 
   filter(kt == "pp") %>% 
   select(name, tarkoitus, p_tar) %>% 
   mutate(jarj = p_tar) %>% 
   arrange(jarj) %>% 
   mutate(jarj = as.character(jarj)) %>%
   group_by(tarkoitus) %>% 
   mutate(label_pos = case_when(p_tar < 0.15 * max(p_tar) ~ p_tar + 0.05 * max(p_tar), TRUE ~ p_tar - 0.05 * max(p_tar))) %>% 
   mutate(label_hjust = case_when(p_tar < 0.15 * max(p_tar) ~ 0, TRUE ~ 1)) %>% 
   ungroup() %>% 
   mutate(col = case_when(str_detect(name, "Oulu") ~ "#B3DE69", TRUE ~ "#FDB462")) %>% 
   arrange(tarkoitus, jarj)

tar_jarj %>% 
   # mutate(tarkoitus = as.character(tarkoitus) %>% isoksi() %>% as_factor()) %>% 
   ggplot(aes(jarj, p_tar)) +
   geom_col(show.legend = FALSE, fill = tar_jarj$col) +
   facet_wrap(~ tarkoitus, scales = "free") +
   scale_x_discrete(labels = tar_jarj$name, breaks = tar_jarj$jarj) +
   scale_y_continuous(expand = expand_scale(c(0.01, 0.08))) +
   coord_flip() +
   # scale_fill_brewer(palette = "Set3") +
   # geom_text(aes(y = p_tar * 0.85, label = format(round(p_tar * 100, 1), decimal.mark = ",")), size = 2.5, 
   geom_text(aes(y = label_pos, label = format(round(p_tar * 100, 1), decimal.mark = ","), hjust = label_hjust), size = 2.5, 
             vjust = 0.5, color = "black") +
   theme_minimal() +
   scale_y_continuous(labels = function(x) paste(x * 100, "%")) +
   labs(x = NULL, y = NULL, title = "Pyöräilyn kulkutapaosuus matkan tarkoituksen mukaan") +
   theme(text = element_text(size = 10))

ggsave("matkan_tarkoitus_pp_only.png", h = 6, w = 10)