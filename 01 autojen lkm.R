# auton omistus

autonimet <- c("alue", "0", "1", "2", "useampi")

isoksi <- function(x) {
   y <- str_sub(x, 1, 1)
   paste0(toupper(y), str_sub(x, 2, nchar(x)))
}

data_auto <- readxl::read_excel("C:/Users/pasih/Documents/analyysit/kulkutavatvyoh/data/Oulun_seutu_tulokset.xlsx",
                               sheet = "D111", skip = 15, n_max = 9, col_names = FALSE) %>% 
   set_names(autonimet)

data_auto <- data_auto %>% 
   filter(!is.na(alue)) %>% 
   gather(autoja, n, `0`:useampi) %>% 
   mutate(alue = isoksi(alue)) %>% 
   mutate_at(vars(alue, autoja), as_factor)

data_auto %>% 
   filter(!str_detect(alue, "Maaseu")) %>% 
   group_by(alue) %>% 
   mutate(osuus = n / sum(n)) %>%
   add_tally() %>% rename(asuntok = nn) %>% 
   ungroup() %>% 
   mutate(alue = as.character(alue) %>% paste0("\n", scales::number(asuntok), " asuntokuntaa") %>% as_factor()) %>% 
   ggplot(aes(alue %>% fct_rev(), osuus, fill = autoja %>% fct_rev())) +
   geom_col() + 
   coord_flip() + 
   scale_fill_brewer(palette = "BuPu", name = "Autojen lukumäärä", labels = function(x) str_to_title(x)) +
   ggrepel::geom_text_repel(aes(label = format(round(osuus * 100, 1), decimal.mark = ",")),
                            position = position_stack(vjust = 0.5), size = 3,
                            direction = "x", box.padding = 0, point.padding = 0) +
   # geom_text(aes(y = 1.15, label = ave(n, alue, FUN = sum)), check_overlap = TRUE, size = 3) +
   scale_y_continuous(labels = function(x) paste(x * 100, "%"), 
                      #expand = expand_scale(c(0.05, 0.2)), 
                      breaks = seq(0, 1, 0.25)) +
   theme_minimal() +
   theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
   guides(fill = guide_legend(reverse = TRUE)) +
   labs(x = NULL, y = NULL, title = "Oulun seudun asuntokuntien autonomistus")

ggsave("autojen_lkm_asuntokunta_oulu.png", h = 4, w = 8)   
