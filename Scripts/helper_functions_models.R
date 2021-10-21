plot_es <- function(es, df, contaminant = "ar", main = "") {
  df %>% drop_na(SYSTEM_NO, contains("td"), contains("dy"), year, ZIP)
  se <- es$se %>% as_tibble() %>% mutate(coef = names(es$se))
  x <- as_tibble(es$coefficients) %>% mutate(coef = rownames(es$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  
  if (contaminant == 'ar') {
    baseline_ar <- df %>%
      mutate(year_n = as.integer(as.character(year))) %>%
      filter(year_n == min(year_n))
    
    bs <- tibble(
        year_n = min(baseline_ar$year_n),
        avg_ar = mean(baseline_ar$ar_ugl, na.rm = TRUE)
      )
    
    x <- x %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_ar = ar_ugl + bs$avg_ar) %>% 
      bind_rows(bs) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_ar,
             lwr = avg_ar - 1.96*se)
    # quartz()
    x %>% ggplot(aes(year_n, avg_ar)) +
      geom_point() +
      geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      scale_x_continuous(breaks = 1990:2021) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "cornsilk", color = NA))
  } else if (contaminant == 'n') {
      baseline_ni <- df %>%
        mutate(year_n = as.integer(as.character(year))) %>%
        filter(year_n == min(year_n))
      
      bs <- tibble(
        year_n = min(baseline_ni$year_n),
        avg_n = mean(baseline_ni$n_mgl, na.rm = TRUE)
      )
      
      x <- x %>% filter(str_detect(coef, 'year')) %>%
        mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
               avg_n = n_mgl + bs$avg_n) %>% 
        bind_rows(bs) %>% 
        arrange(year_n) %>% 
        mutate(upr = 1.96*se+avg_n,
               lwr = avg_n - 1.96*se)
      # quartz()
      x %>% ggplot(aes(year_n, avg_n)) +
        geom_point() +
        geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
        # geom_smooth() +
        theme_minimal_hgrid() +
        ggtitle(label = main) +
        scale_x_continuous(breaks = 1984:2021) +
        theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
              plot.background = element_rect(fill = "cornsilk", color = NA))
  }
}
