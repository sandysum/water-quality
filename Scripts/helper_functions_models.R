plot_es <- function(es, df, contaminant = "ar", main = "") {
  df %>% drop_na(SYSTEM_NO, contains("td"), contains("dy"), year, ZIP)
  se <- es$cse %>% as_tibble() %>% mutate(coef = names(es$cse))
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
      # pluvial: 1993-1999, 2005-2006,2010-2011
      # # drought: 2000-2003, 2007-2009, 2012–2016, 2018 and 2020-2021
      # geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      # geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      # geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      geom_point() +
      geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      scale_x_continuous(breaks = 1990:2021) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA))
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
        # geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        # geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        # geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        geom_point() +
        geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
        # geom_smooth() +
        theme_minimal_hgrid() +
        ggtitle(label = main) +
        scale_x_continuous(breaks = 1984:2021) +
        theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
              plot.background = element_rect(fill = "white", color = NA))
  }
}

plot_reg <- function(mod, df, contaminant = "ar", main = "", nleads = 2, nlags = 5) {
  # df %>% drop_na(SYSTEM_NO, contains("td"), contains("dy"), year, ZIP)
  se <- mod$cse %>% as_tibble() %>% mutate(coef = names(mod$cse))
  x <- as_tibble(mod$coefficients) %>% mutate(coef = rownames(mod$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  # 1 S.D PDSI = 2.6
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
      # pluvial: 1993-1999, 2005-2006,2010-2011
      # drought: 2000-2003, 2007-2009, 2012–2016, 2018 and 2020-2021
      geom_point() +
      geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      scale_x_continuous(breaks = 1990:2021) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA))
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
      geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      geom_point() +
      geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      scale_x_continuous(breaks = 1984:2021) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA))
  }
}
