plot_es <- function(es, df, contaminant = "ar", main = "",
                    ylm = c(0,8)) {
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
      geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
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
            plot.background = element_rect(fill = "white", color = NA)) +
      lims(y = ylm)
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
        # geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
        # geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        # geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        # geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
        geom_point() +
        geom_errorbar(aes(ymin = lwr, ymax = upr, width = .1)) +
        # geom_smooth() +
        theme_minimal_hgrid() +
        ggtitle(label = main) +
        # lims(x=c(1984,2022)) +
        scale_x_continuous(breaks = 1984:2022) +
        theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
              plot.background = element_rect(fill = "white", color = NA)) +
        lims(y = ylm)
  }
}

plot_es2 <- function(es, es2, df, df2, contaminant = "ar", main = "",
                    ylm = c(0,8)) {
  # browser()
  se <- es$se %>% as_tibble() %>% mutate(coef = names(es$se))
  x <- as_tibble(es$coefficients) %>% mutate(coef = rownames(es$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)

    baseline_ni <- df %>%
      mutate(year_n = as.integer(as.character(year))) %>%
      filter(year_n == min(year_n))
    
    bs <- tibble(
      year_n = min(baseline_ni$year_n),
      avg_n = mean(baseline_ni$n_mgl, na.rm = TRUE)
    )
    
    se2 <- es2$se %>% as_tibble() %>% mutate(coef = names(es2$se))
    x2 <- as_tibble(es2$coefficients) %>% mutate(coef = rownames(es2$coefficients)) %>%
      left_join(se2) %>% 
      rename(se = value)
    
    baseline_ni <- df %>%
      mutate(year_n = as.integer(as.character(year))) %>%
      filter(year_n == min(year_n))
    baseline_ni2 <- df2 %>%
      mutate(year_n = as.integer(as.character(year))) %>%
      filter(year_n == min(year_n))
    
    bs <- tibble(
      year_n = min(baseline_ni$year_n),
      avg_n = mean(baseline_ni$n_mgl, na.rm = TRUE)
    )
    bs2 <- tibble(
      year_n = min(baseline_ni2$year_n),
      avg_n = mean(baseline_ni2$n_mgl, na.rm = TRUE)
    )
    x2 <- x2 %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_n = n_mgl + bs2$avg_n) %>% 
      bind_rows(bs2) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_n,
             lwr = avg_n - 1.96*se,
             group = 'majority Latino')
    
    x <- x %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_n = n_mgl + bs$avg_n) %>% 
      bind_rows(bs) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_n,
             lwr = avg_n - 1.96*se,
             group = 'non-majority Latino') %>% 
      bind_rows(x2)
    x %>% ggplot(aes(year_n, avg_n)) +
      geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
      # geom_rect(aes(xmin=1999.5,xmax=2003.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
      geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
      # geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
      # geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      # geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      # geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
      geom_point(aes(color = group)) +
      geom_errorbar(aes(ymin = lwr, ymax = upr, width = .06, color = group)) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      # lims(x=c(1984,2022)) +
      scale_x_continuous(breaks = 1984:2022) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA)) +
      lims(y = ylm) +
      scale_color_brewer(palette = 'Set2') +
      theme(legend.position = 'top')
}

plot_reg <- function(mod, main = " ", ylm = c(-.04, .01), contaminant = "ar", nleads = 2, nlags = 5) {
  
  se <- mod$cse %>% as_tibble() %>% mutate(coef = names(mod$cse))
  x <- as_tibble(mod$coefficients) %>% mutate(coef = rownames(mod$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  # 1 S.D PDSI = 2.6
  if (contaminant == 'ar') {
    
    x <- x %>% 
      mutate(year_since = c(-nleads:0, 1:nlags)) %>% 
      mutate(mean_ar = mean_ar,
             upr = (1.96*se+mean_ar),
             lwr = mean_ar-1.96*se)
   
    x %>% ggplot(aes(year_since, mean_ar)) +
      geom_hline(yintercept = 0, color = "red") +
      # pluvial: 1993-1999, 2005-2006,2010-2011
      # drought: 2000-2003, 2007-2009, 2012–2016, 2018 and 2020-2021
      geom_line() +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "lightblue2", alpha = .3) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      labs(x = "\n Years since drought exposure \nPDSI: higher values more precip", subtitle = "Mean arsenic change (ug/L) per unit increase in PDSI\n", y = " ", title = main) +
      scale_x_continuous(breaks = c(-nleads:0, 1:nlags)) +
      scale_y_continuous(n.breaks = 6) +
      lims(y=ylm) +
      theme(axis.text.x = element_text(size = 12, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA))
    
  } else if (contaminant == 'n') {
    x <- x %>% 
      mutate(year_since = c(-nleads:0, 1:nlags)) %>% 
      mutate(mean_ar = mean_n,
             upr = 1.96*se+mean_n,
             lwr = mean_n-1.96*se)
    
    x %>% ggplot(aes(year_since, mean_n)) +
      geom_hline(yintercept = 0, color = "red") +
      # pluvial: 1993-1999, 2005-2006,2010-2011
      # drought: 2000-2003, 2007-2009, 2012–2016, 2018 and 2020-2021
      geom_line() +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "palegreen1", alpha = .3) +
      # geom_smooth() +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      labs(x = "\n Years since drought exposure \nPDSI: higher values more precip", subtitle = "Mean nitrate change (mg/L) per unit increase in PDSI\n", y = " ", title = main) +
      scale_x_continuous(breaks = c(-nleads:0, 1:nlags)) +
      scale_y_continuous(n.breaks = 6) +
      lims(y=ylm) +
      theme(axis.text.x = element_text(size = 12, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA))
  }
}

pollutant = ar; year_start = 2012; year_end = 2020


# This function returns a dataset of either ni or ar that has *at least* subsequent years from year_start to year_end
# it will return the other years of data too... should I make return a strictly balanced panel by also dropping the other years from before year_start and after year_end?

subset_years <- function(year_start, pollutant, year_end, by = 1) {
 years_desired <- seq(year_start, year_end, by = by)
 
 n_years <- seq(year_start, year_end, by = by) %>% length()
 
 pollutant_int <- pollutant %>% 
   filter(year>=year_start) %>% 
   group_by(samplePointID) %>% 
   filter(n()>=n_years, min(year)<= year_start, max(year)>=year_end) %>% 
   arrange(samplePointID, year) %>% 
   mutate(diff_year = lead(year)-year) %>% 
   filter(max(diff_year, na.rm = TRUE)==1, year>=year_start, year <= year_end)

 return(pollutant_int)
  
}

# mod <- mod_ar_lag3

# this function returns the sum of lagged coefficients

sum_lags <- function(mod, nlags = 3, int_terms = c("gw0", 'raw0', "gw0|raw0"), contaminant = "ar") {
 x <- as_tibble(mod$coefficients) %>% mutate(beta = row.names(mod$coefficients))
 vcov <- mod$clustervcv
 # x %>% filter(str_detect(beta, "d$|dlag\\d$"))
 
 # calculating point estimate for cumulative effect for gw-raw
 
 coeff <- sum(x %>% filter(str_detect(beta, "d$|dlag\\d$")) %>% dplyr::select(1))
 
 v <- 0
 cv <- 0
 e <- 1+nlags
 # calculating cov for gw-raw term
 vcov_tmp <- vcov[1:e, 1:e]
 
 se_d <- sqrt(sum(vcov_tmp))
 
 # first interaction term calculates raw surface water
 # second calculates treated groundwater
 # third calculates treated surface water
 
 for (j in int_terms) {
   
   coeff[length(coeff)+1] = sum(x %>% filter(str_detect(beta, j)) %>% dplyr::select(1)) + coeff[1]

   ind <- row.names(vcov) %>% str_detect(j) %>% which()
   vcov_tmp <- vcov[c(1:e, ind) , c(1:e, ind)]

   se_d[length(se_d)+1] = sqrt(sum(vcov_tmp))
 }
  n <- mod$N
  out <- tibble(est = coeff, se = se_d, coeff = c("d", int_terms)) %>% 
    mutate(t_val = est/se,
           pval = 2*pt(-abs(t_val),df=n-1))
  return(out)
}

sum_marginal <- function(mod, nlags = 3, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'), contaminant = "ar") {
  e <- 1+nlags
  df <- as_tibble(mod$coefficients) %>% mutate(beta = row.names(mod$coefficients)) %>% 
    mutate(beta = str_replace(beta, "d$|d(?=:)", "dlag0"))
  vcov <- mod$clustervcv
  row.names(vcov) <- row.names(vcov) %>% str_replace("d$|d(?=:)", "dlag0")
  colnames(vcov) <-  colnames(vcov) %>% str_replace("d$|d(?=:)", "dlag0")
  # x %>% filter(str_detect(beta, "d$|dlag\\d$"))
  
  # calculating point estimate and se for gw-raw over time
  coeff <- df %>% filter(str_detect(beta, "d$|dlag\\d$")) %>%
    mutate(se = mod$cse[1:e], 
           int_terms = " ")
  out <- c()
  for (j in 1:length(int_terms)) {
    # now within each interaction terms we calculate for each lags
    out[[j]] <- map(0:nlags, function(x){
      
      tmp <- df %>% filter(str_detect(beta, paste0("dlag", x))) %>%
        filter(str_detect(beta, paste0("dlag", x, "$")) |
                 str_detect(beta, int_terms[j]))
      ind <- row.names(vcov) %in% tmp$beta %>% which()
      vcov_tmp <- vcov[ind, ind]
        tibble(
          !!paste0('mean_', contaminant) := sum(tmp[,1]),
          se = sqrt(sum(vcov_tmp)),
          beta = paste0('dlag', x),
          int_terms = int_terms[j]
        )
    }) %>% bind_rows()
 
  } 
  n <- mod$N
  out <- bind_rows(coeff, out) %>%
    mutate(t_val = UQ(rlang::sym(paste0('mean_', contaminant))) / se,
           pval = 2 * pt(-abs(t_val), df = n - 1))
  return(out)
  }

plot_coeff <-
  function(df,
           contaminant = 'ar',
           drought_measure = '') {
    if (contaminant == 'ar') {
      yl <- 'Change in mean Arsenic per pws (ug/l)'
      t <-
        paste0('Cumulative effect of ',
               drought_measure,
               '\non annual mean Arsenic level')
    } else {
      yl <- 'Change in mean Nitrate per pws (mg/l)'
      t <-
        paste0('Cumulative effect of ',
               drought_measure,
               '\non annual mean Nitrate level')
    }
    df <-
      df %>% mutate(
        x_ticks = c(
          'Raw groundwater',
          'Raw surface',
          'Treated water'
        )
      )
    ggplot(df, aes(x = x_ticks, y = est)) +
      geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                    width = .1) +
      geom_hline(yintercept = 0, color = 'red') +
      geom_point() +
      theme_minimal_vgrid() +
      ylim(c(-1, 1)) +
      scale_y_continuous(n.breaks = 11) +
      coord_flip() +
      labs(x = ' ', y = yl,
           subtitle = t)
  }

plot_coeff_lags <-
  function(df,
           type = 'raw groundwater',
           contaminant = 'as',
           drought_measure = '',
           ylm = c(-1.6, 0.5)) {
    if (contaminant == 'as') {
      c <- 'cadetblue2'
      t <- paste0('Change in annual mean As (ug/l): \n', type)
    } else {
      c <- 'aquamarine2'
      t <- paste0('Change in annual mean N (mg/l): \n', type)
    }
    df <-df %>% mutate(
      Year = str_extract(beta, "\\d+") %>% as.integer()
        ) %>% 
      rename(est = !!paste0('mean_', contaminant))
    
    ggplot(df, aes(x = Year, y = est)) +
      geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  fill = c, alpha = .3) +
      geom_hline(yintercept = 0, color = 'red') +
      geom_line() +
      # geom_point() +
      theme_minimal_vgrid() +
      ylim(ylm) +
      # coord_flip() +
      labs(x = ' ', y = ' ',
           subtitle = t)
  }

# save_plot("Plots/cumulative_lagged_effects_ar.png", plot_coeff(df), scale = 1,
#           base_asp = 2)
