# load the necessary packages at the start of sourcing the script
library(broom)
library(lfe)
library(dotwhisker)



plot.means <- function(var.name, df, by.var) {
  
  summary.c <- df %>% group_by(!!rlang::sym(by.var)) %>% 
    summarise(mean.c = mean(!!rlang::sym(var.name), na.rm = TRUE), 
              se.c = var(!!rlang::sym(var.name), na.rm = TRUE) %>% sqrt())
  
  ggplot(summary.c, aes(x=!!rlang::sym(by.var), y=mean.c)) + 
    geom_errorbar(aes(ymin=mean.c-se.c, ymax=mean.c+se.c), width=.1) +
    geom_line() +
    geom_point() +
    theme_bw() +
    # scale_x_continuous(1:12) +
    # scale_x_continuous(breaks = seq(2000, 2014, 2)) +
    ylab("Mean conc. (ug/m3)") +
    xlab(str_to_sentence(by.var)) +
    labs(title = var.name %>% str_extract(".+(?=(_conc))"))
}

add_drought <- function(p) {p + 
  geom_rect(aes(xmin=2006.5,xmax=2009.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
  geom_rect(aes(xmin=1999.5,xmax=2002.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
  geom_rect(aes(xmin=2011.5,xmax=2016.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")+
  geom_rect(aes(xmin=2019.5,xmax=2021.5,ymin=-Inf,ymax=Inf),alpha = .005,fill="indianred1")
  # geom_rect(aes(xmin=2017.5,xmax=2018.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="indianred1")+
  # geom_rect(aes(xmin=1995.5,xmax=1999.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
  # geom_rect(aes(xmin=2004.5,xmax=2006.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
  # geom_rect(aes(xmin=2009.5,xmax=2011.5,ymin=-Inf,ymax=Inf),alpha = .01,fill="skyblue3")+
}

plot_es_delivered <- function(df, pollutant = 'mean_n', by = 'b_majority_latino', years = 2005:2020,
                              ylm = c(1.2,4), main = ' ',
                              ylab = 'Mean N conc. (mg/l)\n') {
  
  form = paste0(pollutant, "~ factor(year) | SYSTEM_NO | 0 | 0")
  
  es <- felm(as.formula(form), data = df %>% filter((!!rlang::sym(by))==1, year %in% years))
  es2 <- felm(as.formula(form), data = df %>% filter((!!rlang::sym(by))==0, year %in% years))
  
  # browser()
  se <- es$se %>% as_tibble() %>% mutate(coef = names(es$se))
  x <- as_tibble(es$coefficients) %>% mutate(coef = rownames(es$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  
  bs <- tibble(
    year_n = min(years),
    avg = mean(getfe(es)[['effect']])
  )
  
  se2 <- es2$se %>% as_tibble() %>% mutate(coef = names(es2$se))
  x2 <- as_tibble(es2$coefficients) %>% mutate(coef = rownames(es2$coefficients)) %>%
    left_join(se2) %>% 
    rename(se = value)

  bs2 <- tibble(
    year_n = min(years),
    avg = mean(getfe(es2)[['effect']])
  )
  
  x2 <- x2 %>% filter(str_detect(coef, 'year')) %>%
    mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
           avg = !!rlang::sym(pollutant) + bs2$avg) %>% 
    bind_rows(bs2) %>% 
    arrange(year_n) %>% 
    mutate(upr = 1.96*se+avg,
           lwr = avg - 1.96*se,
           group = 'All other')
  
  x <- x %>% filter(str_detect(coef, 'year')) %>%
    mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
           avg = !!rlang::sym(pollutant) + bs$avg) %>% 
    bind_rows(bs) %>% 
    arrange(year_n) %>% 
    mutate(upr = 1.96*se+avg,
           lwr = avg - 1.96*se,
           group = 'Majority Latino') %>% 
    bind_rows(x2)
  
  x %>% 
    filter(year_n > min(years)) %>% 
    ggplot(aes(year_n, avg)) +
    geom_line(aes(color = group), size = 1) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, group = group), alpha = .4, fill = 'lightgrey') +
    theme_minimal_hgrid() +
    ggtitle(label = main) +
    lims(y = ylm, x = c(min(years), max(years))) +
    labs(x = '\nYear', y = ylab) +
    scale_x_continuous(breaks = years) +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
          plot.background = element_rect(fill = "white", color = NA)) +
    scale_colour_manual(" ", values=c("darkgreen", "black"),
                        breaks=c('Majority Latino', "All other"), 
                        labels=c(by, "All other")) +
    theme(legend.position = 'top')
  
}

plot_es <- function(es, df, contaminant = "ar", main = "",
                    ylm = c(0,8)) {
  df %>% drop_na(SYSTEM_NO, contains("td"), contains("dy"), year, ZIP)
  years = df$year %>% unique() %>% as.character() %>% as.numeric()
  se <- es$se %>% as_tibble() %>% mutate(coef = names(es$se))
  x <- as_tibble(es$coefficients) %>% mutate(coef = rownames(es$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  # browser()
  if (contaminant == 'ar') {
    
    bs <- tibble(
      year_n = min(years),
      avg_n = mean(getfe(es)[['effect']])
    )
    
    x <- x %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_ar = ar_ugl + bs$avg_ar) %>% 
      bind_rows(bs) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_ar,
             lwr = avg_ar - 1.96*se)
    # quartz()
    x %>% 
      filter(year_n > min(years)) %>% 
      ggplot(aes(year_n, avg_ar)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, group = group), alpha = .4, fill = 'lightgrey') +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      lims(y = ylm, x = c(min(years), max(years))) +
      labs(x = '\nYear') +
      scale_x_continuous(breaks = years) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA)) +
      # scale_colour_manual(" ", values=c("darkgreen", "black"),
      #                     breaks=c('Majority Latino', "All other"), 
      #                     labels=c(by, "All other")) +
      theme(legend.position = 'top')
  } else if (contaminant == 'n') {
   
      
      bs <- tibble(
        year_n = min(years),
        avg_n = mean(getfe(es)[['effect']])
      )
      
      x <- x %>% filter(str_detect(coef, 'year')) %>%
        mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
               avg_n = n_mgl + bs$avg_n) %>% 
        bind_rows(bs) %>% 
        arrange(year_n) %>% 
        mutate(upr = 1.96*se+avg_n,
               lwr = avg_n - 1.96*se)
      # quartz()
      x %>% 
        filter(year_n > min(years)) %>% 
        ggplot(aes(year_n, avg_n)) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .4, fill = 'lightgrey') +
        theme_minimal_hgrid() +
        ggtitle(label = main) +
        labs(x = '\nYear') +
        scale_x_continuous(breaks = years) +
        theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
              plot.background = element_rect(fill = "white", color = NA)) +
        # scale_colour_manual(" ", values=c("darkgreen", "black"),
        #                     breaks=c('Majority Latino', "All other"), 
        #                     labels=c(by, "All other")) +
        theme(legend.position = 'top') + lims(y = ylm)
  }
}

plot_es2 <- function(es, es2, df, df2, contaminant = "ar", main = "", ylab = ' ', 
                    ylm = c(0,8)) {
  years = df$year %>% unique() %>% as.character() %>% as.numeric()
  # browser()
  se <- es$se %>% as_tibble() %>% mutate(coef = names(es$se))
  x <- as_tibble(es$coefficients) %>% mutate(coef = rownames(es$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
    
    bs <- tibble(
      year_n = min(years),
      avg_n = mean(getfe(es)[['effect']])
    )
    
    se2 <- es2$se %>% as_tibble() %>% mutate(coef = names(es2$se))
    x2 <- as_tibble(es2$coefficients) %>% mutate(coef = rownames(es2$coefficients)) %>%
      left_join(se2) %>% 
      rename(se = value)
    
    bs <- tibble(
      year_n = min(years),
      avg_n = mean(getfe(es)[['effect']])
    )
    bs2 <- tibble(
      year_n = min(years),
      avg_n = mean(getfe(es2)[['effect']])
    )
    x2 <- x2 %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_n = n_mgl + bs2$avg_n) %>% 
      bind_rows(bs2) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_n,
             lwr = avg_n - 1.96*se,
             group = 'Majority Latino')
    
    x <- x %>% filter(str_detect(coef, 'year')) %>%
      mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
             avg_n = n_mgl + bs$avg_n) %>% 
      bind_rows(bs) %>% 
      arrange(year_n) %>% 
      mutate(upr = 1.96*se+avg_n,
             lwr = avg_n - 1.96*se,
             group = 'All other') %>% 
      bind_rows(x2)
    
    x %>% 
      filter(year_n > min(years)) %>% 
      ggplot(aes(year_n, avg_n)) +
      geom_line(aes(color = group), size = 1) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, group = group), alpha = .4, fill = 'lightgrey') +
      theme_minimal_hgrid() +
      ggtitle(label = main) +
      lims(y = ylm, x = c(min(years), max(years))) +
      labs(x = '\nYear', y = ylab) +
      scale_x_continuous(breaks = years) +
      theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
            plot.background = element_rect(fill = "white", color = NA)) +
      scale_colour_manual(" ", values=c("darkgreen", "black"),
                          breaks=c('Majority Latino', "All other"), 
                          labels=c('Majority Latino', "All other")) +
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

subset_years_cws <- function(year_start, pollutant, year_end, by = 1) {
  years_desired <- seq(year_start, year_end, by = by)
  
  n_years <- seq(year_start, year_end, by = by) %>% length()
  
  pollutant_int <- pollutant %>% 
    filter(year>=year_start) %>% 
    group_by(SYSTEM_NO) %>% 
    filter(n()>=n_years, min(year)<= year_start, max(year)>=year_end) %>% 
    arrange(SYSTEM_NO, year) %>% 
    mutate(diff_year = lead(year)-year) %>% 
    filter(max(diff_year, na.rm = TRUE)==1, year>=year_start, year <= year_end)
  
  return(pollutant_int)
  
}

# mod <- mod_ar_lag3

# this function returns the sum of lagged coefficients

sum_lags <- function(mod, nlags = 3, int_terms = c("gw0", 'raw0', "gw0|raw0"), pollutant = "ar", scale_by = NULL) {
 x <- as_tibble(mod$coefficients) %>% mutate(beta = row.names(mod$coefficients))
 vcov <- mod$clustervcv
 vcov <- mod$vcv
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
 
 if (is.null(scale_by)) {
  n <- mod$N
  out <- tibble(term = c("d", int_terms), estimate = coeff, std.error = se_d) %>% 
    mutate(statistic = estimate/std.error,
           p.value = 2*pt(-abs(statistic),df=n-1))
} else {
  n <- mod$N
  out <- tibble(term = c("d", int_terms), estimate = coeff*scale_by, 
                std.error = se_d*scale_by) %>% 
    mutate(statistic = estimate/std.error,
           p.value = 2*pt(-abs(statistic),df=n-1))
}
  return(out)
}

residualize <- function(df, vars_to_resid, fes = 'factor(year)') {
  df <- df %>% drop_na(all_of(vars_to_resid))
  for (v in vars_to_resid) {
    form = as.formula(paste0(v, " ~ ", fes))
    mod <- lm(form, data = df)
    df[v] <- as.double(mod$residuals)
  }
  return(df)
}

prep_reg <- function(df) {
  
  df <- df %>% arrange(samplePointID, year)
  
  mean.d <- mean(df$mean_pdsi, na.rm = TRUE)
  sd.d <- sd(df$mean_pdsi, na.rm = TRUE)
  
  x <- df %>% 
    group_by(samplePointID) %>% 
    mutate(
      d = ((mean_pdsi-mean.d)*-1)/sd.d,
      # d = if_else(mean_pdsi <= -1, 1, 0), 
      dlead = lead(d),
      dlead2 = lead(dlead),
      dlag1 = lag(d),
      dlag2 = lag(dlag1),
      dlag3 = lag(dlag2),
      gXraw0 = factor(gw*(raw==0), levels = c('0', '1')),
      gXraw = factor(gw*raw, levels = c('0', '1'))) %>% 
    mutate(
      gw = factor(gw, levels = c("1", "0")),
      raw = factor(raw),
      SYSTEM_NO = factor(SYSTEM_NO),
      RegulatingAgency = factor(RegulatingAgency)
    ) %>%
    group_by(SYSTEM_NO, year) %>%
    mutate(n_spid = 1 / (unique(samplePointID) %>% length()))
  return(x)
}

prep_reg_cws <- function(df) {
  df <- df %>% arrange(SYSTEM_NO, year)
  mean.d <- mean(df$mean_pdsi, na.rm = TRUE)
  sd.d <- sd(df$mean_pdsi, na.rm = TRUE)
  ni_drought <- df %>% 
    group_by(SYSTEM_NO) %>% 
    mutate(
      d = ((mean_pdsi-mean.d)*-1)/sd.d,
      # d = if_else(mean_pdsi <= -1, 1, 0), 
      dlead = lead(d),
      dlead2 = lead(dlead),
      dlag1 = lag(d),
      dlag2 = lag(dlag1),
      dlag3 = lag(dlag2)) %>% 
    mutate(
      SYSTEM_NO = factor(SYSTEM_NO),
      RegulatingAgency = factor(RegulatingAgency)
    ) 
  return(ni_drought)
}

sum_coeffs <- function(mod, int_terms = c('d:b_majority_latino', 
                                          "d:log_hh_income"), 
                       pollutant = "ar", scale_by = c(1, -2)) {
 
  # vcov <- mod$clustervcv
  vcov <- mod$vcv
  vcov <- vcov[int_terms, int_terms]
  for (i in 1:length(scale_by)) {
    vcov[i, i] <- vcov[i, i]*(scale_by[i])^2
  }
  n <- mod$N
  x <- as_tibble(mod$coefficients) %>% mutate(beta = row.names(mod$coefficients)) %>% 
    filter(beta %in% int_terms) %>% 
    bind_cols(tibble(scale = scale_by)) %>% 
    mutate(estimate = mean_n*scale_by)
  
    out <- tibble(estimate = sum(x$estimate), 
                  std.error = sqrt(sum(vcov))) %>% 
      mutate(statistic = estimate/std.error,
             p.value = 2*pt(-abs(statistic),df=n-1))
  return(out)
}

sum_marginal <- function(mod, nlags = 3, int_terms = c('gw0', ':raw0|gXraw0', ':raw0|gw0'), pollutant = "ar") {
  e <- 1+nlags
  df <- as_tibble(mod$coefficients) %>% mutate(beta = row.names(mod$coefficients)) %>% 
    mutate(beta = str_replace(beta, "d$|d(?=:)", "dlag0"))
  vcov <- mod$vcv
  row.names(vcov) <- row.names(vcov) %>% str_replace("d$|d(?=:)", "dlag0")
  colnames(vcov) <-  colnames(vcov) %>% str_replace("d$|d(?=:)", "dlag0")
  # x %>% filter(str_detect(beta, "d$|dlag\\d$"))
  
  # calculating point estimate and se for gw-raw over time
  coeff <- df %>% filter(str_detect(beta, "d$|^dlag\\d$")) %>%
    mutate(se = mod$se[1:e], 
           int_terms = " ")
  out <- c()
  for (j in 1:length(int_terms)) {
    # now within each interaction terms we calculate for each lags
    out[[j]] <- map(0:nlags, function(x){
      
      tmp <- df %>% filter(str_detect(beta, paste0("dlag", x))) %>%
        filter(str_detect(beta, paste0("^dlag", x, "$")))
      tmp2 <- df %>% filter(str_detect(beta, paste0("dlag", x))) %>% 
        filter(str_detect(beta, int_terms[j])) 
      tmp <- tmp %>% bind_rows(tmp2)
      ind <- row.names(vcov) %in% tmp$beta %>% which()
      vcov_tmp <- vcov[ind, ind]
        tibble(
          !!paste0('mean_', pollutant) := sum(tmp[,1]),
          se = sqrt(sum(vcov_tmp)),
          beta = paste0('dlag', x),
          int_terms = int_terms[j]
        )
    }) %>% bind_rows()
 
  } 
  n <- mod$N
  out <- bind_rows(coeff, out) %>%
    mutate(t_val = UQ(rlang::sym(paste0('mean_', pollutant))) / se,
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

plot_int <- function(mod, nlags=0) {
  df <- mod %>% broom::tidy()
  ggplot(df, aes(x = term, y = estimate)) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                  width = .1) +
    theme_minimal_vgrid() +
    # ylim(c(-1, 1)) +
    # scale_y_continuous(n.breaks = 11) +
    # coord_flip() +
    # labs(x = ' ', y = yl,
    #      subtitle = t)
    geom_hline(yintercept = 0, color = 'red') +
    geom_point() 
 
}

# save_plot("Plots/cumulative_lagged_effects_ar.png", plot_coeff(df), scale = 1,
#           base_asp = 2)
smallerMod = function(mod) {
  # mod$residuals = NULL
  mod$response = NULL
  mod$c.fitted.values = NULL
  mod$fitted.values = NULL
  mod$r.residuals = NULL
  return(mod)
}


reg_delivered <- function(df, nlags, xvar, yvar, fe = "0", clust = "0", plot = TRUE,
                          save.reg = '../Data/1int/mod.rds') {
  
  # created list with list of x var in it.
  # for now fe is fixed
  form = map(xvar, function(x) { as.formula(paste0(yvar, " ~ ", x,   " | ", fe, " | 0 | ", clust)) })
  
  mod <- map(form, ~( smallerMod(felm(formula = ., data = df)))) 
  
  # if opt to output plot then return the plot as from dw
  if (plot) {
   mod <- mod %>% map(broom::tidy) %>% 
      map(~(.x %>% mutate(model = paste('mod', which(unlist(xvar)==.x$term %>% paste(collapse = '+')))) %>% 
              filter(!str_detect(term, ':year')))) 
 dwplot(bind_rows(mod)) + theme_minimal()
      
  } else {
    if (yvar == 'mean_n') {
      lb = 'N (mg/l)'
    } else {
      lb = 'As (ug/l)'
    }
    if (!is.null(save.reg)) {
      saveRDS(mod, save.reg)
    }
    stargazer(mod, omit = ':year|Constant', 
              title = 'Impacts of a unit increase in drought measure',
              dep.var.labels = paste0('Mean concentration of ', lb), digits = 3,
              # add.lines = list(c("Fixed effects?", "CWS and year",  "CWS and year", "CWS linear trends", "CWS linear trends")),
              single.row = TRUE)
  }
  }
