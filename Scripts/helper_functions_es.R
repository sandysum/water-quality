
# This function takes in a N/AS dataset, pollutant name, subsets to water type, and outputs the event study
# dataset and year coefficients
es_mod <- function(df, pollutant = 'as_ugl', w = c('S'), year_start = 2006, r = c(1, 0)) {
  # browser()
  mod <- df %>%
    mutate(!!pollutant := Winsorize(!!sym(pollutant), probs = c(0, .99), na.rm = TRUE)) %>%
    filter(year >= year_start, raw %in% r, WATER_TYPE %in% w) %>% 
    mutate(
      td = str_extract(sampleTime, "\\d{2}") %>% as.numeric()
      %>% if_else((. == 44 | . == 80), NA_real_, .),
      dy = yday(sampleDate),
      year = factor(year)
    ) %>% filter(td <25)
  
  # plot.means("as_ugl", df = mod, by.var = 'td')
  
  # drop rows with no time of sample
  mod <- mod %>% tidyr::drop_na(td, dy)
  
  # create polynomials
  poly_td <- poly(mod$td, degree = 3) %>% as_tibble()
  names(poly_td) <- paste0('td_', 1:3)
  poly_dy <- poly(mod$dy, degree = 3) %>% as_tibble()
  names(poly_dy) <- paste0('dy_', 1:3)
  
  mod <- mod %>% bind_cols(poly_td, poly_dy)
  
  es <- felm(as.formula(paste(pollutant, '~ year + td_1 + td_2 + td_3 + dy_1 + dy_2 + dy_3 | samplePointID | 0 | SYSTEM_NO')),
                data = mod)
  return(list(mod, es))
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

# plot_es(asw[[2]], asw[[1]])


plot_es <- function(df, w = c('S'), pollutant = 'as_ugl', years = 2005:2020,
                     ylm = c(1.2,4), main = ' ', r = c(1), 
                     ylab = 'Mean As conc. (g/l)\n') {
  
  es <- es_mod(df = df %>% filter(year %in% years), 
               pollutant = pollutant, w = w, year_start = min(years), r = r)
  
  # browser()
  se <- es[[2]]$se %>% as_tibble() %>% mutate(coef = names(es[[2]]$se))
  x <- as_tibble(es[[2]]$coefficients) %>% mutate(coef = rownames(es[[2]]$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  
  bs <- tibble(
    year_n = min(years),
    avg = mean(getfe(es[[2]])[['effect']])
  )
  
  x <- x %>% filter(str_detect(coef, 'year')) %>%
    mutate(year_n = as.integer(str_extract(coef, "\\d{4}")),
           avg = !!rlang::sym(pollutant) + bs$avg) %>% 
    bind_rows(bs) %>% 
    arrange(year_n) %>% 
    mutate(upr = 1.96*se+avg,
           lwr = avg - 1.96*se) 
  
  x %>% 
    filter(year_n > min(years)) %>% 
    ggplot(aes(year_n, avg)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .4, fill = 'lightgrey') +
    theme_minimal_hgrid() +
    ggtitle(label = main) +
    lims(y = ylm, x = c(min(years), max(years))) +
    labs(x = '\nYear', y = ylab) +
    scale_x_continuous(breaks = seq(min(years), max(years), 2)) +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
          plot.background = element_rect(fill = "white", color = NA)) +
    theme(legend.position = 'top')
  
}

plot_es2 <- function(df, w = c('S'), pollutant = 'as_ugl', by = 'b_majority_latino', years = 2005:2020,
                              ylm = c(1.2,4), main = ' ', r = c(1), 
                              ylab = 'Mean N conc. (mg/l)\n') {
  
  if (by=='b_majority_latino') {
    brk = 'Majority Latino'
    cl = 'darkgreen'
  } else {
    brk = 'Low income'
    cl = 'blue'
  }
  
  es <- es_mod(df = df %>% filter((!!rlang::sym(by))==1, year %in% years), 
               pollutant = pollutant, w = w, year_start = min(years), r = r)
  
  es2 <- es_mod(df = df %>% filter((!!rlang::sym(by))==0, year %in% years), 
               pollutant = pollutant, w = w, year_start = min(years), r = r)
  
  # browser()
  se <- es[[2]]$se %>% as_tibble() %>% mutate(coef = names(es[[2]]$se))
  x <- as_tibble(es[[2]]$coefficients) %>% mutate(coef = rownames(es[[2]]$coefficients)) %>%
    left_join(se) %>% 
    rename(se = value)
  
  bs <- tibble(
    year_n = min(years),
    avg = mean(getfe(es[[2]])[['effect']])
  )
  
  se2 <- es2[[2]]$se %>% as_tibble() %>% mutate(coef = names(es2[[2]]$se))
  x2 <- as_tibble(es2[[2]]$coefficients) %>% mutate(coef = rownames(es2[[2]]$coefficients)) %>%
    left_join(se2) %>% 
    rename(se = value)
  
  bs2 <- tibble(
    year_n = min(years),
    avg = mean(getfe(es2[[2]])[['effect']])
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
           group = brk) %>% 
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
    scale_x_continuous(breaks = seq(min(years), max(years), 2)) +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = .9, vjust = .9),
          plot.background = element_rect(fill = "white", color = NA)) +
    scale_colour_manual(" ", values=c(cl, "black"),
                        breaks=c(brk, "All other"), 
                        labels=c(by, "All other")) +
    theme(legend.position = 'none')
  
}

# create function


source_reg <- function(df, pollutant) {
  
  m1 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d | factor(year)")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  # summary(m1)
  
  m2 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino | factor(year)")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  
  # summary(m2)
  m3 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino | log_hh_income + log_pop_caswrb + percent_ag + avg_percent_clay + RegulatingAgency + factor(year)")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  # summary(m3)
  
  m4 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino + d:log_hh_income + d:percent_ag + d:ag_wells_n + d:log_pop_caswrb | log_hh_income + log_pop_caswrb + percent_ag + avg_percent_clay + RegulatingAgency + factor(year)")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  summary(m4)
  
  m5 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino + d:log_hh_income + d:percent_ag + d:log_pop_caswrb | log_hh_income + log_pop_caswrb + percent_ag+avg_percent_clay + RegulatingAgency + SYSTEM_NO[year]")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  # summary(m5)
  
  m6 <- feols(fml = as.formula(paste0('mean_', pollutant, " ~ d + d:b_majority_latino + d:b_low_income + d:ag_wells_n | samplePointID + SYSTEM_NO[year]")), 
              data = df, weights = df$n_spid, vcov = ~SYSTEM_NO)
  summary(m6)
  
etable(m1, m2, m3, m4, m5, m6, tex = TRUE,
       digits = 3, order = c('d$', 'd:'), drop = 'Intercept')
  
}
