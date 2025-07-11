# notes ----
## gkc size at maturity
## tyler jackson

# load ----

library(tidyverse)
library(RTMB)
library(segmented)
library(gmacsr); theme_set(theme_sleek())
library(sf)

# ols function - this isn't used anymore, but i want to keep the code as written
seg_ols <- function(data, pars = list(beta0 = 0, beta1 = 0, beta2 = 0, psi = 120)) {
  
  model <- function(pars) {
    # get parameters and data
    RTMB::getAll(pars, data)
    
    # predictions
    # indicator 
    i = (cl - psi) > 0
    pred_ch = beta0 + beta1 * cl + beta2 * (cl - psi) * i
    
    # obj
    rss = sum((ch - pred_ch)^2)
    sigma = sqrt(rss / length(pred_ch)   )                 # MLE estimate of sigma^2
    likelihood = -sum( log(dnorm(x = ch, mean = pred_ch, sd = sigma ) ))
    
    REPORT(pred_ch)
    REPORT(ch)
    REPORT(cl)
    
    return(likelihood)
  }
  # build model
  TapeConfig(comparison = "tape")
  mod <- RTMB::MakeADFun(model, parameters = pars)
  # optimize
  opt <- stats::nlminb(mod$par, mod$fn, mod$gr,
                       control = list(iter.max = 1e5, eval.max = 1e5, rel.tol = 1e-15))
  # output
  out <- list()
  out$sdreport <- sdreport(mod)
  data$pred_ch_ols <- mod$report()$pred_ch
  out$data <- data
  out$psi <- opt$par[4]
  out$psi_se <- summary(sdreport(mod))[4, 2]
  out$psi_95 <- opt$par[4] + c(-1.96, 1.96)*summary(sdreport(mod))[4, 2]

  
  
  return(out)
  
}

# segmented function
seg_repar <- function(data, psi_start = 120) {
  lm(ch~cl, data = data) -> fit
  seg = segmented(fit, seg.Z = ~cl, psi = psi_start, control = seg.control(n.boot = 100))
  
  out <- list()
  out$fit <- seg
  out$summary <- summary(seg)
  data$pred_ch_repar <- seg$fitted.values[,1]
  out$data <- data
  out$psi <- seg$psi[2]
  out$psi_se <- seg$psi[3]
  out$psi_95 <- seg$psi[2] + c(-1.96, 1.96)*seg$psi[3]

  
  return(out)
}


# data ----

sm_dat <- readRDS("./AIGKC/data/maturity/aigkc_chela_data_all.RDS") 

# map of data ----

## land boundary
land <- geodata::gadm("United States", level = 1, path = "./AIGKC/data/maps")

st_as_sf(land) %>%
  st_shift_longitude() %>% 
  st_crop(c(xmin = 170, ymin = 51, xmax = 195.27, ymax = 57)) %>% 
  st_transform(32602) -> aleut

sm_dat %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(land)) %>%
  st_shift_longitude() %>% 
  st_transform(32602) -> pts

ggplot()+
  geom_sf(data = aleut)+
  geom_sf(data = pts, aes(color = source), alpha = 0.5)+
  geom_vline(xintercept = 303379.1, linetype = 2)+
  geom_text_npc(data = pts, aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  facet_wrap(~crab_year, ncol = 2, dir = "v")+
  labs(color = NULL)+
  scale_color_manual(values = cbpalette[1:3])+
  theme(axis.text = element_blank(),
        panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) -> x
ggsave("AIGKC/figures/maturity/sample_map.png", plot = x, height = 6, width = 6, units = "in")

# simulation -----

seg_repar(sm_dat)

#parameters
n <- 1000
beta0 <- -2.67
beta1 <- 0.24
beta2 <- 0.20
n_sim <- 1000
mean_psi <- 122.467

sim_grid <- expand_grid(eps_sig = c(1, 3, 6), psi_sig = c(1, 3, 6)) %>% as.matrix()

sim_data <- list(list())
sim_res <- tibble(iteration = 1:(nrow(sim_grid)*n_sim), eps_sig = NA, psi_sig = NA,
                  sizer = NA,repar = NA, repar_se = NA, repar_l95 = NA, repar_u95 = NA)



for(k in 1:nrow(sim_grid)){
  
  for (j in 1:n_sim) {
    cl <- runif(n, 50, 190)
    #sigma <- runif(1, 1, 5)  # varying standard deviation
    epsilon <- rnorm(n, mean = 0, sd = sim_grid[k, 1])  # error term
    psi_i <- mean_psi + rnorm(n, 0, sim_grid[k, 2])
    i <- (cl - psi_i) > 0
    ch <- beta0 + beta1 * cl + beta2 * (cl - psi_i) * i + epsilon
    
    index <- (k-1)*n_sim + j
    
    sim_data[[index]] <- tibble(eps_sig = sim_grid[k, 1], psi_sig = sim_grid[k, 2], cl, ch)
    
    repar <- seg_repar(sim_data[[index]])
    sizer <- SiZer::piecewise.linear(x = sim_data[[index]]$cl, y = sim_data[[index]]$ch)
    
    sim_res$iteration[index] <- index
    
    sim_res$eps_sig[index] <- sim_grid[k, 1]
    sim_res$psi_sig[index] <- sim_grid[k, 2]
    
    sim_res$sizer[index] <- sizer$change.point
    sim_res$repar[index] <- repar$psi
    sim_res$repar_se[index] <- repar$psi_se
    sim_res$repar_l95[index] <- repar$psi_95[1]
    sim_res$repar_u95[index] <- repar$psi_95[2]
    
  }
}


saveRDS(sim_data, "./AIGKC/output/maturity/maturity_sim_data.RDS")
saveRDS(sim_res, "./AIGKC/output/maturity/maturity_sim_res.RDS")

sim_res <- readRDS("./AIGKC/output/maturity/maturity_sim_res.RDS")

# sim_res %>%
#   mutate(ols_ci = (psi >= ols_l95 & psi <= ols_u95),
#          repar_ci = (psi >= repar_l95 & psi <= repar_u95)) %>%
#   summarise(ols_ci = round(sum(ols_ci) / 1000, 2),
#             repar_ci = round(sum(repar_ci) / 1000, 2))
# 
# sim_res %>%
#   transmute(iteration, sigma, ols_cv = ols_se / ols, repar_cv = repar_se / repar) %>%
#   pivot_longer(3:4) %>%
#   mutate(name = ifelse(name == "ols_cv", "OLS", "Newton-Raphson")) %>%
#   ggplot()+
#   geom_boxplot(aes(x = name, y = value))+
#   labs(x = NULL, y = "CV") -> x
# ggsave("AIGKC/figures/maturity/sim_cv_compare.png", plot = x, width = 3, height = 4, units = "in")

sim_res %>%
  mutate(psi_sig = paste0("psi[sigma]", " == ", psi_sig),
         eps_sig = paste0("epsilon[sigma]", " == ", eps_sig)) %>%
  ggplot()+
  geom_point(x = mean_psi, y = mean_psi, color = "firebrick", size = 5, shape = 21)+
  geom_point(aes(x = sizer, y = repar))+
  geom_line(data = tibble(x = 10:145), aes(x = x, y = x), linetype = 2)+
  scale_y_continuous(limits = c(100, 139), expand = c(0,0))+
  scale_x_continuous(limits = c(100, 139), expand = c(0,0))+
  labs(x = "Grid Search", y = "Newton-Raphson") +
  facet_grid(rows = vars(psi_sig), cols = vars(eps_sig), labeller = label_parsed) -> x
ggsave("AIGKC/figures/maturity/sim_est_compare.png", plot = x, width = 7, height = 7, units = "in")

# stats
sim_res %>%
  mutate(diff = repar - sizer) %>%
  group_by(eps_sig, psi_sig) %>%
  summarise(n_same = sum(round(sizer, 1) == round(repar, 1)) / 1000,
            mean_diff = mean(diff)) %>% ungroup %>%
  write_csv("AIGKC/output/maturity/sim_res.csv")

# analysis by data source ----

sm_dat %>%
  filter(source != "Dockside") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict, source) %>% ungroup %>% #pull(data) %>% .[[2]] -> data
  mutate(fit = purrr::map(data, function(data){seg_repar(data, psi_start = 130)}),
         psi = purrr::map_dbl(fit, function(fit) {fit$psi}),
         psi_se = purrr::map_dbl(fit, function(fit) {fit$psi_se}),
         ch_fit =  purrr::map(fit, function(fit) {fit$data$pred_ch_repar})) %>%
  dplyr::select(-fit) %>%
  unnest(c(data, ch_fit)) -> source_fit

source_fit %>% 
  group_by(subdistrict, source) %>%
  summarise(psi = mean(psi),
            l95 = mean(psi)-1.96*mean(psi_se),
            u95 = mean(psi)+1.96*mean(psi_se)) %>% ungroup -> ci

source_fit %>%
  mutate(annotation = paste0("\u03C8 = ", round(psi, 1))) %>% 
  ggplot()+
  geom_point(aes(x = cl, y = ch), shape = 16, color = "grey90")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = annotation), check_overlap = T, size = 3)+
  geom_rect(data = ci, aes(xmin = l95, xmax = u95, ymin = -Inf, ymax = Inf), alpha = 0.5, fill  = cbpalette[3])+
  geom_vline(aes(xintercept = psi), color = 1, linetype = 2)+
  
  geom_line(aes(x = cl, y = ch_fit))+
  facet_grid(rows = vars(source), cols = vars(subdistrict))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (mm)") -> x
ggsave("AIGKC/figures/maturity/est_by_source_subdistrict.png", plot = x, width = 7, height = 6, units = "in")


source_fit %>%
  mutate(annotation = paste0("\u03C8 = ", round(psi, 1))) %>% 
  mutate(cl_bin = floor(cl / 10) * 10) %>%
  group_by(source, subdistrict, cl_bin) %>%
  mutate(mean_ch = mean(ch)) %>%
  ggplot()+
  geom_boxplot(aes(x = cl_bin + 5, y = ch, group = cl_bin), color = "grey90")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = annotation), check_overlap = T, size = 3)+
  geom_rect(data = ci, aes(xmin = l95, xmax = u95, ymin = -Inf, ymax = Inf), alpha = 0.5, fill  = cbpalette[3])+
  geom_vline(aes(xintercept = psi), color = 1, linetype = 2)+
  
  geom_line(aes(x = cl, y = ch_fit))+
  facet_grid(rows = vars(source), cols = vars(subdistrict))+
  labs(x = "Carapace Length (mm)", y = "Chela Height (mm)") -> x
ggsave("AIGKC/figures/maturity/est_by_source_subdistrict_boxplot.png", plot = x, width = 7, height = 6, units = "in")

# bootstrap distribution of psi by source ----

sm_dat %>%
  filter(source != "Dockside") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict, source) %>% ungroup %>%
  expand_grid(boot = 1:1000, .) %>%
  mutate(boot_fit = purrr::map_dbl(data, function(data){
    # take bootstrap sample
    sample <- sample_n(data, nrow(data), replace = T)
    # find starting value of psi
    psi_start <- (max(sample$cl) - min(sample$cl)) / 2 + min(sample$cl)
    seg_repar(sample, psi_start = psi_start)$psi
    })) -> boot
saveRDS(boot, "AIGKC/output/maturity/source_boot.RDS")
boot <- readRDS("AIGKC/output/maturity/source_boot.RDS")

boot %>%
  dplyr::select(-data) %>%
  group_by(subdistrict, source) %>%
  mutate(mean_psi = mean(boot_fit)) %>%
  left_join(ci, by = join_by(subdistrict, source)) %>%
  
  ggplot()+
  geom_density(aes(x = boot_fit))+
  geom_vline(aes(xintercept = mean_psi), linetype = 2)+
  geom_vline(aes(xintercept = psi))+
  labs(x = "Carapace Length (mm)", y = "Probability Density")+
  facet_grid(rows = vars(source), cols = vars(subdistrict)) -> x
ggsave("AIGKC/figures/maturity/est_by_source_subdistrict_bootstrap.png", plot = x, width = 7, height = 6, units = "in")

# likelihood profile ----

sm_dat %>%
  filter(source != "Dockside") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict, source) %>% ungroup %>% #pull(data) %>% .[[2]] -> data
  mutate(prof = map(data, function(data) {
    
    # Fit initial model
    fit <- lm(ch ~ cl, data = data)
    
    # Define grid of breakpoint values
    psi_vals <- seq(100, 150, by = 0.5)
    loglik <- tibble(psi_fix = psi_vals, nll = NA)
    # Loop over fixed breakpoints
    for (i in 1:length(psi_vals)) {
      seg_fit <- segmented(fit, seg.Z = ~cl, psi = list(cl = psi_vals[i]), control = seg.control(it.max = 0))
      loglik$nll[i] <- -logLik(seg_fit)
    }
    return(loglik)
    
  }),
    psi = map_dbl(data, function(data){seg_repar(data, psi_start = 130)$psi})
   ) %>%
  dplyr::select(-data) %>%
  unnest(prof) -> lik

lik %>%
  group_by(source, subdistrict) %>%
  mutate(psi_opt = psi_fix[nll == min(nll)]) %>%
  ggplot()+
  geom_line(aes(x = psi_fix, y = nll))+
  geom_vline(aes(xintercept = psi), linetype = 2, color = "grey80")+
  geom_vline(aes(xintercept = psi_opt), color = 1)+
  labs(x = "Carapace Length (mm)", y = "Negative Log-Likelihood")+
  facet_wrap(subdistrict ~ source, scales = "free_y", dir = "v") -> x
ggsave("AIGKC/figures/maturity/est_by_source_subdistrict_profile.png", plot = x, width = 7, height = 6, units = "in")




# analysis and profile by subdistrict----

sm_dat %>%
  filter(source != "Dockside") %>%
  #filter(source != "Observer") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict) %>% ungroup %>% #pull(data) %>% .[[2]] -> data
  mutate(prof = map(data, function(data) {
    
    # Fit initial model
    fit <- lm(ch ~ cl, data = data)
    
    # Define grid of breakpoint values
    psi_vals <- seq(100, 150, by = 0.5)
    loglik <- tibble(psi_fix = psi_vals, nll = NA)
    # Loop over fixed breakpoints
    for (i in 1:length(psi_vals)) {
      seg_fit <- segmented(fit, seg.Z = ~cl, psi = list(cl = psi_vals[i]), control = seg.control(it.max = 0))
      loglik$nll[i] <- -logLik(seg_fit)
    }
    return(loglik)
    
  }),
  psi = map_dbl(data, function(data){seg_repar(data, psi_start = 130)$psi})
  ) %>%
  dplyr::select(-data) %>%
  unnest(prof) -> sublik

sublik %>%
  group_by(subdistrict) %>%
  mutate(psi_opt = psi_fix[nll == min(nll)]) %>%
  mutate(annotation = paste0("\u03C8 = ", round(psi_opt, 1))) %>% 
  ggplot()+
  geom_line(aes(x = psi_fix, y = nll))+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = annotation), check_overlap = T, size = 3)+
  geom_vline(aes(xintercept = psi), linetype = 2, color = "grey80")+
  geom_vline(aes(xintercept = psi_opt))+
  labs(x = "Carapace Length (mm)", y = "Negative Log-Likelihood")+
  facet_wrap(~subdistrict, scales = "free_y", dir = "v") -> x

sm_dat %>%
  filter(source != "Dockside") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict) %>% ungroup %>% #pull(data) %>% .[[2]] -> data
  mutate(fit = purrr::map(data, function(data){seg_repar(data, psi_start = 130)}),
         psi = purrr::map_dbl(fit, function(fit) {fit$psi}),
         psi_se = purrr::map_dbl(fit, function(fit) {fit$psi_se}),
         ch_fit =  purrr::map(fit, function(fit) {fit$data$pred_ch_repar})) %>%
  dplyr::select(-fit) %>%
  unnest(c(data, ch_fit)) -> sub_fit

sub_fit %>% 
  group_by(subdistrict) %>%
  summarise(psi = mean(psi),
            l95 = mean(psi)-1.96*mean(psi_se),
            u95 = mean(psi)+1.96*mean(psi_se)) %>% ungroup -> sub_ci

sub_fit %>%
  mutate(annotation = paste0("\u03C8 = ", round(psi, 1))) %>% 
  ggplot()+
  geom_point(aes(x = cl, y = ch), shape = 16, color = "grey80")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = annotation), check_overlap = T, size = 3)+
  geom_rect(data = sub_ci, aes(xmin = l95, xmax = u95, ymin = -Inf, ymax = Inf), alpha = 0.5, fill  = cbpalette[3])+
  geom_vline(aes(xintercept = psi), color = 1, linetype = 2)+
  
  geom_line(aes(x = cl, y = ch_fit))+
  facet_wrap(~subdistrict, scales = "free_y", dir = "v")+
  labs(x = "Carapace Length (mm)", y = "Chela Height (mm)") -> y

ggsave("AIGKC/figures/maturity/est_by_subdistrict.png", 
       plot = x + y + plot_layout(widths = c(1, 2)), 
       width = 7, height = 6, units = "in")


# bootstrap by sudustrict ----

sm_dat %>%
  filter(source != "Dockside") %>%
  filter(!(source == "Observer" & cl < 100 & ch > 35)) %>%
  nest_by(subdistrict) %>% ungroup %>%
  expand_grid(boot = 1:1000, .) %>%
  mutate(boot_fit = purrr::map_dbl(data, function(data){
    # take bootstrap sample
    sample <- sample_n(data, nrow(data), replace = T)
    # find starting value of psi
    psi_start <- (max(sample$cl) - min(sample$cl)) / 2 + min(sample$cl)
    seg_repar(sample, psi_start = psi_start)$psi
  })) -> subboot
saveRDS(subboot, "AIGKC/output/maturity/subdistrict_boot.RDS")
subboot <- readRDS("AIGKC/output/maturity/subdistrict_boot.RDS")

subboot %>%
  dplyr::select(-data) %>%
  group_by(subdistrict) %>%
  mutate(mean_psi = mean(boot_fit)) %>%
  left_join(sub_ci, by = join_by(subdistrict)) %>%
  
  ggplot()+
  geom_density(aes(x = boot_fit))+
  geom_vline(aes(xintercept = mean_psi), linetype = 2)+
  geom_vline(aes(xintercept = psi))+
  labs(x = "Carapace Length (mm)", y = "Probability Density")+
  facet_wrap(~subdistrict, scales = "free_y", dir = "v") -> x
ggsave("AIGKC/figures/maturity/est_by_subdistrict_bootstrap.png", plot = x, width = 6, height = 6, units = "in")

# output tables ----

# psi estimates
sm_dat %>%
  filter(source != "Dockside",
         !(source == "Observer" & cl < 100 & ch > 35),
         !is.na(cl), !is.na(ch)) %>%
  count(subdistrict, source) %>%
  
  bind_rows(sm_dat %>%
              filter(source != "Dockside",
                     !(source == "Observer" & cl < 100 & ch > 35),
                     !is.na(cl), !is.na(ch)) %>%
              count(subdistrict) ) %>%
  
  left_join(ci %>%
              left_join(lik %>%
                          group_by(subdistrict, source) %>%
                          summarise(lik_psi = psi_fix[nll == min(nll)])) %>%
              bind_rows(sub_ci %>%
                          left_join(sublik %>%
                                      group_by(subdistrict) %>%
                                      summarise(lik_psi = psi_fix[nll == min(nll)]))) )%>%
  write_csv("AIGKC/output/maturity/psi_estimates_sept25.csv")


# trim probably erroneuous data ----

sm_dat %>%
  # step 1: fit segmented regression to all of data, minus the two very obvious outliers
  filter(!(cl < 100 & ch > 30)) %>%
  seg_repar(.) %>% .$psi -> all_dat_psi

# step 2: filter initial dataset for observations within 1.5 x the IQR of ch1/cl each side of the all data psi
sm_dat %>%
  filter(!is.na(ch), !is.na(cl)) %>%
  mutate(size = ifelse(cl <= all_dat_psi, "imm", "mat")) %>%
  group_by(size) %>% nest %>% 
  
  mutate(data = purrr::map(data, function(data) {
    
    data %>%
      mutate(chcl = ch/cl) %>%
      pull(chcl) %>%
      quantile(., c(0.25, 0.75)) %>%
      as.numeric -> quant
    
    data %>%
      filter(ch/cl > (quant[1] - 1.5 * (quant[2] - quant[1])),
             ch/cl < (quant[2] + 1.5 * (quant[2] - quant[1]))) -> trim
    
    return(trim)
    
  })) %>%
  
  unnest(data) %>%
  ungroup %>%
  dplyr::select(-size) -> aigkc_trim

# repeat source / subdistrict analysis with trimmed data ----

aigkc_trim %>%
  filter(!is.na(lon), !is.na(lat), lat <= 55) %>%
  mutate(lon_bin = (floor(lon / 5) * 5),
         lon_bin = ifelse(lon_bin > 0, -360 + lon_bin, lon_bin)) %>%
  arrange(lon_bin, cl)-> lme_data

# fit segmented lme
l_mod <- lme(ch ~ cl, random = ~1|lon_bin, lme_data)
seg_mod_lme <- segmented.lme(l_mod, seg.Z = ~cl, psi = 120, random=list(lon_bin = pdDiag(~1 + cl + U + G0)))
summary(seg_mod_lme) 

## issues with pulling the fitted values from the model, so need to join with data after sorting
fits <- fitted.segmented.lme(seg_mod_lme, sort = F)

## breakpoints
tibble(lon_bin = as.numeric(names(seg_mod_lme$psi.i)),
       psi = seg_mod_lme$psi.i) -> re_psi

## plot of psi as a function of longitude bin
re_psi %>%
  ggplot()+
  geom_point(aes(x = lon_bin, y = psi))+
  geom_smooth(aes(x = lon_bin, y = psi), method = "lm", color = 1, se = F)+
  scale_x_continuous(labels = c(expression(170*degree*E), expression(175*degree*E), expression(180*degree), 
                                expression(175*degree*W), expression(170*degree*W)))+
  theme_sleek()+
  labs(x = "Longitudinal Bin", y = "Size at Maturity (mm)") -> x
ggsave("./figures/size_at_maturity/re_model_psi_longitude_eda.png", height = 3, width = 4, units = "in")

## plot random effect model fits to data
lme_data %>%
  mutate(fit = fits) %>%
  dplyr::select(lon_bin, cl, ch, fit) %>%
  ggplot()+
  geom_point(aes(x = cl, y = ch), color = "grey70", alpha = 0.5)+
  geom_line(aes(x = cl, y = fit), color = 1)+
  facet_wrap(~lon_bin, 
             labeller =labeller(lon_bin = ~paste0(c(170, 175, 180, 175, 170), "°", c(rep("E", 2), "", rep("W", 2)))))+
  labs(x = "Carapace Length (mm)", y = "Standard Carapace Height (mm)")+
  scale_color_manual()
theme_sleek() -> x
ggsave("./figures/size_at_maturity/re_model_fit_longitude_eda.png", height = 5, width = 6, units = "in")


