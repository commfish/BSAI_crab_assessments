# notes ----
# observer cpue standardization with GAMs
# tyler jackson
# 10/30 / 2023

# load ----

source("./AIGKC/code/aigkc_functions.R")
library(mgcv)
library(mgcViz)
library(maps)

# data ----

# fish ticket data
ft_raw <- read_csv("./AIGKC/data/observer/item2_linked_fish_ticket_dump.csv")

# observer pots
obs_raw <- read.csv("./AIGKC/data/observer/item1_linked_potsum_dump.csv", na.strings = -9)

# season dates
season_dates <- readRDS("./AIGKC/data/observer/season_dates.RDS")

# bathy slope
eag_slope <- readRDS("./AIGKC/data/bathy/eag_bathy_slope.RDS")
wag_slope <- readRDS("./AIGKC/data/bathy/wag_bathy_slope.RDS")


ai <- raster::getData("GADM", country = c("USA"), level = 1, path = "./AIGKC/data/maps")
ai@data <- filter(ai@data, NAME_1 == "Alaska")
eag_proj <- coord_quickmap(xlim = c(-173.9, -167.8), ylim = c(51.8, 53.5))
wag_proj <- coord_quickmap(xlim = c(-189.5, -173.9), ylim = c(51, 55))

# core data prep ----

obs_raw %>%
  as_tibble() %>%
  # add time period
  mutate(period = ifelse(crab_year < 2005, "pre_rat", "post_rat")) %>%
  # nest by eag / wag
  nest_by(subdistrict, period, .keep = T) %>% ungroup %>% # pull(data) %>% .[[4]] -> data
  # do core data filtering
  mutate(core = purrr::map(data, function(data) {
    
    data %>%
      # add block
      f_add_blocks() %>%
      # remove missing key data
      filter(!is.na(crab_year),
             crab_year >= 1995,
             !is.na(sampdate),
             !is.na(permit_holder),
             !is.na(adfg),
             !is.na(soaktime),
             !is.na(depth),
             !is.na(gearcode),
             gearcode != "NA",
             !is.na(block), block != -9,
             !is.na(latitude), !is.na(longitude)) %>%
      # remove odd gear types, combine others
      filter(!(gearcode %in% c(1:3, 14:23, 80, 81))) %>%
      mutate(gearcode = ifelse(gearcode == 9, 5,
                        ifelse(gearcode == 10, 6, 
                        ifelse(gearcode == 11, 7, gearcode))),
             soaktime = ifelse(crab_year %in% 1995:1996, soaktime * 24, soaktime)) %>%
      # filter for middle quantiles of soaktime and depth by year
      group_by(crab_year) %>%
      filter(soaktime > quantile(soaktime, 0.025), 
             soaktime < quantile(soaktime, 0.975),
             depth > quantile(depth, 0.01), 
             depth < quantile(depth, 0.99)) %>%
      # biotwine is okay
      filter(biotwine_ok != "N") %>% ungroup -> tmp
    
    if(unique(tmp$period) == "pre_rat") {
      tmp %>%
        # reduce number of permit holders and vessel
        # present in more than a single season
        group_by(permit_holder) %>%
        filter(length(unique(crab_year)) > 1) %>%
        ungroup %>%
        group_by(adfg) %>%
        filter(length(unique(crab_year)) > 1) %>%
        ungroup -> tmp
    }
    
    tmp %>%
      # join to season start date
      left_join(season_dates %>% rename(subdistrict = fishery)) %>%
      mutate(start_date = mdy(start_date),
             season_day =  yday(sampdate) - yday(start_date)) %>%
      # get data
      transmute(crab_year = factor(crab_year), adfg = factor(adfg), trip, sampdate = as_date(sampdate),
                month = factor(month(sampdate), levels = c(7:12, 1:6)), 
                yday = yday(sampdate) - yday(mdy(paste0("7/1/", as.numeric(as.character(crab_year))))),
                yday = ifelse(yday < 0, abs(yday) + yday(mdy(paste0("7/1/", as.numeric(as.character(crab_year))))), yday),
                start_date,
                season_day,
                depth, soaktime, gearcode = factor(gearcode), 
                permit_holder = factor(as.numeric(factor(permit_holder))),
                block = factor(block), latitude, longitude, tot_legal,
                tot_male = tot_legal + sublegal) -> tmp
    
    # join to slope data
    if(max(tmp$longitude) >= -174) {
      
      tmp_xy <- transmute(tmp, latitude, longitude)
      
      nearest <- RANN::nn2(eag_slope %>%
                             transmute(latitude = Lat, longitude = Long), tmp_xy, k = 1)
      tmp$slope <- eag_slope$Slope_ai_1[nearest$nn.idx]
      
    }
    
    # join to slope data
    if(max(tmp$longitude) < -174) {
      
      tmp_xy <- transmute(tmp, latitude, longitude)
      
      nearest <- RANN::nn2(wag_slope %>%
                             transmute(latitude = Lat, longitude = Long), tmp_xy, k = 1)
      tmp$slope <- wag_slope$Slope_ai_1[nearest$nn.idx]
      
    }
    
    return(tmp)  
    
  })) -> obs_core
  


# plot of core data cpue ----

# siddek core data
file <- c("./AIGKC/data/observer/pre_eag_datacore_siddeek.RDS", 
          "./AIGKC/data/observer/post_eag_datacore_siddeek.RDS", 
          "./AIGKC/data/observer/pre_wag_datacore_siddeek.RDS", 
          "./AIGKC/data/observer/post_wag_datacore_siddeek.RDS")
tibble(period = c("pre_rat", "post_rat", "pre_rat", "post_rat"),
       subdistrict = c(rep("EAG", 2), rep("WAG", 2)),
       file = file) %>%
  mutate(cpue = purrr::map(file, function(file){
    readRDS(file) %>%
      rename_all(tolower) %>%
      rename(crab_year = year) %>%
      group_by(crab_year) %>%
      summarise(nom = mean(legals))
  })) %>%
  transmute(period, subdistrict, cpue) %>%
  unnest(cpue) %>%
  mutate(source = "Siddeek et al. (2023)") %>%
  # current analysis core data
  bind_rows(obs_core %>%
              transmute(period, subdistrict, core) %>%
              unnest(core) %>%
              group_by(period, subdistrict, crab_year) %>%
              summarise(nom = mean(tot_legal)) %>% ungroup %>%
              mutate(source = "Current Analysis")) %>%
  mutate(crab_year = as.numeric(as.character(crab_year))) %>%
  
  ggplot()+
  geom_point(aes(x = crab_year, y = nom, color = source))+
  geom_line(aes(x = crab_year, y = nom, group = source, color = source))+
  facet_wrap(~subdistrict, nrow = 2, scales = "free_y")+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  labs(x = NULL, y = "Nominal CPUE (crab / pot)", color = NULL)+
  theme(legend.justification = c(0, 1),
        legend.position = c(0,1)) -> p
ggsave("./AIGKC/figures/cpue_std/2024/jan/nominal_cpue.png",
       plot = p,
       height = 5, width = 6, units = "in")


# plot of data, to see excess zeros ----

obs_core %>%
  transmute(subdistrict, period, core) %>%
  unnest(core) %>% 
  mutate(period = ifelse(period == "pre_rat", "Pre Rationalization", "Post Rationalization"),
         period = factor(period, levels = c("Pre Rationalization", "Post Rationalization"))) %>% 
  
  ggplot()+
  geom_histogram(aes(x = tot_legal, y = after_stat(density)), binwidth = 1)+
  facet_grid(period ~ subdistrict, scales = "free")

# pre-rationalized eag ----

obs_core %>%
  filter(subdistrict == "EAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core) -> pre_eag


# fit
# f_step_gam(null = bam(tot_legal ~ crab_year,
#                       family = nb, data = pre_eag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> pre_eag_std
# saveRDS(pre_eag_std, "./AIGKC/output/cpue_std/2024/jan/pre_eag_std.RDS")
pre_eag_std <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_eag_std.RDS")[[1]]


# effect plots and diagnostics
pre_eag_viz <- getViz(pre_eag_std)
pre_eag_diag <- check.gamViz(pre_eag_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_eag_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_eag_diag
dev.off()

# check for excess zeros
pred <- predict(pre_eag_std, type = "response") 
prop0 <- dnbinom(x = 0, mu = pred, size = pre_eag_std$family$getTheta(trans = T))
sum(prop0)
sum(pre_eag$tot_legal == 0)

# permit holder
plot(pterm(pre_eag_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
        labs(x = "Permit Holder", y = "f(Permit Holder)")+
        theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_eag_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# month
plot(pterm(pre_eag_viz, select = 4))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Month", y = "f(Month)") -> mo
# soaktime
plot(sm(pre_eag_viz, select = 1)) + 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)") -> st
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_effects.png",
       plot = gridPrint(ph, gc, mo, st, ncol = 2),
       height = 6, width = 8, units = "in")

# lon x lat
plot(sm(pre_eag_viz, select = 2)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  eag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")

# extract cpue index
loc <- grep("year", names(coef(pre_eag_std)))
yrs <- unique(pre_eag$crab_year)
pre_eag_index <- f_getCPUE_gam(pre_eag_std, loc, yrs)
write_csv(pre_eag_index, "./AIGKC/output/cpue_std/2024/jan/pre_eag_index.csv")

# pre-rationalized eag, ti ----

# add tensor interaction
pre_eag_std_ti <- update(pre_eag_std, ~. + ti(longitude, latitude, by = crab_year))
saveRDS(pre_eag_std_ti, "./AIGKC/output/cpue_std/2024/jan/pre_eag_std_ti.RDS")
pre_eag_std_ti <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_eag_std_ti.RDS")

# effect plots and diagnostics
pre_eag_std_ti_viz <- getViz(pre_eag_std_ti)
pre_eag_std_ti_diag <- check.gamViz(pre_eag_std_ti_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_eag_std_ti_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_eag_std_ti_diag
dev.off()

# lat/lon
tibble(select = 3:12) %>%
  mutate(crab_year = unique(pre_eag$crab_year),
         fit = purrr::map(select, function(x){tmp <- plot(sm(pre_eag_std_ti_viz, select = x)); tmp$data$fit})) %>%
  unnest(fit) -> ti_fit
tibble(select = 3:12) %>%
  mutate(crab_year = unique(pre_eag$crab_year),
         fit = purrr::map(select, function(x){tmp <- plot(sm(pre_eag_std_ti_viz, select = x)); tmp$data$res})) %>%
  unnest(fit) -> ti_res

ggplot2::ggplot()+
  geom_raster(data = ti_fit, aes(x, y, fill = z), )+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "te(Lon:Lat)")+
  #geom_point(data = ti_res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  facet_wrap(~crab_year, ncol = 2, dir = "v")+
  eag_proj-> ti_p
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_effects_ti.png",
       plot = ti_p,
       height = 8, width = 8, units = "in")


summary(pre_eag_std_ti)
# check r2 improvement
((pre_eag_std_ti$null.deviance - pre_eag_std_ti$deviance) / pre_eag_std_ti$null.deviance) - ((pre_eag_std$null.deviance - pre_eag_std$deviance) / pre_eag_std$null.deviance)
# check AIC improvement
(AIC(pre_eag_std, k = log(nrow(pre_eag_std$model)) + 1) - AIC(pre_eag_std_ti, k = log(nrow(pre_eag_std_ti$model)) + 1)) > (2 * (pre_eag_std$df.residual - pre_eag_std_ti$df.residual))

# not a significant improvement

# prediction set
pre_eag_std_ti$model %>%
  as_tibble() %>%
  transmute(longitude, latitude) %>%
  distinct %>%
  mutate(gearcode = names(sort(-table(pre_eag_std_ti$model$gearcode)))[1],
         permit_holder = names(sort(-table(pre_eag_std_ti$model$permit_holder)))[1],
         month = names(sort(-table(pre_eag_std_ti$model$month)))[1],
         soaktime = mean(pre_eag_std_ti$model$soaktime)) %>%
  expand_grid(., crab_year = unique(pre_eag_std_ti$model$crab_year)) %>%
  # get predicted values
  mutate(fit = predict(pre_eag_std_ti, newdata = ., type = "response", se.fit = F),
         se = predict(pre_eag_std_ti, newdata = ., type = "response", se.fit = T)$se.fit) %>%
  # average predicted values by year
  group_by(crab_year) %>% 
  summarise(std = mean(fit),
            std_se = sqrt(sum(se^2) / (n())^2)) %>%
  # scale
  mutate(index_ps = std / prod(std) ^ (1 / 10)) %>%
  # join to index extracted from year effects to get se
  rename(year = crab_year) %>%
  left_join(f_getCPUE_gam(pre_eag_std_ti, loc, yrs)) -> pre_eag_index_tiyr
write_csv(pre_eag_index_tiyr, "./AIGKC/output/cpue_std/2024/jan/pre_eag_index_tiyr.csv")


# pre-rationalized eag, yr:block ----

# remove block 1 in 1995 due to small sample size (n = 1)
obs_core %>%
  filter(subdistrict == "EAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core) %>%
  filter(!(crab_year == 1995 & block == 1)) -> pre_eag

# fit
# f_step_gam(null = bam(tot_legal ~ crab_year:block,
#                       family = nb, data = pre_eag),
#            full_scope = list(~(crab_year:block + s(soaktime) + month + adfg + permit_holder + gearcode + s(depth) + s(slope)))) -> pre_eag_std_yrb
# saveRDS(pre_eag_std_yrb, "./AIGKC/output/cpue_std/2024/jan/pre_eag_std_yrb.RDS")
pre_eag_std_yrb <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_eag_std_yrb.RDS")[[1]]

# effect plots and diagnostics
pre_eag_yrb_viz <- getViz(pre_eag_std_yrb)
pre_eag_yrb_diag <- check.gamViz(pre_eag_yrb_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_eag_yrb_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_eag_yrb_diag
dev.off()

# permit holder
plot(pterm(pre_eag_yrb_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_eag_yrb_viz, select = 1))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# soaktime
plot(sm(pre_eag_yrb_viz, select = 1)) + 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)") -> st
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_yrb_effects.png",
       plot = gridPrint(ph, gc, st, ncol = 2),
       height = 6, width = 8, units = "in")

# plot yr:block effect (must use visreg to get data)
yrb <- visreg(pre_eag_std_yrb, xvar = c("crab_year"), by = "block", type = "contrast")
ggplot()+
  geom_jitter(data = yrb$res, aes(x = as.numeric(as.character(crab_year)), y = visregRes), color = "grey70", alpha = 0.5)+
  geom_point(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), y = visregFit))+
  geom_errorbar(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), ymin = visregLwr, ymax = visregUpr), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~paste0("Block ", block), nrow = 1)+
  labs(x = NULL, y = "s(Year)") -> yrbgg
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_yrb_effects_yrb.png",
       plot = yrbgg,
       height = 3, width = 8, units = "in")

# extract cpue index
pre_eag_yrb_index <- f_getCPUEyrb_gam(pre_eag_std_yrb)
write_csv(pre_eag_yrb_index, "./AIGKC/output/cpue_std/2024/jan/pre_eag_index_yrb.csv")

# pre-rationalized eag, hurdle ----

obs_core %>%
  filter(subdistrict == "EAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core) -> pre_eag



f_step_gam(null = glm(tot_legal > 0 ~ crab_year,
                      family = binomial, data = pre_eag),
           full_scope = list(~(crab_year + ns(soaktime,3) + month + adfg + permit_holder +
                                 block + gearcode + ns(depth,3) + ns(slope,3))))

f_step_gam(null = glmmTMB(tot_legal ~ crab_year,
                      family = truncated_nbinom1, data = pre_eag %>% filter(tot_legal > 0)),
           full_scope = list(~(crab_year + ns(soaktime,3) + month + adfg + permit_holder +
                                 block + gearcode + ns(depth,3) + ns(slope,3))))


# fit
f_step_gam(null = bam(tot_legal > 0 ~ crab_year,
                      family = "binomial", data = pre_eag),
           full_scope = list(~(crab_year + ns(soaktime,3) + month + adfg + permit_holder +
                                 block + gearcode + ns(depth,3) + ns(slope,3)))) -> pre_eag_hurdle_pres
saveRDS(pre_eag_hurdle_pres, "./AIGKC/output/cpue_std/2024/jan/pre_eag_hurdle_pres.RDS")
pre_eag_hurdle_pres <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_eag_hurdle_pres.RDS")


f_step_gam(null = bam(tot_legal ~ crab_year,
                      family = nb, data = pre_eag, subset = tot_legal > 0),
           full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
                                 block + gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> pre_eag_hurdle_pos
summary(pre_eag_hurdle_pos)



# post-rationalized eag ----

obs_core %>%
  filter(subdistrict == "EAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) -> post_eag


# fit
# f_step_gam(null = gam(tot_legal ~ crab_year,
#                       family = nb, data = post_eag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> post_eag_std
# saveRDS(post_eag_std, "./AIGKC/output/cpue_std/2024/jan/post_eag_std.RDS")
post_eag_std <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_eag_std.RDS")[[1]]


# effect plots and diagnostics
post_eag_viz <- getViz(post_eag_std)
post_eag_diag <- check.gamViz(post_eag_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_eag_diag.png", height = 6, width = 7, units = "in", res = 300)
post_eag_diag
dev.off()

# check for excess zeros
pred <- predict(post_eag_std, type = "response") 
prop0 <- dnbinom(x = 0, mu = pred, size = post_eag_std$family$getTheta(trans = T))
sum(prop0)
sum(post_eag$tot_legal == 0)


# permit holder
plot(pterm(post_eag_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(post_eag_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# soaktime
plot(sm(post_eag_viz, select = 1)) + 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)") -> st


ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_effects.png",
       plot = gridPrint(ph, gc, st, ncol = 2),
       height = 6, width = 8, units = "in")


# extract cpue index
loc <- grep("year", names(coef(post_eag_std)))
yrs <- unique(post_eag$crab_year)
post_eag_index <- f_getCPUE_gam(post_eag_std, loc, yrs)
write_csv(post_eag_index, "./AIGKC/output/cpue_std/2024/jan/post_eag_index.csv")


# post-rationalized eag, ti ----

# add tensor interaction
post_eag_std_ti <- update(post_eag_std, ~. + s(longitude, latitude) + ti(longitude, latitude))
saveRDS(post_eag_std_ti, "./AIGKC/output/cpue_std/2024/jan/post_eag_std_ti.RDS")
post_eag_std_ti <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_eag_std_ti.RDS")

# effect plots and diagnostics
post_eag_std_ti_viz <- getViz(post_eag_std_ti)
post_eag_std_ti_diag <- check.gamViz(post_eag_std_ti_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_eag_std_ti_diag.png", height = 6, width = 7, units = "in", res = 300)
post_eag_std_ti_diag
dev.off()

# lat/lon
plot(sm(post_eag_std_ti_viz, select = 3)) -> ti
ggplot2::ggplot()+
  geom_raster(data = ti$data$fit, aes(x, y, fill = z), )+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "ti(Lon:Lat)")+
  #geom_point(data = te$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  eag_proj -> ti_p
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_effects_ti.png",
       plot = ti_p,
       height = 3, width = 6, units = "in")

# check r2 improvement
((post_eag_std_ti$null.deviance - post_eag_std_ti$deviance) / post_eag_std_ti$null.deviance) - ((post_eag_std$null.deviance - post_eag_std$deviance) / post_eag_std$null.deviance)
# check AIC improvement
(AIC(post_eag_std, k = log(nrow(post_eag_std$model)) + 1) - AIC(post_eag_std_ti, k = log(nrow(post_eag_std_ti$model)) + 1)) > (2 * (post_eag_std$df.residual - post_eag_std_ti$df.residual))

# extract cpue index
loc <- grep("year", names(coef(post_eag_std_ti)))
yrs <- unique(post_eag$crab_year)
post_eag_index_ti <- f_getCPUE_gam(post_eag_std_ti, loc, yrs)
write_csv(post_eag_index_ti, "./AIGKC/output/cpue_std/2024/jan/post_eag_index_ti.csv")

# post-rationalized eag, yr:block ----

# remove block 1 in 1995 due to small sample size (n = 1)
obs_core %>%
  filter(subdistrict == "EAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) %>% 
  filter(block != 1) -> post_eag

# fit
# f_step_gam(null = bam(tot_legal ~ crab_year:block,
#                       family = nb, data = post_eag),
#            full_scope = list(~(crab_year:block + s(soaktime) + month + adfg + permit_holder + gearcode + s(depth) + s(slope)))) -> post_eag_std_yrb
# 
# saveRDS(post_eag_std_yrb, "./AIGKC/output/cpue_std/2024/jan/post_eag_std_yrb.RDS")
post_eag_std_yrb <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_eag_std_yrb.RDS")[[1]]

 # effect plots and diagnostics
post_eag_yrb_viz <- getViz(post_eag_std_yrb)
post_eag_yrb_diag <- check.gamViz(post_eag_yrb_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_eag_yrb_diag.png", height = 6, width = 7, units = "in", res = 300)
post_eag_yrb_diag
dev.off()

# vessel
plot(pterm(post_eag_yrb_viz, select = 1))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> adfg
# gearcode
plot(pterm(post_eag_yrb_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc

ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_yrb_effects.png",
       plot = gridPrint(adfg, gc, ncol = 1),
       height = 6, width = 8, units = "in")

# plot yr:block effect (must use visreg to get data)
yrb <- visreg(post_eag_std_yrb, xvar = c("crab_year"), by = "block", type = "contrast")
ggplot()+
  geom_jitter(data = yrb$res, aes(x = as.numeric(as.character(crab_year)), y = visregRes), color = "grey70", alpha = 0.5)+
  geom_point(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), y = visregFit))+
  geom_errorbar(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), ymin = visregLwr, ymax = visregUpr), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~paste0("Block ", block), nrow = 1)+
  labs(x = NULL, y = "s(Year)") -> yrb
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_yrb_effects_yrb.png",
       plot = yrb,
       height = 3, width = 8, units = "in")

# extract cpue index

post_eag_yrb_index <- f_getCPUEyrb_gam(post_eag_std_yrb)
write_csv(post_eag_yrb_index, "./AIGKC/output/cpue_std/2024/jan/post_eag_index_yrb.csv")


# pre-rationalized eag, tot_male ----

obs_core %>%
  filter(subdistrict == "EAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core) -> pre_eag


# fit
# f_step_gam(null = bam(tot_male ~ crab_year,
#                       family = nb, data = pre_eag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                    gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> pre_eag_std_tm
# saveRDS(pre_eag_std_tm, "./AIGKC/output/cpue_std/2024/jan/pre_eag_std_tm.RDS")
pre_eag_std_tm <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_eag_std_tm.RDS")[[1]]

# effect plots and diagnostics
pre_eag_tm_viz <- getViz(pre_eag_std_tm)
pre_eag_tm_diag <- check.gamViz(pre_eag_tm_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_eag_tm_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_eag_tm_diag
dev.off()

# check for excess zeros
pred <- predict(pre_eag_std_tm, type = "response") 
prop0 <- dnbinom(x = 0, mu = pred, size = pre_eag_std_tm$family$getTheta(trans = T))
sum(prop0)
sum(pre_eag$tot_male == 0)

# permit holder
plot(pterm(pre_eag_tm_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_eag_tm_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_tm_effects.png",
       plot = gridPrint(ph, gc, ncol = 1),
       height = 6, width = 6, units = "in")

# lon x lat
plot(sm(pre_eag_tm_viz, select = 1)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  eag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_eag_tm_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")

# extract cpue index
loc <- grep("year", names(coef(pre_eag_std_tm)))
yrs <- unique(pre_eag$crab_year)
pre_eag_tm_index <- f_getCPUE_gam(pre_eag_std_tm, loc, yrs)
write_csv(pre_eag_tm_index, "./AIGKC/output/cpue_std/2024/jan/pre_eag_tm_index.csv")

# post-rationalized eag, tot_male ----

obs_core %>%
  filter(subdistrict == "EAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) -> post_eag

# fit
# f_step_gam(null = bam(tot_male ~ crab_year,
#                       family = nb, data = post_eag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                   gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> post_eag_std_tm
# saveRDS(post_eag_std_tm, "./AIGKC/output/cpue_std/2024/jan/post_eag_std_tm.RDS")
post_eag_std_tm <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_eag_std_tm.RDS")[[1]]

# effect plots and diagnostics
post_eag_tm_viz <- getViz(post_eag_std_tm)
post_eag_tm_diag <- check.gamViz(post_eag_tm_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_eag_tm_diag.png", height = 6, width = 7, units = "in", res = 300)
post_eag_tm_diag
dev.off()

# month
plot(pterm(post_eag_tm_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Month", y = "f(Month)") -> mo
# vessel
plot(pterm(post_eag_tm_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> vs
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_tm_effects.png",
       plot = gridPrint(mo, vs, ncol = 1),
       height = 4, width = 6, units = "in")

# lon x lat
plot(sm(post_eag_tm_viz, select = 1)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  eag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/post_eag_tm_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")

# extract cpue index
loc <- grep("year", names(coef(post_eag_std_tm)))
yrs <- unique(post_eag$crab_year)
post_eag_tm_index <- f_getCPUE_gam(post_eag_std_tm, loc, yrs)
write_csv(post_eag_tm_index, "./AIGKC/output/cpue_std/2024/jan/post_eag_tm_index.csv")


# eag timeseries plot ----

## legal male time series
# siddeek's glm index
matrix(scan("./AIGKC/models/2024/jan/EAG/23.0/EAG_23_0.dat",
            skip = 260, nlines = length(261:288)), 
       nrow = length(261:288), ncol = 10, byrow = T) %>%
  as.data.frame %>%
  rename_all(~c("which", "year", "season", "fleet", "sex", 
                "maturity", "index", "cv", "unit", "timing")) %>%
  transmute(year, index, cv, period = ifelse(year < 2005, "pre", "post"),
            type = "Legal GLM") %>%
  
  # add nominal cpue
  bind_rows(pre_eag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_legal)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "pre",
                     type = "Legal Nominal")) %>%
  bind_rows(post_eag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_legal)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "post",
                     type = "Legal Nominal")) %>%
  
  # join to standardized indices
  ## no yrb
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_eag_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_eag_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal GAM")) %>%
  ## year:block
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_eag_index_yrb.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_eag_index_yrb.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal GAM yr:block")) %>%
  mutate(type = factor(type, levels = c("Legal GLM", "Legal GAM", "Legal GAM yr:block", "Legal Nominal"))) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type, linetype = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type, linetype = type))+
  #geom_line(aes(x = year, y = index, color = type, linetype = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1:3], "grey40"))+
  scale_linetype_manual(values = c(1, 1, 1, 2))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> eag_timserseries_legal
ggsave("./AIGKC/figures/cpue_std/2024/jan/eag_std_cpue_timeseries_legal.png",
       plot = eag_timserseries_legal,
       height = 4, width = 7, units = "in")



## total male time series
pre_eag %>%
  mutate(year = as.numeric(as.character(crab_year))) %>%
  group_by(year) %>%
  summarise(index = mean(tot_male)) %>% ungroup %>%
  mutate(index = index / (prod(index)^(1/n())),
         period = "pre",
         type = "Total Nominal") %>%
  bind_rows(post_eag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_male)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "post",
                     type = "Total Nominal")) %>% 
  
  # join to standardized indices
  ## no yrb, total male
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_eag_tm_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_eag_tm_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Total GAM")) %>%
  mutate(type = factor(type, levels = c("Total GAM", "Total Nominal"))) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type, linetype = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type, linetype = type))+
  #geom_line(aes(x = year, y = index, color = type, linetype = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1], "grey40"))+
  scale_linetype_manual(values = c(1, 2))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> eag_timserseries_total
ggsave("./AIGKC/figures/cpue_std/2024/jan/eag_std_cpue_timeseries_total.png",
       plot = eag_timserseries_total,
       height = 4, width = 7, units = "in")


# total cpue
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_eag_tm_index.csv") %>% mutate(period = "pre"),
                    read_csv("./AIGKC/output/cpue_std/2024/jan/post_eag_tm_index.csv") %>% mutate(period = "post")) %>%
            mutate(type = "Total CPUE")) %>%
  # legal CPUE
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_eag_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_eag_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal CPUE")) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1:2]))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> eag_timserseries
ggsave("./AIGKC/figures/cpue_std/2024/jan/eag_std_cpue_timeseries.png",
       plot = eag_timserseries,
       height = 4, width = 7, units = "in")


# pre-rationalized wag ----

obs_core %>%
  filter(subdistrict == "WAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core)  -> pre_wag

# fit
# f_step_gam(null = bam(tot_legal ~ crab_year,
#                       family = nb, data = pre_wag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder + 
#                                  gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> pre_wag_std
# saveRDS(pre_wag_std, "./AIGKC/output/cpue_std/2024/jan/pre_wag_std.RDS")
pre_wag_std <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_wag_std.RDS")[[1]]

# effect plots and diagnostics
pre_wag_viz <- getViz(pre_wag_std)
pre_wag_diag <- check.gamViz(pre_wag_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_wag_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_wag_diag
dev.off()

# permit holder
plot(pterm(pre_wag_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_wag_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# soaktime
plot(sm(pre_wag_viz, select = 1)) + 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)") -> st

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_effects.png",
       plot = gridPrint(ph, gc, st, ncol = 2),
       height = 6, width = 8, units = "in")


# lon x lat
plot(sm(pre_wag_viz, select = 2)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  wag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")


# extract cpue index
loc <- grep("year", names(coef(pre_wag_std)))
yrs <- unique(pre_wag$crab_year)
pre_wag_index <- f_getCPUE_gam(pre_wag_std, loc, yrs)
write_csv(pre_wag_index, "./AIGKC/output/cpue_std/2024/jan/pre_wag_index.csv")


# pre-rationalized wag, ti ----

# add tensor interaction
pre_wag_std_ti <- update(pre_wag_std, ~. + ti(longitude, latitude, by = crab_year))
saveRDS(pre_wag_std_ti, "./AIGKC/output/cpue_std/2024/jan/pre_wag_std_ti.RDS")
pre_wag_std_ti <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_wag_std_ti.RDS")

# effect plots and diagnostics
pre_wag_std_ti_viz <- getViz(pre_wag_std_ti)
pre_wag_std_ti_diag <- check.gamViz(pre_wag_std_ti_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_wag_std_ti_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_wag_std_ti_diag
dev.off()

# lat/lon
tibble(select = 3:12) %>%
  mutate(crab_year = unique(pre_wag$crab_year),
         fit = purrr::map(select, function(x){tmp <- plot(sm(pre_wag_std_ti_viz, select = x)); tmp$data$fit})) %>%
  unnest(fit) -> ti_fit
tibble(select = 3:12) %>%
  mutate(crab_year = unique(pre_wag$crab_year),
         fit = purrr::map(select, function(x){tmp <- plot(sm(pre_wag_std_ti_viz, select = x)); tmp$data$res})) %>%
  unnest(fit) -> ti_res

ggplot2::ggplot()+
  geom_raster(data = ti_fit, aes(x, y, fill = z), )+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "te(Lon:Lat)")+
  #geom_point(data = ti_res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30")+
  facet_wrap(~crab_year, ncol = 2, dir = "v")+
  wag_proj -> ti_p
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_effects_ti.png",
       plot = ti_p,
       height = 8, width = 8, units = "in")


summary(pre_wag_std_ti)
# check r2 improvement
((pre_wag_std_ti$null.deviance - pre_wag_std_ti$deviance) / pre_wag_std_ti$null.deviance) - ((pre_wag_std$null.deviance - pre_wag_std$deviance) / pre_wag_std$null.deviance)
# check AIC improvement
(AIC(pre_wag_std, k = log(nrow(pre_wag_std$model)) + 1) - AIC(pre_wag_std_ti, k = log(nrow(pre_wag_std_ti$model)) + 1)) > (2 * (pre_wag_std$df.residual - pre_wag_std_ti$df.residual))

# tensor interaction is not a significant improvement


# pre-rationalized wag, yr:block ----

obs_core %>%
  filter(subdistrict == "WAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core)  -> pre_wag

# fit
# f_step_gam(null = bam(tot_legal ~ crab_year:block,
#                       family = nb, data = pre_wag),
#            full_scope = list(~(crab_year:block + s(soaktime) + month + adfg + permit_holder + gearcode + s(depth) + s(slope)))) -> pre_wag_std_yrb
# saveRDS(pre_wag_std_yrb, "./AIGKC/output/cpue_std/2024/jan/pre_wag_std_yrb.RDS")
pre_wag_std_yrb <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_wag_std_yrb.RDS")[[1]]

# effect plots and diagnostics
pre_wag_yrb_viz <- getViz(pre_wag_std_yrb)
pre_wag_yrb_diag <- check.gamViz(pre_wag_yrb_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_wag_yrb_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_wag_yrb_diag
dev.off()

# permit holder
plot(pterm(pre_wag_yrb_viz, select = 1))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_wag_yrb_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# soaktime
plot(sm(pre_wag_yrb_viz, select = 1)) + 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitLine()+
  l_ciLine()+
  labs(x = "Soak Time (hr)", y = "f(Soak Time)") -> st
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_yrb_effects.png",
       plot = gridPrint(ph, gc, st, ncol = 2),
       height = 6, width = 8, units = "in")


# plot yr:block effect (must use visreg to get data)
yrb <- visreg(pre_wag_std_yrb, xvar = c("crab_year"), by = "block", type = "contrast")
ggplot()+
  geom_jitter(data = yrb$res, aes(x = as.numeric(as.character(crab_year)), y = visregRes), color = "grey70", alpha = 0.5)+
  geom_point(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), y = visregFit))+
  geom_errorbar(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), ymin = visregLwr, ymax = visregUpr), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~paste0("Block ", block), nrow = 1)+
  labs(x = NULL, y = "s(Year)") -> yrb
ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_yrb_effects_yrb.png",
       plot = yrb,
       height = 3, width = 8, units = "in")


# extract cpue index
pre_wag_yrb_index <- f_getCPUEyrb_gam(pre_wag_std_yrb)
write_csv(pre_wag_yrb_index, "./AIGKC/output/cpue_std/2024/jan/pre_wag_index_yrb.csv")


# post-rationalized wag ----

obs_core %>%
  filter(subdistrict == "WAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) -> post_wag


# fit
# f_step_gam(null = gam(tot_legal ~ crab_year,
#                       family = nb, data = post_wag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                    gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> post_wag_std
# saveRDS(post_wag_std, "./AIGKC/output/cpue_std/2024/jan/post_wag_std.RDS")
post_wag_std <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_wag_std.RDS")[[1]]


# effect plots and diagnostics
post_wag_viz <- getViz(post_wag_std)
post_wag_diag <- check.gamViz(post_wag_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_wag_diag.png", height = 6, width = 7, units = "in", res = 300)
post_wag_diag
dev.off()

# check for excess zeros
pred <- predict(post_wag_std, type = "response") 
prop0 <- rnbinom(n = nrow(pre_wag), mu = pred, size = post_wag_std$family$getTheta(trans = T))
sum(prop0)
sum(post_wag$tot_legal == 0)


# permit holder
plot(pterm(post_wag_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# permit holder
plot(pterm(post_wag_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear", y = "f(Gear)")+
  theme(axis.text.x = element_blank()) -> gc

ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_effects.png",
       plot = gridPrint(ph, gc, ncol = 1),
       height = 8, width = 6, units = "in")

# lon x lat
plot(sm(post_wag_viz, select = 1)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30")+
  wag_proj -> lonlat_p
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 6, units = "in")


# extract cpue index
loc <- grep("year", names(coef(post_wag_std)))
yrs <- unique(post_wag$crab_year)
post_wag_index <- f_getCPUE_gam(post_wag_std, loc, yrs)
write_csv(post_wag_index, "./AIGKC/output/cpue_std/2024/jan/post_wag_index.csv")


# post-rationalized wag, ti ----

# add tensor interaction
post_wag_std_ti <- update(post_wag_std, ~. + ti(longitude, latitude))
saveRDS(post_wag_std_ti, "./AIGKC/output/cpue_std/2024/jan/post_wag_std_ti.RDS")
post_wag_std_ti <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_wag_std_ti.RDS")

# effect plots and diagnostics
post_wag_std_ti_viz <- getViz(post_wag_std_ti)
post_wag_std_ti_diag <- check.gamViz(post_wag_std_ti_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_wag_std_ti_diag.png", height = 6, width = 7, units = "in", res = 300)
post_wag_std_ti_diag
dev.off()

# lat/lon
plot(sm(post_wag_std_ti_viz, select = 2)) -> ti
ggplot2::ggplot()+
  geom_raster(data = ti$data$fit, aes(x, y, fill = z), )+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "ti(Lon:Lat)")+
  #geom_point(data = te$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  wag_proj -> ti_p
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_effects_ti.png",
       plot = ti_p,
       height = 3, width = 6, units = "in")

# check r2 improvement
((post_wag_std_ti$null.deviance - post_wag_std_ti$deviance) / post_wag_std_ti$null.deviance) - ((post_wag_std$null.deviance - post_wag_std$deviance) / post_wag_std$null.deviance)
# check AIC improvement
(AIC(post_wag_std, k = log(nrow(post_wag_std$model)) + 1) - AIC(post_wag_std_ti, k = log(nrow(post_wag_std_ti$model)) + 1)) > (2 * (post_wag_std$df.residual - post_wag_std_ti$df.residual))

# extract cpue index
loc <- grep("year", names(coef(post_wag_std_ti)))
yrs <- unique(post_wag$crab_year)
post_wag_index_ti <- f_getCPUE_gam(post_wag_std_ti, loc, yrs)
write_csv(post_wag_index_ti, "./AIGKC/output/cpue_std/2024/jan/post_wag_index_ti.csv")

# post-rationalized wag, yr:block ----

obs_core %>%
  filter(subdistrict == "WAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) %>%
  group_by(block, crab_year, gearcode, adfg, permit_holder) %>% 
  add_count() %>%
  filter(n >= 10,
         block != 10) -> post_wag

# fit
f_step_gam(null = bam(tot_legal ~ crab_year:block,
                      family = nb, data = post_wag),
           full_scope = list(~(crab_year:block + s(soaktime) + month + adfg + permit_holder + gearcode + s(depth) + s(slope)))) -> post_wag_std_yrb
saveRDS(post_wag_std_yrb, "./AIGKC/output/cpue_std/2024/jan/post_wag_std_yrb.RDS")
post_wag_std_yrb <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_wag_std_yrb.RDS")[[1]]

# effect plots and diagnostics
post_wag_yrb_viz <- getViz(post_wag_std_yrb)
post_wag_yrb_diag <- check.gamViz(post_wag_yrb_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_wag_yrb_diag.png", height = 6, width = 7, units = "in", res = 300)
post_wag_yrb_diag
dev.off()

# gearcode
plot(pterm(post_wag_yrb_viz, select = 1))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc

ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_yrb_effects.png",
       plot = gridPrint(gc, ncol = 1),
       height = 4, width = 8, units = "in")

# plot yr:block effect (must use visreg to get data)
yrb <- visreg(post_wag_std_yrb, xvar = c("crab_year"), by = "block", type = "contrast")
ggplot()+
  geom_jitter(data = yrb$res, aes(x = as.numeric(as.character(crab_year)), y = visregRes), color = "grey70", alpha = 0.5)+
  geom_point(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), y = visregFit))+
  geom_errorbar(data = yrb$fit, aes(x = as.numeric(as.character(crab_year)), ymin = visregLwr, ymax = visregUpr), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  facet_wrap(~paste0("Block ", block), nrow = 1)+
  labs(x = NULL, y = "s(Year)") -> yrb
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_yrb_effects_yrb.png",
       plot = yrb,
       height = 3, width = 8, units = "in")

# extract cpue index
post_wag_yrb_index <- f_getCPUEyrb_gam(post_wag_std_yrb)
write_csv(post_wag_yrb_index, "./AIGKC/output/cpue_std/2024/jan/post_wag_index_yrb.csv")


# pre-rationalized wag, tot_male ----

obs_core %>%
  filter(subdistrict == "WAG", period == "pre_rat") %>%
  transmute(core) %>%
  unnest(core) -> pre_wag


# fit
# f_step_gam(null = bam(tot_male ~ crab_year,
#                       family = nb, data = pre_wag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                    gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> pre_wag_std_tm
# saveRDS(pre_wag_std_tm, "./AIGKC/output/cpue_std/2024/jan/pre_wag_std_tm.RDS")
pre_wag_std_tm <- readRDS("./AIGKC/output/cpue_std/2024/jan/pre_wag_std_tm.RDS")[[1]]

# effect plots and diagnostics
pre_wag_tm_viz <- getViz(pre_wag_std_tm)
pre_wag_tm_diag <- check.gamViz(pre_wag_tm_viz)
png("./AIGKC/figures/cpue_std/2024/jan/pre_wag_tm_diag.png", height = 6, width = 7, units = "in", res = 300)
pre_wag_tm_diag
dev.off()

# permit holder
plot(pterm(pre_wag_tm_viz, select = 4))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
# gearcode
plot(pterm(pre_wag_tm_viz, select = 3))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Gear Code", y = "f(Gear Code)") -> gc
# vessel
plot(pterm(pre_wag_tm_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Vessel", y = "f(Vessel)")+
  theme(axis.text.x = element_blank()) -> vs

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_tm_effects.png",
       plot = gridPrint(ph, gc, vs, ncol = 1),
       height = 6, width = 6, units = "in")

# lon x lat
plot(sm(pre_wag_tm_viz, select = 1)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  wag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/pre_wag_tm_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")

# extract cpue index
loc <- grep("year", names(coef(pre_wag_std_tm)))
yrs <- unique(pre_wag$crab_year)
pre_wag_tm_index <- f_getCPUE_gam(pre_wag_std_tm, loc, yrs)
write_csv(pre_wag_tm_index, "./AIGKC/output/cpue_std/2024/jan/pre_wag_tm_index.csv")




# post-rationalized wag, tot_male ----

obs_core %>%
  filter(subdistrict == "WAG", period == "post_rat") %>%
  transmute(core) %>%
  unnest(core) -> post_wag

# fit
# f_step_gam(null = bam(tot_male ~ crab_year,
#                       family = nb, data = post_wag),
#            full_scope = list(~(crab_year + s(soaktime) + month + adfg + permit_holder +
#                                    gearcode + s(depth) + s(slope) + s(longitude, latitude)))) -> post_wag_std_tm
# saveRDS(post_wag_std_tm, "./AIGKC/output/cpue_std/2024/jan/post_wag_std_tm.RDS")
post_wag_std_tm <- readRDS("./AIGKC/output/cpue_std/2024/jan/post_wag_std_tm.RDS")[[1]]

# effect plots and diagnostics
post_wag_tm_viz <- getViz(post_wag_std_tm)
post_wag_tm_diag <- check.gamViz(post_wag_tm_viz)
png("./AIGKC/figures/cpue_std/2024/jan/post_wag_tm_diag.png", height = 6, width = 7, units = "in", res = 300)
post_wag_tm_diag
dev.off()

# permit holder
plot(pterm(post_wag_tm_viz, select = 2))+ 
  l_points(color = "grey70", alpha = 0.5)+
  l_fitPoints()+
  l_ciBar(linetype = 1, width = 0)+
  labs(x = "Permit Holder", y = "f(Permit Holder)")+
  theme(axis.text.x = element_blank()) -> ph
ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_tm_effects.png",
       plot = gridPrint(ph, ncol = 1),
       height = 4, width = 6, units = "in")

# lon x lat
plot(sm(post_wag_tm_viz, select = 1)) -> lonlat
ggplot2::ggplot()+
  geom_raster(data = lonlat$data$fit, aes(x, y, fill = z))+
  scale_fill_gradientn(colors = topo.colors(5), na.value = "white")+
  labs(x = "Longitude", y = "Latitude", fill = "s(Lon:Lat)")+
  geom_point(data = lonlat$data$res, aes(x, y), size = 0.2, alpha = 0.2)+
  geom_polygon(data = ai, aes(x = long, y = lat, group = group), fill = "grey80", color = "grey30", linewidth = 0.2)+
  wag_proj -> lonlat_p

ggsave("./AIGKC/figures/cpue_std/2024/jan/post_wag_tm_effects_lonlat.png",
       plot = lonlat_p,
       height = 4, width = 8, units = "in")

# extract cpue index
loc <- grep("year", names(coef(post_wag_std_tm)))
yrs <- unique(post_wag$crab_year)
post_wag_tm_index <- f_getCPUE_gam(post_wag_std_tm, loc, yrs)
write_csv(post_wag_tm_index, "./AIGKC/output/cpue_std/2024/jan/post_wag_tm_index.csv")






# wag timeseries plot ----


## legal male time series
# siddeek's glm index
matrix(scan("./AIGKC/models/2024/jan/WAG/22.1e2_upd/WAG_22_1e2upd.dat",
            skip = 256, nlines = length(257:284)), 
       nrow = length(257:284), ncol = 10, byrow = T) %>%
  as.data.frame %>%
  rename_all(~c("which", "year", "season", "fleet", "sex", 
                "maturity", "index", "cv", "unit", "timing")) %>%
  transmute(year, index, cv, period = ifelse(year < 2005, "pre", "post"),
            type = "Legal GLM") %>%
  
  # add nominal cpue
  bind_rows(pre_wag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_legal)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "pre",
                     type = "Legal Nominal")) %>%
  bind_rows(post_wag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_legal)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "post",
                     type = "Legal Nominal")) %>%
  
  # join to standardized indices
  ## no yrb
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_wag_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_wag_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal GAM")) %>%
  ## year:block
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_wag_index_yrb.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_wag_index_yrb.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal GAM yr:block")) %>%
  mutate(type = factor(type, levels = c("Legal GLM", "Legal GAM", "Legal GAM yr:block", "Legal Nominal"))) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type, linetype = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type, linetype = type))+
  #geom_line(aes(x = year, y = index, color = type, linetype = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1:3], "grey40"))+
  scale_linetype_manual(values = c(1, 1, 1, 2))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> wag_timseseries_legal
ggsave("./AIGKC/figures/cpue_std/2024/jan/wag_std_cpue_timeseries_legal.png",
       plot = wag_timseseries_legal,
       height = 4, width = 7, units = "in")



## total male time series
pre_wag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_male)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "pre",
                     type = "Total Nominal") %>%
  bind_rows(post_wag %>%
              mutate(year = as.numeric(as.character(crab_year))) %>%
              group_by(year) %>%
              summarise(index = mean(tot_male)) %>% ungroup %>%
              mutate(index = index / (prod(index)^(1/n())),
                     period = "post",
                     type = "Total Nominal")) %>% 
  
  # join to standardized indices
  ## no yrb, total male
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_wag_tm_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_wag_tm_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Total GAM")) %>%
  mutate(type = factor(type, levels = c("Total GAM", "Total Nominal"))) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type, linetype = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type, linetype = type))+
  #geom_line(aes(x = year, y = index, color = type, linetype = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1], "grey40"))+
  scale_linetype_manual(values = c(1, 2))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> wag_timserseries_total
ggsave("./AIGKC/figures/cpue_std/2024/jan/wag_std_cpue_timeseries_total.png",
       plot = wag_timserseries_total,
       height = 4, width = 7, units = "in")


# total cpue
bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_wag_tm_index.csv") %>% mutate(period = "pre"),
                    read_csv("./AIGKC/output/cpue_std/2024/jan/post_wag_tm_index.csv") %>% mutate(period = "post")) %>%
            mutate(type = "Total CPUE")) %>%
  # legal CPUE
  bind_rows(bind_rows(read_csv("./AIGKC/output/cpue_std/2024/jan/pre_wag_index.csv") %>% mutate(period = "pre"),
                      read_csv("./AIGKC/output/cpue_std/2024/jan/post_wag_index.csv") %>% mutate(period = "post")) %>%
              mutate(type = "Legal CPUE")) %>%
  
  ggplot()+
  geom_line(data = function(x) filter(x, period == "pre"), aes(x = year, y = index, color = type))+
  geom_line(data = function(x) filter(x, period == "post"), aes(x = year, y = index, color = type))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE Index", color = NULL, linetype = NULL)+
  scale_color_manual(values = c(cb_palette[1:2]))+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> wag_timserseries
ggsave("./AIGKC/figures/cpue_std/2024/jan/wag_std_cpue_timeseries.png",
       plot = eag_timserseries,
       height = 4, width = 7, units = "in")

