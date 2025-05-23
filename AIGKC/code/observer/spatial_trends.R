library(tidyverse)
library(sf)
library(ggpmisc)
library(gmacsr)

theme_set(theme_sleek())

# data ----

# land
ai <- geodata::gadm("United States", level = 1, path = "./AIGKC/data/maps")
st_as_sf(ai) %>%
  st_shift_longitude() %>%
  st_crop(c(xmin = 186, ymin = 51, xmax = 195.27, ymax = 55.5)) %>%
  st_shift_longitude() -> land
# coordinate reference system
ai_crs <- readRDS("./AIGKC/data/maps/ai_crs.RDS")

# stat areas
st_read("./AIGKC/data/maps/adfg_stat_area", "StatAreas") %>%
  transmute(stat_area = STAT_AREA, geometry) -> sa

# pot data
pots <- read_csv("AIGKC/data/observer/2025/linked_potsum_dump.csv")
# meas data
meas <- read_csv("AIGKC/data/observer/2025/linked_meas_dump.csv")
# ft data 
ft <-  read_csv("AIGKC/data/observer/2025/linked_fish_ticket_dump.csv")

# EAG
# district boundary
tibble(lon = c(-174, -164.73, -164.73, -171, -171, -174),
       lat = c(51, 51, 54.6, 54.6, 55.5, 55.5)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ai_crs) %>%
  st_combine() %>%  
  st_cast("POLYGON") %>%
  st_shift_longitude() -> eag
# grid
eag %>%
  st_make_grid(cellsize = c(0.6666, 0.3333)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number()) %>%
  st_shift_longitude() -> eag_grid
# join grid to pot data
pots %>%
  filter(subdistrict == "EAG", crab_year >= 1995) %>%
  mutate(lon = longitude, 
         lat = latitude) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ai_crs) %>%
  st_shift_longitude() %>%
  st_join(eag_grid, .) -> eaggrid

meas %>%
  filter(subdistrict == "EAG", crab_year >= 1995, sex == 1) %>%
  mutate(lon = -longitude, 
         lat = latitude) %>% 
  group_by(crab_year, lon, lat) %>%
  summarise(non_legal = sum(size < 138),
            legal = sum(size >= 138)) %>% ungroup %>% 
  
  
  st_as_sf(coords = c("lon", "lat"), crs = ai_crs) %>%
  st_shift_longitude() %>%
  st_join(eag_grid, .) -> eaggrid_meas


meas %>%
  filter(subdistrict == "EAG") %>%
  mutate(longitude = -longitude) %>%
  # get cell id
  left_join(st_drop_geometry(eaggrid) %>%
              distinct(longitude, latitude, cellid)) %>%
  filter(!is.na(cellid)) %>%
  
  filter(sex == 1, size > 100) %>%
  
  group_by(crab_year) %>%
  mutate(nmeas = n()) %>%
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin)) %>%
  group_by(crab_year,  bin) %>%
  summarise(tp = n() / mean(nmeas)) %>%
  
  filter(crab_year == 2021)-> tot_comp
  

meas %>%
  filter(subdistrict == "EAG") %>%
  mutate(longitude = -longitude) %>%
  # get cell id
  left_join(st_drop_geometry(eaggrid) %>%
              distinct(longitude, latitude, cellid)) %>%
  filter(!is.na(cellid)) %>%
  
  filter(sex == 1, size > 100) %>%
  
  group_by(crab_year, cellid) %>%
  mutate(nmeas = n()) %>%
  mutate(bin = ceiling(size / 5) * 5 - 2,
         bin = ifelse(bin < 103, 103, bin),
         bin = ifelse(bin > 183, 183, bin)) %>%
  group_by(crab_year, cellid, bin) %>%
  summarise(p = n() / mean(nmeas)) %>%
  left_join(meas %>%
              filter(subdistrict == "EAG") %>%
              mutate(longitude = -longitude) %>%
              # get cell id
              left_join(st_drop_geometry(eaggrid) %>%
                          distinct(longitude, latitude, cellid)) %>%
              filter(!is.na(cellid)) %>%
              
              filter(sex == 1, size > 100) %>%
              
              group_by(crab_year) %>%
              mutate(nmeas = n()) %>%
              mutate(bin = ceiling(size / 5) * 5 - 2,
                     bin = ifelse(bin < 103, 103, bin),
                     bin = ifelse(bin > 183, 183, bin)) %>%
              group_by(crab_year,  bin) %>%
              summarise(tp = n() / mean(nmeas))) %>%
  

  
  ggplot()+
  geom_line(aes(x = bin, y = p, color = factor(cellid)))+
  geom_line(aes(x = bin, y = tp), size = 1, color = 1)+
  facet_wrap(~crab_year)

# eag cpue index
read_csv("./AIGKC/output/cpue_std/2025/may/eag_index_full_soakyr_mesh.csv") %>%
  transmute(crab_year = year, index, l95, u95) -> eag_index

# WAG
# district boundary
tibble(lon = c(-174, -174, -190, -190),
       lat = c(51, 51, 55.5, 55.5)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ai_crs) %>%
  st_combine() %>%  
  st_cast("POLYGON") %>%
  st_shift_longitude() -> wag
# grid
wag %>%
  st_make_grid(cellsize = c(0.6666, 0.3333)) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number()) %>%
  st_shift_longitude() -> wag_grid
# join grid to pot data
pots %>%
  filter(subdistrict == "WAG",
         !is.na(longitude), !is.na(latitude),
         longitude != -9, latitude != -9,
         crab_year >= 1995) %>% 
  mutate(lon = longitude, #ifelse(longitude < -180, 360 + longitude, longitude), 
         lat = latitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = ai_crs) %>%
  st_shift_longitude() %>%
  st_join(wag_grid, .) -> waggrid

# wag cpue index
read_csv("./AIGKC/output/cpue_std/2025/may/wag_st_index_mesh.csv") %>%
  transmute(crab_year = year, index, l95, u95) -> wag_index

# eag plots ----

## proportion effort
eaggrid %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year) %>%
  mutate(n_pots = n()) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(effort_p = n() / mean(n_pots)) %>%
  
  ggplot()+
  geom_sf(aes(fill = effort_p), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(eaggrid)[1], st_bbox(eaggrid)[3]), ylim = c(st_bbox(eaggrid)[2], st_bbox(eaggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Effort")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/eag_prop_effort.png", plot = x, height = 8, width = 8, units = "in")

## cpue
eaggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(cpue = mean(tot_legal)) %>%
  ggplot()+
  geom_sf(aes(fill = cpue), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(eaggrid)[1], st_bbox(eaggrid)[3]), ylim = c(st_bbox(eaggrid)[2], st_bbox(eaggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Legal CPUE")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/eag_legal_cpue.png", plot = x, height = 8, width = 8, units = "in")

# line plot
pots %>%
  filter(subdistrict == "EAG", crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(cpue = mean(tot_legal),
            l95 = cpue - 1.96 * sqrt(var(tot_legal)/n()),
            u95 = cpue + 1.96 * sqrt(var(tot_legal)/n())) -> legal_cpue
eaggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(cellid, crab_year) %>%
  summarise(grid_cpue = mean(tot_legal)) %>%
  
  right_join(expand_grid(crab_year = unique(.$crab_year),
                         cellid = unique(.$cellid))) %>% ungroup %>%
  ggplot()+
  geom_line(aes(x = crab_year, y = grid_cpue, group = cellid), color = "grey90")+
  geom_ribbon(data = legal_cpue, aes(x = crab_year, ymin = l95, ymax = u95), fill = "#0072B2", alpha = 0.2)+
  geom_line(data = legal_cpue, aes(x = crab_year, y = cpue, group = 1), color = "#0072B2")+
  labs(x = NULL, y = "CPUE (crab / pot)")+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  theme_sleek() -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/eag_legal_cpue_by_grid.png", plot = x, height = 3, width = 5, units = "in")


## proportion legal
eaggrid %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(legal_p = sum(tot_legal) / sum(tot_legal + sublegal) ) %>%
  ggplot()+
  geom_sf(aes(fill = legal_p), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(eaggrid)[1], st_bbox(eaggrid)[3]), ylim = c(st_bbox(eaggrid)[2], st_bbox(eaggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Effort")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/eag_prop_legal.png", plot = x, height = 8, width = 8, units = "in")



# effort vs cpue
st_drop_geometry(eaggrid) %>%
  filter(crab_year >= 1995) %>%
  group_by(crab_year, cellid) %>%
  summarise(cpue = mean(tot_legal)) %>%
  left_join(st_drop_geometry(eaggrid) %>%
              filter(!is.na(crab_year)) %>%
              group_by(crab_year) %>%
              mutate(n_pots = n()) %>%
              group_by(crab_year, cellid) %>%
              summarise(effort_p = n() / n_pots) ) %>%
  
  ggplot()+
  geom_point(aes(x = effort_p, y = cpue))+
  facet_wrap(~crab_year, scales = "free_y")

# get cells that continuously contribute the top 50% of effort
eaggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(crab_year) %>%
  mutate(tot_pots = n()) %>%
  group_by(crab_year, cellid, geometry, tot_pots) %>%
  summarise(n_pots = n()) %>% ungroup %>%
  nest_by(crab_year) %>% ungroup %>% #pull(data) %>% .[[1]] -> x
  mutate(data = purrr::map(data, function(x){
    x %>%
      arrange(n_pots) %>%
      mutate(cum_p = cumsum(n_pots) / tot_pots) %>%
      filter(cum_p > 0.5)
  })) %>% unnest(data) %>% st_as_sf() %>%
  ggplot()+
  geom_sf(aes(fill = n_pots), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(eaggrid)[1], st_bbox(eaggrid)[3]), ylim = c(st_bbox(eaggrid)[2], st_bbox(eaggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Effort")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./tmp.png", plot = x, height = 8, width = 8, units = "in")

eaggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(crab_year) %>%
  mutate(tot_pots = n()) %>%
  group_by(crab_year, cellid, geometry, tot_pots) %>%
  summarise(n_pots = n()) %>% ungroup %>%
  nest_by(crab_year) %>% ungroup %>% #pull(data) %>% .[[1]] -> x
  mutate(data = purrr::map(data, function(x){
    x %>%
      arrange(n_pots) %>%
      mutate(cum_p = cumsum(n_pots) / tot_pots) %>%
      filter(cum_p < 0.5)
  })) %>% unnest(data) %>%
  st_drop_geometry() %>% 
  transmute(crab_year, cellid) %>%
  left_join(eaggrid) %>%
  group_by(crab_year) %>%
  summarise(cpue = mean(sublegal)) %>%
  
  ggplot()+
  geom_line(aes(x = crab_year, y = cpue))

  





# wag plots ----

## proportion effort
waggrid %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year) %>%
  mutate(n_pots = n()) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(effort_p = n() / n_pots) %>%
  ggplot()+
  geom_sf(aes(fill = effort_p), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(waggrid)[1], st_bbox(waggrid)[3]), ylim = c(st_bbox(waggrid)[2], st_bbox(waggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Effort")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x

ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/wag_prop_effort.png", plot = x, height = 8, width = 8, units = "in")


## cpue
waggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(cpue = mean(tot_legal)) %>%
  ggplot()+
  geom_sf(aes(fill = cpue), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(waggrid)[1], st_bbox(waggrid)[3]), ylim = c(st_bbox(waggrid)[2], st_bbox(waggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Legal CPUE")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/wag_legal_cpue.png", plot = x, height = 8, width = 8, units = "in")

pots %>%
  filter(subdistrict == "WAG", crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(cpue = mean(tot_legal),
            l95 = cpue - 1.96 * sqrt(var(tot_legal)/n()),
            u95 = cpue + 1.96 * sqrt(var(tot_legal)/n())) -> legal_cpue
waggrid %>%
  filter(crab_year >= 1995) %>%
  group_by(cellid, crab_year) %>%
  summarise(grid_cpue = mean(tot_legal)) %>%
  
  right_join(expand_grid(crab_year = unique(.$crab_year),
                         cellid = unique(.$cellid))) %>% ungroup %>%
  
  ggplot()+
  geom_line(aes(x = crab_year, y = grid_cpue, group = cellid), color = "grey90")+
  geom_ribbon(data = legal_cpue, aes(x = crab_year, ymin = l95, ymax = u95), fill = "#0072B2", alpha = 0.2)+
  geom_line(data = legal_cpue, aes(x = crab_year, y = cpue, group = 1), color = "#0072B2")+
  labs(x = NULL, y = "CPUE (crab / pot)")+
  scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks)+
  theme_sleek() -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/wag_legal_cpue_by_grid.png", plot = x, height = 3, width = 5, units = "in")

## proportion legal
waggrid %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year, cellid, geometry) %>%
  summarise(legal_p = sum(tot_legal, na.rm = T) / sum(tot_legal + sublegal, na.rm = T) ) %>%
  mutate(legal_p = ifelse(is.nan(legal_p), 0, legal_p)) %>%
  ggplot()+
  geom_sf(aes(fill = legal_p), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(waggrid)[1], st_bbox(waggrid)[3]), ylim = c(st_bbox(waggrid)[2], st_bbox(waggrid)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Effort")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/wag_prop_legal.png", plot = x, height = 8, width = 8, units = "in")



# combined area plots ----

# effort vs cpue
st_drop_geometry(waggrid) %>%
  filter(crab_year >= 1995) %>%
  group_by(subdistrict, crab_year, cellid) %>%
  summarise(cpue = mean(tot_legal)) %>%
  left_join(st_drop_geometry(waggrid) %>%
              filter(!is.na(crab_year)) %>%
              group_by(subdistrict, crab_year) %>%
              mutate(n_pots = n()) %>%
              group_by(subdistrict, crab_year, cellid) %>%
              summarise(effort_p = n() / n_pots) ) %>%
  bind_rows(# effort vs cpue
    st_drop_geometry(eaggrid) %>%
      filter(crab_year >= 1995) %>%
      group_by(subdistrict, crab_year, cellid) %>%
      summarise(cpue = mean(tot_legal)) %>%
      left_join(st_drop_geometry(eaggrid) %>%
                  filter(!is.na(crab_year)) %>%
                  group_by(subdistrict, crab_year) %>%
                  mutate(n_pots = n()) %>%
                  group_by(subdistrict, crab_year, cellid) %>%
                  summarise(effort_p = n() / n_pots) )) %>%
      
  
  ggplot()+
  geom_point(aes(x = effort_p, y = cpue, color = subdistrict))+
  facet_wrap(~crab_year, scales = "free_y")


# effort vs cpue
st_drop_geometry(waggrid) %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year, cellid) %>%
  summarise(legal_p = sum(tot_legal, na.rm = T) / sum(tot_legal + sublegal, na.rm = T) ) %>%
  mutate(legal_p = ifelse(is.nan(legal_p), 0, legal_p)) %>%
  left_join(st_drop_geometry(waggrid) %>%
              group_by(subdistrict, crab_year, cellid) %>%
              summarise(cpue = mean(tot_legal)) ) %>%
  bind_rows(
    st_drop_geometry(eaggrid) %>%
      filter(!is.na(crab_year)) %>%
      group_by(crab_year, cellid) %>%
      summarise(legal_p = sum(tot_legal, na.rm = T) / sum(tot_legal + sublegal, na.rm = T) ) %>%
      mutate(legal_p = ifelse(is.nan(legal_p), 0, legal_p)) %>%
      left_join(st_drop_geometry(eaggrid) %>%
                  group_by(subdistrict, crab_year, cellid) %>%
                  summarise(cpue = mean(tot_legal)) )) %>%
  
  
  ggplot()+
  geom_point(aes(x = legal_p, y = cpue, color = subdistrict))+
  facet_wrap(~crab_year, scales = "free_y")


# effort vs prop legal
st_drop_geometry(waggrid) %>%
  filter(!is.na(crab_year)) %>%
  group_by(crab_year, cellid) %>%
  summarise(legal_p = sum(tot_legal, na.rm = T) / sum(tot_legal + sublegal, na.rm = T) ) %>%
  mutate(legal_p = ifelse(is.nan(legal_p), 0, legal_p)) %>%
  left_join(st_drop_geometry(waggrid) %>%
              filter(!is.na(crab_year)) %>%
              group_by(subdistrict, crab_year) %>%
              mutate(n_pots = n()) %>%
              group_by(subdistrict, crab_year, cellid) %>%
              summarise(effort_p = n() / n_pots) ) %>%
  bind_rows(
    st_drop_geometry(eaggrid) %>%
      filter(!is.na(crab_year)) %>%
      group_by(crab_year, cellid) %>%
      summarise(legal_p = sum(tot_legal, na.rm = T) / sum(tot_legal + sublegal, na.rm = T) ) %>%
      mutate(legal_p = ifelse(is.nan(legal_p), 0, legal_p)) %>%
      left_join(st_drop_geometry(eaggrid) %>%
                  filter(!is.na(crab_year)) %>%
                  group_by(subdistrict, crab_year) %>%
                  mutate(n_pots = n()) %>%
                  group_by(subdistrict, crab_year, cellid) %>%
                  summarise(effort_p = n() / n_pots) )) %>%
  
  
  ggplot()+
  geom_point(aes(x = effort_p, y = legal_p, color = subdistrict))
  facet_wrap(~crab_year, scales = "free_y")
  
  # number of vessels and number of pots
  pots %>%
    group_by(subdistrict, crab_year) %>%
    summarise(n_ves = length(unique(adfg)),
              n_pots = n()) %>%
    mutate(pots_ves = n_pots / n_ves) %>%
    
    ggplot()+
    geom_line(aes(x = crab_year, y = pots_ves, color = subdistrict))
  
  
  


# fishery extent and other trends----

## eag

st_drop_geometry(eaggrid)  %>% 
  transmute(crab_year, longitude, latitude) %>%
  filter(!is.na(crab_year)) %>%
  nest_by(crab_year) %>% ungroup %>%
  mutate(extent = purrr::map_dbl(data, function(x) {
    x %>% dist %>% mean
  })) %>%
  dplyr::select(-data) %>%
  mutate(extent = extent - mean(extent),
         type = "Observer") %>% 
  # bind to fish ticket extent
  bind_rows(ft %>%
              filter(fishery == "EAG",
                     crab_year >= 1995) %>%
              mutate(stat_area = as.character(stat_area)) %>%
              # replace some statareas missing from the shape file
              mutate(stat_area = ifelse(stat_area %in% c("695231", "695232", "695233"), "695238",
                                        ifelse(stat_area %in% "665334", "665331", stat_area))) %>%
              group_by(crab_year, stat_area) %>%
              summarise(effort = sum(effort_sum, na.rm = T)) %>% ungroup %>%
              left_join(sa) %>%
              st_as_sf() %>% st_centroid() %>%
              bind_cols(., st_coordinates(.)) %>%
              st_drop_geometry() %>%
              group_by(crab_year) %>% nest() %>%    
              mutate(extent = purrr::map_dbl(data, function(data) {
                
                data %>%
                  expand_grid(., ., .name_repair = "unique") %>%
                  rename_all(~c("stat_area1", "effort1", "X1", "Y1", "stat_area2", "effort2", "X2", "Y2")) %>%
                  mutate(dist = sqrt((X2 - X1)^2 + (Y2 - Y1)^2),
                         wt = effort1 * effort2) %>%
                  summarise(mean = weighted.mean(dist, wt)) %>%  pull(mean)
                
                
              })) %>% 
              transmute(crab_year, extent) %>% ungroup %>% 
              mutate(extent = extent - mean(extent),
                     type = "Fish Ticket")) %>%
  
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_line(aes(x = crab_year, y = extent, linetype = type))+
  labs(x = NULL, y = "Extent", linetype = NULL, title = "EAG")+
  scale_linetype_manual(values = c(2, 1))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        plot.title = element_text(hjust = 0.5)) -> ep1

  
ft %>%
  filter(fishery == "EAG",
         crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(cpue = sum(number_of_crab, na.rm = T) / sum(effort_sum, na.rm = T)) %>%
  mutate(index = cpue / prod(cpue)^(1/nrow(.)),
         type = "Fish Ticket Nominal") %>%
  bind_rows(eag_index %>% mutate(type = "GAMM")) %>% #print(n = 1000)
  
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_ribbon(aes(x = crab_year, ymin = l95, ymax = u95, group = type), fill = "grey40", alpha = 0.2)+
  geom_line(aes(x = crab_year, y = index, linetype = type))+
  scale_linetype_manual(values = c(2, 1))+
  labs(x = NULL, y = "Index", linetype = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> ep2

ft %>%
  filter(fishery == "EAG",
         crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(vessels = length(unique(adfg_number)),
            tot_pots = sum(effort_sum, na.rm = T) / 10000) %>%
  transmute(crab_year, Vessels = vessels, `10K pots` = tot_pots) %>%
  pivot_longer(2:3) %>%
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_line(aes(x = crab_year, y = value, linetype = name))+
  scale_linetype_manual(values = c(2, 1))+
  labs(x = NULL, y = "N", linetype = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  #scale_y_continuous(sec.axis = sec_axis(trans = ~. * 10, name = "1,000 pots (dotted)"))+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) -> ep3



## wag

st_drop_geometry(waggrid)  %>% 
  transmute(crab_year, longitude, latitude) %>%
  filter(!is.na(crab_year)) %>%
  nest_by(crab_year) %>% ungroup %>%
  mutate(extent = purrr::map_dbl(data, function(x) {
    x %>% dist %>% mean
  })) %>%
  dplyr::select(-data) %>%
  mutate(extent = extent - mean(extent),
         type = "Observer") %>%
  # bind to fish ticket extent
  bind_rows(ft %>%
              filter(fishery == "WAG",
                     crab_year >= 1995) %>%
              mutate(stat_area = as.character(stat_area)) %>%
              # replace some statareas missing from the shape file
              mutate(stat_area = ifelse(stat_area %in% c("755204"), "755201",
                                 ifelse(stat_area %in% c("765131", "765133", "765134"), "765132",
                                 ifelse(stat_area %in% c("765135", "765136"), "765137",
                                 ifelse(stat_area %in% c("765201", "765202"), "765203",
                                 ifelse(stat_area %in% c("775136"), "775135",
                                        ifelse(stat_area %in% c("755205"), "755201", stat_area))))))) %>%
              group_by(crab_year, stat_area) %>%
              summarise(effort = sum(effort_sum, na.rm = T)) %>% ungroup %>%
              left_join(sa) %>% 
              st_as_sf() %>% st_centroid() %>%
              bind_cols(., st_coordinates(.)) %>%
              st_drop_geometry() %>% 
              mutate(X = ifelse(X > 0, X - 360, X)) %>%
              group_by(crab_year) %>% nest() %>%    
              mutate(extent = purrr::map_dbl(data, function(data) {
                
                data %>%
                  expand_grid(., ., .name_repair = "unique") %>%
                  rename_all(~c("stat_area1", "effort1", "X1", "Y1", "stat_area2", "effort2", "X2", "Y2")) %>%
                  mutate(dist = sqrt((X2 - X1)^2 + (Y2 - Y1)^2),
                         wt = effort1 * effort2) %>%
                  summarise(mean = weighted.mean(dist, wt)) %>%  pull(mean)
                
                
              })) %>% 
              transmute(crab_year, extent) %>% ungroup %>% 
              mutate(extent = extent - mean(extent),
                     type = "Fish Ticket")) %>%
  
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_line(aes(x = crab_year, y = extent, linetype = type))+
  labs(x = NULL, y = "Extent", linetype = NULL, title = "WAG")+
  scale_linetype_manual(values = c(2, 1))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        plot.title = element_text(hjust = 0.5)) -> wp1


ft %>%
  filter(fishery == "WAG",
         crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(cpue = sum(number_of_crab, na.rm = T) / sum(effort_sum, na.rm = T)) %>%
  mutate(index = cpue / prod(cpue)^(1/nrow(.)),
         type = "Fish Ticket Nominal") %>%
  bind_rows(wag_index %>% mutate(type = "GAMM")) %>% #print(n = 1000)
  
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_ribbon(aes(x = crab_year, ymin = l95, ymax = u95, group = type), fill = "grey40", alpha = 0.2)+
  geom_line(aes(x = crab_year, y = index, linetype = type))+
  scale_linetype_manual(values = c(2, 1))+
  labs(x = NULL, y = "Index", linetype = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> wp2

ft %>%
  filter(fishery == "WAG",
         crab_year >= 1995) %>%
  group_by(crab_year) %>%
  summarise(vessels = length(unique(adfg_number)),
            tot_pots = sum(effort_sum, na.rm = T) / 10000) %>%
  transmute(crab_year, Vessels = vessels, `10K pots` = tot_pots) %>%
  pivot_longer(2:3) %>%
  ggplot()+
  geom_vline(xintercept = 2005, linetype = 3, color = "grey60")+
  geom_line(aes(x = crab_year, y = value, linetype = name))+
  scale_linetype_manual(values = c(2, 1))+
  labs(x = NULL, y = "N", linetype = NULL)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  #scale_y_continuous(sec.axis = sec_axis(trans = ~. * 10, name = "1,000 pots (dotted)"))+
  theme(legend.position = c(1, 1), legend.justification = c(1, 1)) -> wp3



ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/extent_cpue_nvessels.png", 
       plot = (wp1 + ep1) / (wp2 + ep2) / (wp3 + ep3), height = 8, width = 10, units = "in")

  
# spatial comparison of gearcode ----

# plot proportion of round pots by grid cell
eaggrid %>%
  filter(!is.na(crab_year)) %>%
  filter(!is.na(gearcode)) %>%
  filter(!(gearcode %in% c(1:3, 14:23, 80, 81))) %>%
  mutate(gearcode = ifelse(gearcode == 9, 5,
                           ifelse(gearcode == 10, 6, 
                                  ifelse(gearcode == 11, 7, gearcode))),
         soaktime = ifelse(crab_year %in% 1995:1996, soaktime * 24, soaktime)) %>%
  # biotwine is okay
  filter(biotwine_ok != "N") %>%
  # remove area specific gear codes and quantiles
  # filter for middle quantiles of soaktime and depth by year
  filter(!gearcode %in% c(4, 13, 8)) %>%
  group_by(crab_year) %>%
  filter(soaktime > quantile(soaktime, 0.025), 
         soaktime < quantile(soaktime, 0.975),
         depth > quantile(depth, 0.01), 
         depth < quantile(depth, 0.99)) %>% ungroup %>%
  mutate(pot_shp = ifelse(gearcode == 12, "Round", "Rectangle")) %>%
  count(cellid, crab_year, pot_shp) %>%
  pivot_wider(names_from = pot_shp, values_from = n) %>%
  replace_na(list(Rectangle = 0, Round = 0)) %>%
  group_by(cellid, crab_year) %>%
  mutate(prop_rnd = Round / (Round + Rectangle)) %>% ungroup %>% 
  mutate(prop_rnd = ifelse(prop_rnd == 0, NA, prop_rnd)) -> tmp
ggplot(data = tmp)+
  geom_sf(aes(fill = prop_rnd), size = 2)+
  geom_sf(data = land)+
  coord_sf(xlim = c(st_bbox(tmp)[1], st_bbox(tmp)[3]), ylim = c(st_bbox(tmp)[2], st_bbox(tmp)[4]))+
  facet_wrap(~crab_year, ncol = 5)+
  scale_fill_gradient2(low = "white", mid = "gold", high = "firebrick")+
  labs(fill = "Proportion Round Pot")+
  geom_text_npc(aes(npcx = "left", npcy = 0.9, label = crab_year),
                check_overlap = T, size = 3)+
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(0, "lines"),
        panel.border = element_rect(color = "grey30", fill = NA),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") -> x
ggsave(filename = "./AIGKC/figures/fishery_data_maps/2025/proportion_round_pots.png", plot = x, height = 8, width = 8, units = "in")
  
dir.create("./gearcode_plots")
st_drop_geometry(eaggrid) %>%
  filter(!is.na(crab_year)) %>%
  filter(!is.na(gearcode)) %>%
  filter(!(gearcode %in% c(1:3, 14:23, 80, 81))) %>%
  mutate(gearcode = ifelse(gearcode == 9, 5,
                           ifelse(gearcode == 10, 6, 
                                  ifelse(gearcode == 11, 7, gearcode))),
         soaktime = ifelse(crab_year %in% 1995:1996, soaktime * 24, soaktime)) %>%
  # biotwine is okay
  filter(biotwine_ok != "N") %>%
  # remove area specific gear codes and quantiles
  # filter for middle quantiles of soaktime and depth by year
  filter(!gearcode %in% c(4, 13, 8)) %>%
  group_by(crab_year) %>%
  filter(soaktime > quantile(soaktime, 0.025), 
         soaktime < quantile(soaktime, 0.975),
         depth > quantile(depth, 0.01), 
         depth < quantile(depth, 0.99)) %>% ungroup %>%
  group_by(crab_year, cellid, adfg) %>%
  filter(12 %in% unique(gearcode),
         length(unique(gearcode)) > 1) %>% ungroup %>%
  nest_by(crab_year, cellid, adfg, .keep = T) %>% ungroup %>%
  mutate(plot = purrr::map(data, function(data){
    data %>%
      add_count(gearcode) %>%
      mutate(nsamp = paste0("N = ", n)) %>%
      mutate(nsamp_y = max(tot_legal + sublegal)*1.1 ) %>%
      ggplot()+
      geom_boxplot(aes(x = factor(gearcode), y = tot_legal + sublegal))+
      geom_text(aes(x = factor(gearcode), y = nsamp_y, label = nsamp))+
      labs(x = "Gearcode", y = "CPUE")+
      theme_sleek() -> x
    ggsave(paste0("./gearcode_plots/", unique(data$cellid), "_", unique(data$adfg), "_", unique(data$crab_year), ".png"),
           plot = x, width = 4, height = 4, units = "in")
  }))
  

  
