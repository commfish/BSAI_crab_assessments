# **************************************************************************************************
# Produce plots for SAFE
# Date: March 2024
# Author: Caitlin Stern
# **************************************************************************************************

folder <- "smbkc_24s"
plot.dir <- paste0(here::here(), "/SMBKC/", folder, "/doc/safe_figures/")

## read in models
m16.0 <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0/Gmacsall.out", model = "smbkc16.0")
m16.0a <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_a/Gmacsall.out", model = "smbkc16.0a")
m16.0b <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_b/Gmacsall.out", model = "smbkc16.0b")
m16.0c <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out", model = "smbkc16.0c")
m24.0 <- gmacs_read_allout(file = "./SMBKC/smbkc_24s/model_16_0_c/Gmacsall.out", model = "smbkc24.0")

# plot indices
gmacs_plot_index(all_out = list(m16.0, m16.0a, m16.0b, m16.0c, m24.0), plot_dir = "./SMBKC/smbkc_24s/safe_figures")
gmacs_get_index_summary(all_out = list(m16.0, m16.0a, m16.0b, m16.0c, m24.0))

# plot size comps
gmacs_plot_size_comp(all_out = list(m16.0, m16.0a, m16.0b, m16.0c, m24.0), plot_dir = "./SMBKC/smbkc_24s/safe_figures")


# plot mmb
gmacs_plot_mmb(all_out = list(m16.0, m16.0a, m16.0b, m16.0c, m24.0), save_plot = T, plot_dir = "./SMBKC/smbkc_24s/safe_figures")

# *******************************************************************************************************************
# data extent plot
# *******************************************************************************************************************

# for each data type, 0 indicates no data from that year and a non-zero number indicates data in that year.
# the non-zero numbers are chosen only to control order of display on the plot and have no other meaning
years <- c(1978:2023)
pot.retained <- c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,0,0,0,0,0,0,0,0,0,0,8,8,8,8,0,8,8,0,0,0,0,0,0,0,0)
pot.bycatch <- c(0,0,0,0,0,0,0,0,0,0,0,0,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,0,0,0,0,0,0,0,0,0,0,6.5,6.5,6.5,6.5,0,6.5,6.5,6.5,6.5,6.5,0,0,0,0,0)
trawl.bycatch <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,0)
fixed.bycatch <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,0)
nmfs.sur <- c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)
adfg.sur <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.5,0,0,3.5,0,0,3.5,0,0,3.5,0,0,3.5,0,0,3.5,0,0,3.5,0,3.5,3.5,3.5,3.5,0,0,0,3.5,0)
pot.size <- c(0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,2,2,2,2,0,2,2,0,0,0,0,0,0,0,0)
nmfs.size <- c(1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5)
adfg.size <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,1,1,1,0,0,0,1,0)

data.extent <- data.frame(years, pot.retained, pot.bycatch, trawl.bycatch, fixed.bycatch, nmfs.sur, adfg.sur, pot.size, nmfs.size, adfg.size) %>%
  rename(Year = years) %>%
  pivot_longer(!Year, names_to = "group", values_to = "value") %>%
  mutate(color = case_when(
    group %in% c("pot.retained", "pot.bycatch", "pot.size") ~ "pot.fishery",
    group %in% c("nmfs.sur", "nmfs.size") ~ "nmfs",
    group %in% c("adfg.sur", "adfg.size") ~ "adfg",
    TRUE ~ group
  ))

de.plot <- ggplot(data.extent, aes(x = Year, y = value)) +
  geom_point(aes(color = color)) +
  scale_color_manual(values = cbpalette) +
  theme(legend.position = "none",
        axis.title.y=element_blank()) + #,
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank()) +
  scale_y_continuous(position = "right", limits = c(0.5, 9), breaks = c(1,1.5,2,3.5,4,5.5,6,6.5,8), labels = c("ADF&G pot", "NMFS trawl", "Pot fishery", "ADF&G pot", "NMFS trawl", "Fixed bycatch", "Trawl bycatch", "Pot fishery", "Pot fishery")) +
  annotate("text", x = 2000, y = 8.5, label = "Retained catch") +
  annotate("text", x = 2000, y = 7, label = "Discards") +
  annotate("text", x = 2000, y = 4.5, label = "Abundance indices") +
  annotate("text", x = 2000, y = 2.5, label = "Size compositions")

ggsave(filename = paste0(plot.dir, "data_extent_plot.png"),
       plot = de.plot,
       height = 3, width = 5, units = "in")
