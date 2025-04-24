# bbrkc functions for 2024 SAFE that aren't in gmacsr.R 
# kjp
# 7-31-24 / 4-23-25

# ideally would source this R doc at beginning 

# selectivity ----------
# gmacs_plot_slx() ----

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_slx()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_slx <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_slx(all_out, file, model_name)}
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  
  # plots
  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    nest_by(fleet, .keep = T) %>% ungroup %>% 
    mutate(capture_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>% 
        ggplot()+
        geom_line(aes(x = size, y = slx_capture, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~capture_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(capture_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Selectivity", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_capture.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$capture_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    retention_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_retention, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Retention", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_retention.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    discard_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_discard, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Discard", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_discard.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    })) -> out
  
  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {c(out$capture_plot, out$retention_plot, out$discard_plot)}
  
  
}



# gmacs_plot_catch_kjp() ----

## plot fits to gmacs catch data
## edited to include sex as an option for plotting and the wrap y axises

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### y_labs - optional, custom y axis labels, as character vector
### data_summary - alternate way to bring in data, output of gmacs_get_catch_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_catch(all_out = list(mod_23.1b), plot_dir = "./put/file/here")

gmacs_plot_catch_kjp <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, 
                             data_summary = NULL, file = NULL, model_name = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_catch_summary(all_out, file, model_name)}
  
  # plots 
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  data_summary %>%
    nest_by(series, units, .keep = T) %>% ungroup %>%
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%
    #dplyr::slice(1) %>% # pull(data) %>% .[[1]]-> data
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {
      
      # y label
      if(is.null(y_labs)) {
        y_lab <- str_wrap(paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " ", unique(data$sex) ," Catch (", unique(data$wt_units), ")"), 30)
        if(unique(data$units) == "Numbers") {
          y_lab <- str_wrap(paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " ", unique(data$sex) ," Catch (", unique(data$n_units), ")"), 30)
        }
      }
      
      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, units),
                               year = min(data$year):max(data$year)),
                   by = join_by(model, series, year, units)) %>%
        mutate(obs_l95 = obs_catch * exp(-1.96 * sqrt(log(1 + cv^2))),
               obs_u95 = obs_catch * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
        ggplot()+
        geom_point(aes(x = year, y = obs_catch), color = "grey40")+
        geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey40")+
        geom_line(aes(x = year, y = pred_catch, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      
      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      
      if(save_plot == T) {
        
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("catch_fit_", tolower(unique(data$fleet)), "_", tolower(unique(data$units)), ".png")), 
               width = pwidth, 
               height = pwidth * (3/5), units = "in")
      }
      
      return(p)
      
    })) -> plots
  
  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2), 
           filename = file.path(plot_dir, "catch_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in") 
    
    return("done")
    
  } else {return(plots$plot)}
  
}



#gmacs_plot_f_mmb_dir() ----
  
  ## plot relationship between f and mmb
  
  ## args:
  ### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
  ### save_plot - T/F save plots, default = T
  ### plot_dir - file directory in which to save plots
  ### beta- spr beta. Default 0.25
  ### alpha - spr alpha. Default 0.1
  ### spr_targ - target spr ratio. Default 0.35
  ### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
  ### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
  
  gmacs_plot_f_mmb_dir <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){
    
    # bring in all out data ----
    
    if(!is.null(file) && is.null(all_out)) {
      if(is.null(model_name)) {stop("Must supply model name(s)!!")}
      # read all out file
      all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
    }
    
    # save plot dirs ----
    if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
    if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
    
    # extract data
    tibble(mod = names(all_out),
           all_out = all_out) %>%
      mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
             plot = purrr::map(all_out, function(x) {
               
               
               # get directed fs
               gmacs_get_f(list(x)) %>%
                 filter(sex == "male", fleet == "Pot_Fishery") %>%
                 group_by(year) %>%
                 summarise(f = sum(F)) -> fs
               
               # get mmb time series
               gmacs_get_derived_quantity_summary(list(x)) %>%
                 transmute(year, mmb = ssb/1000) -> mmb
               
               # set up mmb vector for fofl control rule line
               b <- 0:max(mmb$mmb)
               
               # get biomass target and fmsy
               btarg <- x$bmsy/1000
               ftarg <- x$f_msy_tot
               
               # control rule
               tibble(b = b) %>%
                 mutate(f_ofl = case_when(b / btarg <= beta ~ 0,
                                          b / btarg > beta & b/btarg <= 1 ~ ftarg * ((b/btarg - alpha) / (1 - alpha)),
                                          b / btarg > 1 ~ ftarg)) -> control_rule
               
               # plot annotation
               annotation <- paste0("F", spr_targ*100, " = ", round(ftarg, 3), "\nFOFL = ", round(x$f_ofl_tot, 3))
               
               # plot
               left_join(fs, mmb, by = "year") %>%
                 filter(f > 0, f < 0.8) %>%
                 ggplot()+
                 geom_line(data = control_rule, aes(x = b, y = f_ofl), size = 1)+
                 geom_text(aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5)+
                 geom_text(data = function(x){filter(x, year == max(x$year))}, 
                           aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5, color = "firebrick")+
                 geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation),
                               check_overlap = T, size = 3)+
                 scale_x_continuous(labels = scales::comma)+
                 labs(x = paste0("MMB (", x$wt_units, ")"), y = "F") -> p
               
               if(save_plot == T) {
                 ggsave(plot = p, 
                        filename = file.path(plot_dir, paste0(x$model_name, "_f_mmb_direct.png")),
                        height = 6, width = 5, units = "in") 
               }
               
               return(p)
               
             })) -> out
    # out ----
    if(save_plot == T) {return("done")}
    if(save_plot == F) {return(out$plot)}
    
    
  }











  
  
  
  
  
  
  

# exploratory area --------------
  # gmacs_plot_sizecomp() ----
  
  ## plot fits to gmacs size comp data
  
  ## args:
  ### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
  ### save_plot - T/F save plots, default = T
  ### plot_dir - file directory in which to save plots
  ### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
  ### plot_nsamp_est - show stage 2 effective sample sizes on plots? T/F
  ### nsamp_est_model - name of model to display nsamp_est for. Only required if plotting more than a single model. Defaults to first in list.
  ### aggregate_series_labels - character vector of labels for aggregate series, ex: c("Male", "Female") or c("New Shell", "Old Shell");
  ###                           or list with elements being character vectors for each aggregate series (if you want to use different labels)
  ### data_summary - alternate way to bring in data, output of gmacs_get_index_summary()
  ### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
  ### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
  
  
  gmacs_plot_sizecomp_kjp <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size",
                                      plot_nsamp_est = F, nsamp_est_model = NULL, aggregate_series_label = NULL, data_summary = NULL, file = NULL, 
                                      model_name = NULL) {
    
    # get size summary data
    if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name)}
    
    # check dir
    if(save_plot == T & is.null(plot_dir)) {dir.create("./plots", showWarnings = F, recursive = TRUE); plot_dir <- "./plots"}
    if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
    
    # make plots with all models
    data_summary %>% 
      nest_by(mod_series, .keep = T) %>% ungroup %>%# pull(data) %>% .[[5]] -> data
      mutate(agg = purrr::map_lgl(data, function(data) {
        # check for aggregated comp
        data <- dplyr::select(data, where(function(x) !all(is.na(x))))
        agg <- "aggregate_series" %in% names(data)
        return(agg)
      }),
      agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
      aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
        if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
        if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
        if(agg == F) {return(NA)}
      }),
      plots = purrr::pmap(list(data, agg, aggregate_series_label), function(data, agg, aggregate_series_label){
        
        ### check nsamp_est_model
        if(is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
        
        ## comp, agg comp, and residual plots
        if(agg == T) {
          ## setup for plotting aggregate series ----
          # get some detail about size bins
          size_bins <- data %>% pull(size) %>% unique 
          n_bins <- length(size_bins)
          n_yr <- length(unique(data$year))
          bin_width <- size_bins[2] - size_bins[1]
          # adjust size bin for the secondary series
          data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
          # get size breaks and labels for the plot
          brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
          data %>%
            distinct(aggregate_series, plot_size) %>% 
            nest_by(aggregate_series) %>% ungroup %>%
            mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
            pull(breaks) %>% unlist %>% as.numeric -> breaks
          data %>%
            distinct(size, plot_size) %>%
            filter(plot_size %in% breaks) %>% pull(size) -> labels
          data %>%
            filter(aggregate_series > 1) %>%
            group_by(aggregate_series) %>%
            summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
          if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
          
          ## comp by year ----
          ### plot dimensions
          cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
          rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3))) 
          ### plot
          data %>%  
            rowwise %>%
            mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                           paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
                                           paste0("N = ", round(nsamp_obs))),
                   nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
            mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
            ggplot()+
            geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
            geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
            geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
            scale_x_continuous(breaks = breaks, labels = labels)+
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
            labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
            geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
            geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
                          check_overlap = T, size = 3)+
            facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
            scale_color_manual(values = cbpalette)+
            scale_fill_grey()+
            theme(panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  panel.border = element_blank(),
                  axis.line.x = element_line(color = "grey70", size = 0.2),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_blank()) -> p_comp_year
          ### save
          if(save_plot == T){
            height = min(rows, 10)
            width = min(cols*3, 9)
            ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_comp_year, height = height, width = width, units = "in") 
          }
          
          
          ## aggregate size comp ----
          ### plot
          data %>%  
            group_by(model, aggregate_series, size, plot_size) %>%
            summarise(obs = sum(obs), pred = sum(pred),
                      nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
            
            mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                           paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                           paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                   nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
            mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
            ggplot()+
            geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
            geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
            geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
            scale_x_continuous(breaks = breaks, labels = labels)+
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
            labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
            geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                          check_overlap = T, size = 3)+
            scale_color_manual(values = cbpalette)+
            scale_fill_grey()+
            theme(panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  panel.border = element_blank(),
                  axis.line.x = element_line(color = "grey70", size = 0.2),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_blank()) -> p_comp_agg
          ### save
          if(save_plot == T){
            ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_comp_agg, height = 3, width = 5, units = "in") 
          }
          
          ## line residual plot ----
          
          #   ### plot dimensions
          #   cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
          #   rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
          #   ### plot
          #   data %>%  
          #     ggplot()+
          #     geom_line(aes(x = plot_size, y = residual, group = interaction(aggregate_series, model), color = model))+
          #     geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
          #     geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          #     scale_x_continuous(breaks = breaks, labels = labels)+
          #     scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          #     labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          #     geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
          #     facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          #     scale_color_manual(values = cbpalette)+
          #     scale_fill_grey()+
          #     theme(panel.spacing.x = unit(0.2, "lines"),
          #           panel.spacing.y = unit(0, "lines"),
          #           panel.border = element_blank(),
          #           axis.line.x = element_line(color = "grey70", size = 0.2),
          #           axis.ticks.y = element_blank(),
          #           axis.text.y = element_blank(),
          #           axis.text.x = element_text(size = 8),
          #           plot.title = element_text(hjust = 0.5),
          #           strip.background = element_blank(),
          #           strip.text.x = element_blank(),
          #           panel.background = element_blank()) -> p_resid_line
          #   ### save
          #   if(save_plot == T){
          #     height = min(rows, 10)
          #     width = min(cols*3, 9)
          #     ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
          #            plot = p_resid_line, height = height, width = width, units = "in") 
          #   }
          #   
          # }
          # if(agg == F) {
          #   ## setup for plotting aggregate series ----
          #   # get some detail about size bins
          #   size_bins <- data %>% pull(size) %>% unique 
          #   n_bins <- length(size_bins)
          #   n_yr <- length(unique(data$year))
          #   bin_width <- size_bins[2] - size_bins[1]
          #   
          #   ## comp by year ----
          #   ### plot dimensions
          #   cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
          #   rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
          #   ### plot
          #   data %>%  
          #     rowwise() %>%
          #     mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
          #                                    paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
          #                                    paste0("N = ", round(nsamp_obs))),
          #            nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
          #     ggplot()+
          #     geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          #     geom_line(aes(x = size, y = pred, color = model))+
          #     scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          #     labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          #     geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
          #     geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
          #                   check_overlap = T, size = 3)+
          #     facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          #     scale_color_manual(values = cbpalette)+
          #     theme(panel.spacing.x = unit(0.2, "lines"),
          #           panel.spacing.y = unit(0, "lines"),
          #           panel.border = element_blank(),
          #           axis.line.x = element_line(color = "grey70", size = 0.2),
          #           axis.ticks.y = element_blank(),
          #           axis.text.y = element_blank(),
          #           axis.text.x = element_text(size = 8),
          #           plot.title = element_text(hjust = 0.5),
          #           strip.background = element_blank(),
          #           strip.text.x = element_blank(),
          #           panel.background = element_blank()) -> p_comp_year
          #   ### save
          #   if(save_plot == T){
          #     height = min(rows, 10)
          #     width = min(cols*3, 9)
          #     ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
          #            plot = p_comp_year, height = height, width = width, units = "in") 
          #   }
          #   
          
          ## aggregate size comp ----
          ### plot
          data %>%  
            group_by(model, size) %>%
            summarise(obs = sum(obs), pred = sum(pred),
                      nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
            mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                           paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                           paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                   nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
            ggplot()+
            geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
            geom_line(aes(x = size, y = pred, color = model))+
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
            labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
            geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                          check_overlap = T, size = 3)+
            scale_color_manual(values = cbpalette)+
            theme(panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  panel.border = element_blank(),
                  axis.line.x = element_line(color = "grey70", size = 0.2),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_blank()) -> p_comp_agg
          ### save
          if(save_plot == T){
            ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_comp_agg, height = 3, width = 5, units = "in") 
          }
          
          ## line residual plot ----
          
          ### plot dimensions
          cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
          rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
          ### plot
          data %>%  
            ggplot()+
            geom_line(aes(x = size, y = residual, color = model))+
            geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
            labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
            geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
            facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
            scale_color_manual(values = cbpalette)+
            theme(panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0, "lines"),
                  panel.border = element_blank(),
                  axis.line.x = element_line(color = "grey70", size = 0.2),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5),
                  strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  panel.background = element_blank()) -> p_resid_line
          ### save
          if(save_plot == T){
            height = min(rows, 10)
            width = min(cols*3, 9)
            ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_resid_line, height = height, width = width, units = "in") 
          }
          
        }
      })) -> out
    
    # make plots by model
    data_summary %>% 
      nest_by(model, mod_series, .keep = T) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
      mutate(agg = purrr::map_lgl(data, function(data) {
        # check for aggregated comp
        data <- dplyr::select(data, where(function(x) !all(is.na(x))))
        agg <- "aggregate_series" %in% names(data)
        return(agg)
      }),
      agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
      aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
        if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
        if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
        if(agg == F) {return(NA)}
      }),
      plots = purrr::pmap(list(data, agg, aggregate_series_label, model), function(data, agg, aggregate_series_label, model){
        
        ### check nsamp_est_model
        if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
        if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
        
        if(agg == T) {
          ## setup for plotting aggregate series ----
          # get some detail about size bins
          size_bins <- data %>% pull(size) %>% unique 
          n_bins <- length(size_bins)
          n_yr <- length(unique(data$year))
          bin_width <- size_bins[2] - size_bins[1]
          # adjust size bin for the secondary series
          data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
          # get size breaks and labels for the plot
          brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
          data %>%
            distinct(aggregate_series, plot_size) %>% 
            nest_by(aggregate_series) %>% ungroup %>%
            mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
            pull(breaks) %>% unlist %>% as.numeric -> breaks
          data %>%
            distinct(size, plot_size) %>%
            filter(plot_size %in% breaks) %>% pull(size) -> labels
          data %>%
            filter(aggregate_series > 1) %>%
            group_by(aggregate_series) %>%
            summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
          if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
          
          ## dot plot ----
          
          data %>%
            # compute residual
            mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                   residual < 0 ~ "< 0"),
                   residual = ifelse(residual == 0, NA, residual),
                   aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
            ggplot()+
            geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                       shape = 21, alpha = 0.5)+
            scale_fill_manual(values = c("black", "white", NA))+
            scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
            labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
            theme(legend.position = "top")+
            facet_wrap(~aggregate_series_label, ncol = 1, scales = "free_y") -> p_dot
          ### save
          if(save_plot == T){
            height = unique(data$aggregate_series) * 6
            width = min(6, n_yr * 0.5)
            ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_dot, height = height, width = width, units = "in") 
          }
        }
        if(agg == F) {
          ## setup for plotting aggregate series ----
          # get some detail about size bins
          size_bins <- data %>% pull(size) %>% unique 
          n_bins <- length(size_bins)
          n_yr <- length(unique(data$year))
          bin_width <- size_bins[2] - size_bins[1]
          
          ## dot plot ----
          
          data %>%
            # compute residual
            mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                   residual < 0 ~ "< 0"),
                   residual = ifelse(residual == 0, NA, residual)) %>%
            ggplot()+
            geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                       shape = 21, alpha = 0.5)+
            scale_fill_manual(values = c("black", "white", NA))+
            scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
            labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
            theme(legend.position = "top") -> p_dot
          ### save
          if(save_plot == T){
            height = 6
            width = min(6, n_yr * 0.5)
            ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                   plot = p_dot, height = height, width = width, units = "in") 
          }
        }
        ## mean size (francis) plot ---- 
        
        data %>%
          group_by(year) %>%
          summarise(obs_mean_size = weighted.mean(size, obs),
                    pred_mean_size = weighted.mean(size, pred), 
                    sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
                    l95 = pred_mean_size + sd * qnorm(0.025),
                    u95 = pred_mean_size + sd * qnorm(0.975),
                    sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
                    l95_est = pred_mean_size + sd_est * qnorm(0.025),
                    u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
          ggplot()-> tmp
        if(plot_nsamp_est == T){tmp+geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)->tmp}
        tmp+geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
          geom_line(aes(x = year, y = pred_mean_size, group = 1))+
          geom_point(aes(x = year, y = obs_mean_size))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
          labs(x = NULL, y = paste0("Mean ", size_lab)) -> p_mean_size
        ### save
        if(save_plot == T){
          height = 4
          width = 6
          ggsave(file.path(plot_dir, paste0(model, "_mean_size_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_mean_size, height = height, width = width, units = "in") 
        }
        
        return(list(p_dot, p_mean_size))
        
      })) -> out2
    
    # output
    if(save_plot == F) {return(c(out %>% pull(plots), out2 %>% pull(plots)))} else{"done"}
    
  }
  
  
  
  
  
  
  
  # explore f mmb -----
  temp <-  m230a_24$F_by_sex_fleet_year_season
  temp %>% 
    filter(sex == "male", fleet == "Pot_Fishery", season == "3") %>% 
    print(n = 100)
  # these appear to match 2023 values from figure ....but not newly created figure....
  
  
  # gmacs_do_jitter() ----
  
  # do gmacs jitter runs
  
  ### gmacs.dat - file path to gmacs.dat file
  ### sd - jitter standard deviation
  ### iter - number of iteration of jittering to run
  ### ref_points - T/F calculate reference points, add mmb and b35 to jittering results, default = T
  ### pin - T/F use pin file
  ### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
  ### save_csv - T/F, save csv file output
  ### csv_dir - file directory in which to save output
  ### save_plot - T/F, create histograms, default = T
  ### plot_dir - file directory in which to save plots
  ### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"
  
  gmacs_do_jitter <- function(gmacs.dat, sd, iter, ref_points = T, pin = F, wait = T,
                              save_csv = T, csv_dir = NULL, save_plot = T, plot_dir = NULL, model_name = NULL, plot_only = F, version1 = "2.20.16") {
    
    # create output directories
    if(save_csv == T & is.null(csv_dir)) {csv_dir <- file.path(dirname(gmacs.dat), "output"); dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
    if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
    if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
    if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
    
    # directory ----
    
    options(warn = -1) 
    # get parent directory
    dir <- dirname(gmacs.dat)
    # save working dir
    wd <- getwd()
    setwd(dir) # change wd
    
    if(plot_only == F){
      
      # set up ----
      
      # check for other needed inputs
      if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
      # look for gmacs_file_in.dat - if not present, run gmacs
      if(!file.exists("./gmacs_files_in.dat")) {setwd(wd); gmacs_do_exe(gmacs.dat, pin = pin, reweight = F)}
      dat <- readLines("./gmacs_files_in.dat")
      if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
      if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
      if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
      # make sure pin file is being used as expected
      if(pin == T){
        dat[grep("use pin file", dat)] <- "1 # use pin file (0=no, 1=yes)"   
        if(!file.exists("gmacs.pin")) {stop(paste("Cannot find gmacs.pin!!"))}
      }
      if(pin == F){
        dat[grep("use pin file", dat)] <- "0 # use pin file (0=no, 1=yes)"   
        
      }
      
      # do jitter ----
      
      # create subdirectory for jitter run files
      dir.create("./jitter")
      # put files in - this likely will not work with relative pathes
      file.copy(c(dat[grep("\\.dat", dat)], dat[grep("\\.ctl", dat)], dat[grep("\\.prj", dat)], "gmacs.exe", "gmacs_files_in.dat"), 
                to = "./jitter")
      # set working 
      setwd("./jitter")
      # turn on reference points
      if(ref_points == T){dat[33] <- "1 # Calculate reference points (0=no)"}
      if(ref_points == F){dat[33] <- "0 # Calculate reference points (0=no)"}
      # set up jitter
      dat[16] <- 1
      dat[18] <- sd
      # write gmacs.dat file
      writeLines(dat, "gmacs.dat"); file.remove("gmacs_files_in.dat")
      gfiles <- list.files()
      
      # do jitter runs
      out <- tibble(iteration = 1:iter,
                    obj_function = NA,
                    max_gradient = NA,
                    catch_lik = NA,
                    index_lik = NA,
                    size_lik = NA,
                    mmb_curr = NA,
                    bmsy = NA)
      for (i in 1:iter) {
        rundir <- paste0("./run_", i)
        dir.create(rundir)
        file.copy(from = gfiles, to = rundir)
        # do gmacs run
        setwd(rundir)
        while(!("gmacs.rep" %in% list.files())){shell("gmacs.exe", wait = wait)}
        ao <- gmacs_read_allout("./Gmacsall.out", version = version1)
        out$obj_function[i] <- ao$objective_function
        out$max_gradient[i] <- ao$max_gradient
        out$catch_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "catch"]
        out$index_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "index"]
        out$size_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "size"]
        if(ref_points == T) {
          out$mmb_curr[i] <- ao$mmb_curr
          out$bmsy[i] <- ao$bmsy
        }
        setwd("..")
      }
      out <- out %>% dplyr::select(where(function(x) !all(is.na(x))))
      # return to model directory
      setwd("..")
      
    }
    
    # get mle estimates of objects
    mle_ao <- gmacs_read_allout("./Gmacsall.out", model_name = model_name, version = version1)
    # set wd back to original
    setwd(wd)
    
    # plots ----
    
    if(plot_only == T){out <- read_csv(paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
    
    # obj fxn
    ggplot()+
      geom_histogram(data = out, aes(x = obj_function), color = 1, fill = "grey80", 
                     width = 1)+
      geom_vline(xintercept = mle_ao$objective_function, linetype = 2, color = 2)+
      scale_x_continuous(labels = scales::comma)+
      labs(x = "Negative Log-likelihood", y = "Jitter Runs") -> p_obj
    
    if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_obj_fxn_jitter_sd_", sd, ".png"),
                              plot = p_obj,
                              height = 3, width = 5, units = "in")}
    
    if(ref_points == T){
      # mmb
      ggplot()+
        geom_histogram(data = out, aes(x = mmb_curr), color = 1, fill = "grey80", 
                       width = 1)+
        geom_vline(xintercept = mle_ao$mmb_curr, linetype = 2, color = 2)+
        scale_x_continuous(labels = scales::comma)+
        labs(x = paste0("MMB (", mle_ao$wt_units, ")") , y = "Jitter Runs") -> p_mmb
      
      if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_mmb_jitter_sd_", sd, ".png"),
                                plot = p_mmb,
                                height = 3, width = 5, units = "in")}  
      
      # b35
      ggplot()+
        geom_histogram(data = out, aes(x = bmsy), color = 1, fill = "grey80", 
                       width = 1)+
        geom_vline(xintercept = mle_ao$bmsy, linetype = 2, color = 2)+
        scale_x_continuous(labels = scales::comma)+
        labs(x = bquote(B["35%"]~"("~.(mle_ao$wt_units)~")"), y = "Jitter Runs") -> p_bmsy
      
      if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_b35_jitter_sd_", sd, ".png"),
                                plot = p_bmsy,
                                height = 3, width = 5, units = "in")}
    }
    
    # output ----
    if(ref_points == T){plots <- list(p_obj, p_mmb, p_bmsy)}else{plots <- list(p_obj)}
    if(save_csv == T) {write_csv(out, paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
    
    if(save_plot == F){return(c(list(out), plots))}else{return(out)}
    
  }
  
  
#  mature female abundance **fix ** not working------------------------------
gmacs_plot_matfem <- function(all_out = NULL, save_plot = T, plot_dir = NULL) {
  
  # get summary data
  
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_ssb", par)) %>%
      transmute(ssb_se = se / (1 / exp(est)), 
                ssb_lci = exp(est) + ssb_se * qnorm(ci_alpha / 2), 
                ssb_uci = exp(est) + ssb_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(ssb_se = NA, ssb_lci = NA, ssb_uci = NA) -> data_summary

  
  # plot ssb
  data_summary %>%
    ggplot()+
    {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = ssb_lci, ymax = ssb_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
    geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
    {if(plot_proj == T){geom_point(data = proj,
                                   aes(x = factor(year), y = mmb, color = model))}}+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL, y = paste0("MMB (", unique(data_summary$wt_units), ")"), color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mmb
  # plot ssa
  data_summary %>%
    ggplot()+
    geom_line(aes(x = factor(year), y = ssa, group = model, color = model))+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL, 
         y = ifelse(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones"),
                    "MMA", paste0("MMA (", unique(data_summary$n_units), ")")),
         color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mma
  if(save_plot == T){
    ggsave(file.path(plot_dir, "mmb_trajectory.png"), plot = mmb, height = 4.2, width = 7, units = "in")
    ggsave(file.path(plot_dir, "mma_trajectory.png"), plot = mma, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F){return(list(mmb, mma))}
  
}}