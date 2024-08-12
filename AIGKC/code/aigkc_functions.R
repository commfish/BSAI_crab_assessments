# notes ---- 

# functions for aigkc assessment data prep
# tyler jackson
# 3/24/2023

# load ---- 

library(MASS)
library(splines)
library(tidyverse)
library(ggfortify)
library(lubridate)
library(lme4)
library(mgcViz)
library(mgcv)
library(patchwork)
library(visreg)
library(DHARMa)

# define orthogonal contrast method
options(contrasts = c("contr.treatment", "contr.poly"))

# ggplot theme and yr axis ----

theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}
tickr <- function(data, var, to) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1950:2100), yr, 5)

### custom color/fill pallete
cb_palette <- c("#009E73", "#0072B2", "#E69F00", "#56B4E9", 
                "#F0E442", "#D55E00", "#CC79A7")
# scale_colour_discrete <- function(...) {scale_colour_manual(..., values = cb_palette)}
# scale_fill_discrete <- function(...) {scale_fill_manual(..., values = cb_palette)}
# ggplot <- function(...) ggplot2::ggplot(...) + scale_colour_discrete() + scale_fill_discrete()

# f_add_blocks() ----

## function to add lat / lon area blocks to observer data
## arg: data - observer pot data including column names 'lat' and 'lon'
f_add_blocks <- function(data) {
  
  data %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>%
    # block definitions from siddeek
    mutate(block = case_when((latitude == -9 | longitude == -9) ~ -9,
                             (latitude < 50.75 | latitude > 55.25) ~ -9,
                             (longitude < -189.50 | longitude > -167.5) ~ -9,
                             ((latitude >= 52.5 & latitude <= 53.75) & (longitude >= -169.25 & longitude <= -167.5)) ~ 1,
                             ((latitude >= 52.25 & latitude <= 53.5) & (longitude >= -170.5 & longitude <= -169.251)) ~ 2,
                             ((latitude >= 51.75 & latitude <= 53.5) & (longitude >= -172.0 & longitude <= -170.501)) ~ 3,
                             ((latitude >= 51.75 & latitude <= 52.75) & (longitude >= -174.0 & longitude <= -172.001)) ~ 4,
                             ((latitude >= 51.25 & latitude <= 52.5) & (longitude >= -177.0 & longitude <= -174.001)) ~ 5,
                             ((latitude >= 51.0 & latitude <= 52.0) & (longitude >= -179.75 & longitude <= -177.001)) ~ 6,
                             ((latitude >= 52.0 & latitude <= 53.0) & (longitude >= -181.0 & longitude <= -179.11)) ~ 7,
                             ((latitude >= 51.75 & latitude <= 52.0) & (longitude >= -181.0 & longitude <= -179.75)) ~ 7,
                             ((latitude >= 51.0 & latitude <= 51.75) & (longitude >= -181.0 & longitude <= -179.751)) ~ 8,
                             ((latitude >= 51.0 & latitude <= 52.52) & (longitude >= -185.0 & longitude <= -181.000)) ~ 8,
                             ((latitude >= 51.75 & latitude <= 53.5) & (longitude >= -189.50 & longitude <= -185.001)) ~ 9,
                             ((latitude >= 53.75 & latitude <= 55.0) & (longitude >= -181.50 & longitude <= -179.75)) ~ 10)) -> out
  
  return(out)
                             
}


# f_add_area() ----
## arg: data - observer pot data including column name statarea

f_add_area <- function(data) {
  
  data %>%
    # area gp definitions from siddeek
    mutate(areagp = case_when(statarea == -9 ~ -9,
                              statarea %in% 665300:685334 ~ 66,
                              statarea %in% 695199:705301 ~ 69,
                              statarea %in% 715129:735301 ~ 71,
                              statarea %in% 745130:755331 ~ 74,
                              statarea %in% 765099:775201 ~ 76,
                              statarea %in% 785100:795431 ~ 78,
                              statarea %in% 805100:815432 ~ 80,
                              statarea %in% 825099:835301 ~ 82,
                              statarea %in% 845099:865303 ~ 84,
                              statarea %in% 865303:895331 ~ 87)) -> out
  
  return(out)
}

# f_getCPUE() ----

## function to extract standardized CPUE indices and confidence limits from a fitted linear model
## arg: model - a fitted model object (lm, glm) or a summary object(summary.lm, summary.glm) 
##      where - a vector of integers pointing at year effects among model coefficients
##      years - a vector of numbers for labeling the years
## notes:    The effect of the first year is inferred, so length(years) = length(i) + 1                                  #
##           f_getCPUE() is a generalized version of canonical(), with full backward compatibility                         #
##                                                                                                                       #
## history:  23mar2003 - Terese sends canonical() source code to Arni                                                    #
##           31mar2003 - Arni implements getCPUE                                                                         #
##                                                                                                                       #
## returns:  Data frame containing standardized CPUE indices and confidence limits 
f_getCPUE <- function(model, where, years) {
  
  ## Make sure that model is a summary and error-check vector lengths
  if(any(class(model) == "lm"))
    model <- summary(model)    # model is now a summary object
  n <- length(where) + 1       # number of CPUE indices to be extracted
  if(length(years) != n)
    stop("Expected ", n, " years, got ", length(years), ".")
  
  ## Extract model components
  relative <- as.numeric(c(0, model$coefficients[where,1]))  # shave off names
  std_cpue <- exp(relative - mean(relative))
  if(class(model)=="summary.lm")
    V <- model$cov.unscaled[where, where] * model$sigma^2
  else
    V <- model$cov.scaled[where, where]
  
  ## Compute confidence limits
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
  V0 <- (Q %*% V) %*% t(Q)
  SE <- sqrt(diag(V0))
  Upper <- exp(log(std_cpue) + 2 * SE)
  Lower <- exp(log(std_cpue) - 2 * SE)
  
  out <- tibble(year = years, 
                index = std_cpue,
                se = SE, 
                l95 = Lower,
                u95 = Upper)
  return(out)
  
}
# f_getCPUE_gam() ----
f_getCPUE_gam <- function(model, where, years) {
  
  ## Make sure that model is a summary and error-check vector lengths
  if(any(class(model) == "gam"))
    model <- summary(model)    # model is now a summary object
  n <- length(where) + 1       # number of CPUE indices to be extracted
  if(length(years) != n)
    stop("Expected ", n, " years, got ", length(years), ".")
  
  ## Extract model components
  relative <- as.numeric(c(0, model$p.coeff[where]))  # shave off names
  std_cpue <- exp(relative - mean(relative))
  
  V <- model$cov.scaled[where, where]
  
  ## Compute confidence limits
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
  V0 <- (Q %*% V) %*% t(Q)
  SE <- sqrt(diag(V0))
  Upper <- exp(log(std_cpue) + 2 * SE)
  Lower <- exp(log(std_cpue) - 2 * SE)
  
  out <- tibble(year = years, 
                index = std_cpue, 
                se = SE, 
                l95 = Lower,
                u95 = Upper)
  return(out)
  
}

# f_getCPUE_gamm4() ----
f_getCPUE_gamm4 <- function(model, where, years) {
  
  ## Make sure that model is a summary and error-check vector lengths
  if(any(class(model) == "glmerMod"))
    model <- summary(model)    # model is now a summary object
  n <- length(where) + 1       # number of CPUE indices to be extracted
  if(length(years) != n)
    stop("Expected ", n, " years, got ", length(years), ".")
  
  ## Extract model components
  relative <- as.numeric(c(0, model[[10]][where]))  # shave off names
  std_cpue <- exp(relative - mean(relative))
  
  model[[20]]
  V <- model$vcov[where, where]
  
  ## Compute confidence limits
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
  V0 <- (Q %*% V) %*% t(Q)
  SE <- sqrt(diag(V0))
  Upper <- exp(log(std_cpue) + 2 * SE)
  Lower <- exp(log(std_cpue) - 2 * SE)
  
  out <- tibble(year = years, 
                index = std_cpue, 
                se = SE, 
                l95 = Lower,
                u95 = Upper)
  return(out)
  
}

# f_getCPUE_glmm() ----
f_getCPUE_glmm <- function(model, where, years) {
  
  ## Make sure that model is a summary and error-check vector lengths
  if(any(class(model) == "glmmTMB"))
    model <- summary(model)    # model is now a summary object
  n <- length(where) + 1       # number of CPUE indices to be extracted
  if(length(years) != n)
    stop("Expected ", n, " years, got ", length(years), ".")
  
  ## Extract model components
  relative <- as.numeric(c(0, model$coefficients$cond[where,1]))  # shave off names
  std_cpue <- exp(relative - mean(relative))
  
  V <- model$vcov$cond[where, where]
  
  ## Compute confidence limits
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
  V0 <- (Q %*% V) %*% t(Q)
  SE <- sqrt(diag(V0))
  Upper <- exp(log(std_cpue) + 2 * SE)
  Lower <- exp(log(std_cpue) - 2 * SE)
  
  out <- tibble(year = years, 
                index = std_cpue, 
                se = SE, 
                l95 = Lower,
                u95 = Upper)
  return(out)
  
}

# f_getCPUEyrb_gam() ----

## extract cpue index for glm with year:block interaction
## args: model = gam object

f_getCPUEyrb_gam <- function(model) {
    
    # get parameters location
    loc <- grep(":block", names(coef(model)))
    
    # block area
    block_area <- tibble(block = 1:10, 
                         area_nmi2 = c(375, 1364, 1765, 915, 452, 1026, 812, 2172, 1042, 334))
    
    # tibble with parameter estimates
    # compute bias correction
    tibble(par = names(coef(model)[loc]),
           year = as.numeric(substring(par, 10, 13)),
           block = ifelse(as.numeric(substring(par, nchar(par), nchar(par))) == 0,
                          10, 
                          as.numeric(substring(par, nchar(par), nchar(par)))), # need a better solution for this line
           bias_cor_est = exp(coef(model)[loc] + diag(vcov(model))[loc] / 2),
           se_est = sqrt(diag(vcov(model))[loc])) %>%
      # join to block area
      left_join(block_area) %>%
      # multiply cpue and block area
      mutate(bindex = bias_cor_est * area_nmi2) -> bindex
    
    # add fit for missing values
    bindex_fit <- glm(log(bindex) ~ factor(year) + factor(block), data = bindex)
    bindex %>% 
      mutate(fit = predict.glm(bindex_fit, ., se.fit = T)$fit,
             se = predict.glm(bindex_fit, ., se.fit = T)$se.fit,
             bindex = ifelse(is.na(bindex), exp(fit + (se^2)/2), bindex),
             se_bindex = ifelse(is.na(se_est), se, se_est)) %>%
      # sum across blocks compute variance
      group_by(year) %>%
      summarise(bindex = sum(bindex, na.rm = T),
                var = sum(area_nmi2^2 * se_bindex^2)) %>%
      # scale index
      mutate(index = bindex / prod(bindex)^(1/nrow(.)),
             cv = sqrt(var) / bindex) -> out
    
    return(out)
  
}



# f_getCPUEyrblock() ----

## extract cpue index for glm with year:block interaction
## args: mod = glm object

f_getCPUEyrblock <- function(mod) {
  
  # get parameters location
  loc <- grep(":block", names(coef(mod)))
  
  # block area
  block_area <- tibble(block = 1:10, 
                       area_nmi2 = c(375, 1364, 1765, 915, 452, 1026, 812, 2172, 1042, 334))
  
  # tibble with parameter estimates
  # compute bias correction
  tibble(par = names(coef(mod)[loc]),
         year = as.numeric(substring(par, 10, 13)),
         block = ifelse(as.numeric(substring(par, nchar(par), nchar(par))) == 0,
                        10, 
                        as.numeric(substring(par, nchar(par), nchar(par)))), # need a better solution for this line
         bias_cor_est = exp(coef(mod)[loc] + diag(vcov(mod))[loc] / 2),
         se_est = sqrt(diag(vcov(mod))[loc])) %>%
    # join to block area
    left_join(block_area) %>%
    # multiply cpue and block area
    mutate(bindex = bias_cor_est * area_nmi2) -> bindex
  
  # add fit for missing values
  bindex_fit <- glm(log(bindex) ~ factor(year) + factor(block), data = bindex)
  bindex %>% 
    mutate(fit = predict.glm(bindex_fit, ., se.fit = T)$fit,
           se = predict.glm(bindex_fit, ., se.fit = T)$se.fit,
           bindex = ifelse(is.na(bindex), exp(fit + (se^2)/2), bindex),
           se_bindex = ifelse(is.na(se_est), se, se_est)) %>%
    # sum across blocks compute variance
    group_by(year) %>%
    summarise(bindex = sum(bindex, na.rm = T),
              var = sum(area_nmi2^2 * se_bindex^2)) %>%
    # scale index
    mutate(index = bindex / prod(bindex)^(1/nrow(.)),
           cv = sqrt(var) / bindex) -> out
  
  return(out)
  
}

# f_step_gam() ----
# args:
# null - null model
# full_scope - list giving the formula for the full model
# step - max number of steps, must just be sufficiently high
# r2_level - level of R2 change considered significant, default = 0.01
f_step_gam <- function(null, full_scope, steps = 1000, r2_level = 0.01, theta = NULL) {
  
  # functions within ----
  
  f_forward_selection <- function(null, full) {
    
    best.for <- null
    for (i in 1:steps){
      
      # extract best model stats
      best.for_AIC <- AIC(best.for, k = log(nrow(best.for$model)) + 1)
      best.for_r2 <- (best.for$null.deviance - best.for$deviance) / best.for$null.deviance
      
      # set up available terms
      best.for_terms <- gsub(" ", "", strsplit(deparse1(best.for$formula), split = "\\+|\\~")[[1]])
      avail_terms <- gsub(" ", "", strsplit(deparse1(full$formula), split = "\\+|\\~")[[1]])
      avail_terms <- avail_terms[!(avail_terms %in% best.for_terms)]
  
      
      # fit test models
      for.mods = list()
      for.aic <- NULL
      for.r2 <- NULL
      sig <- NULL
      df <- NULL
      for(i in 1:length(avail_terms)){
        for.mods[[i]] <- update(best.for, paste0("~ . +", avail_terms[i]))
        for.aic[i] <- AIC(for.mods[[i]], k = log(nrow(for.mods[[i]]$model)) + 1)
        for.r2[i] <- (for.mods[[i]]$null.deviance - for.mods[[i]]$deviance) / for.mods[[i]]$null.deviance
        df[i] <- best.for$df.residual - for.mods[[i]]$df.residual
      }
      tibble(term = paste0("+ ", avail_terms),
             AIC = for.aic,
             delta_AIC = for.aic - best.for_AIC,
             r2 = for.r2,
             delta_r2 = for.r2 - best.for_r2,
             df) -> fortmp
      # filter for significant improvements
      sig <- fortmp %>% 
        filter(abs(delta_AIC) >= (2 * df),
               delta_r2 >= r2_level)
      
      # not complete
      if(nrow(sig) > 0) {
        # print progress
        message("best form:"); print(best.for$formula); message("trying:");print(fortmp)
        
        # keep the best by AIC
        filter(sig, delta_AIC == min(delta_AIC)) -> upd
        # update the best forward model
        best.for <- update(best.for, paste0("~ .", upd$term))
        steps <- steps - 1
      }
      # complete
      if(nrow(sig) == 0) {
        message("Done with forward selection")
        print(fortmp)
        print(best.for)
        break
      }
    }
    
    return(list(best.for, fortmp))
    
  }
  f_backward_selection <- function(null, full) {
    
    best.back <- full      
    for (i in 1:steps){
      
      # extract best model stats
      best.back_AIC <- AIC(best.back, k = log(nrow(best.back$model)) + 1)
      best.back_r2 <- (best.back$null.deviance - best.back$deviance) / best.back$null.deviance
      
      # set up available terms
      avail_terms <- gsub(" ", "", strsplit(deparse1(best.back$formula), split = "\\+|\\~")[[1]])
      null_terms <- gsub(" ", "", strsplit(deparse1(null$formula), split = "\\+|\\~")[[1]])
      avail_terms <- avail_terms[!(avail_terms %in% null_terms)]
      
      # fit test models
      back.mods = list()
      back.aic <- NULL
      back.r2 <- NULL
      sig <- NULL
      df <- NULL
      for(i in 1:length(avail_terms)){
        back.mods[[i]] <- update(best.back, paste0("~ . -", avail_terms[i]))
        back.aic[i] <- AIC(back.mods[[i]], k = log(nrow(back.mods[[i]]$model)) + 1)
        back.r2[i] <- (back.mods[[i]]$null.deviance - back.mods[[i]]$deviance) / back.mods[[i]]$null.deviance
        df[i] <- back.mods[[i]]$df.residual - best.back$df.residual 
      }
      tibble(term = paste0("- ", avail_terms),
             AIC = back.aic,
             delta_AIC = back.aic - best.back_AIC,
             r2 = back.r2,
             delta_r2 = best.back_r2 - back.r2,
             df) -> backtmp
      
      # drop the term that is the least improvement
      backtmp %>% 
        filter((delta_r2 < r2_level) | (delta_AIC < 2 * df)) -> drop
      
      
      # not complete
      if(nrow(drop) > 0) {
        # print progress
        message("best form:"); print(best.back$formula); message("trying:");print(backtmp)
        
        # keep the best by AIC
        filter(drop, delta_AIC == min((delta_AIC))) -> upd
        # update the best forward model
        best.back <- update(best.back, paste0("~ .", upd$term))
        steps <- steps - 1
      }
      # complete
      if(nrow(drop) == 0) {
        message("Done with backward selection")
        print(backtmp)
        print(best.back)
        break
      }
    }
    
    return(best.back)
    
  }
  
  # model selection and theta search ----
  
  for(k in 1:100) {
    
    # update full model
    full <- update(null, full_scope[[1]])
    # forward selection 
    for_mod <- f_forward_selection(null, full)
    for_best <- for_mod[[1]]
    fortmp <- for_mod[[2]]
    # backward selection 
    back_best <- f_backward_selection(null, full)
    
    # check that forward and backward models match ----
    if(paste(sort(attributes(for_best$terms)$term.labels), collapse = "+") != paste(sort(attributes(back_best$terms)$term.labels), collapse = "+")) {
      message("Forward and Backward do not match, choosing the best model based on CAIC")
      if(AIC(for_best, k = log(nrow(for_best$model))) < AIC(back_best, k = log(nrow(back_best$model)))) {best <- for_best} else{best <- back_best}
    }
    else{best = for_best}
    
    if(!is.null(theta)){
    # get optimal theta
    theta_opt <- MASS::theta.ml(best$model$tot_legal, fitted(best))[1]  
    
    # check theta
    if(abs(as.numeric(str_extract(null$family$family, "\\d+\\.*\\d*")) - theta_opt) > 0.01) {
      # refit null
      null <- update(null, family = negbin(theta = round(theta_opt, 2), link = "log")) 
    } 
    else{break}
    }
    else{break}
    
  }
  
  # output ----
  
  if(is.null(theta)) {return(list(best, fortmp))}
  else{return(list(model = best,
                   theta = theta_opt))}
  
  
}

# f_stepCPUE() ----

f_stepCPUE <- function(object, scope, r2.change = 0.01, scale = 0, direction = c("both", "backward", "forward"), 
                       trace = 1, keep = deviance, steps = 1000, ...) {
  
  extractCPUE <- function(fit, scale = 0, ...) {
    n <- length(fit$residuals)
    edf <- n - fit$df.residual
    r.squared<- (fit$null.deviance - fit$deviance) / fit$null.deviance
    c(edf, r.squared)
  }
  # Reset reference year to be first year in regresssion
  contr.baseyear1 <- function(n, contrasts = T, base = 1){
    if(is.numeric(n) && length(n) == 1) {
      levs <- 1:n 
    } else {
      levs <- n
      n <- length(n)
    }
    contr <- array(0, c(n, n), list(levs, levs))
    contr[seq(1, n^2, n + 1)] <- 1
    if(contrasts) {
      if(n < 2)
        stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
      contr <- contr[,  - base, drop = F]
    }
    return(contr)
  }
  # Reset reference year to be second year in regresssion
  contr.baseyear2 <- function(n, contrasts = T, base = 2){
    if(is.numeric(n) && length(n) == 1) {
      levs <- 1:n 
    } else {
      levs <- n
      n <- length(n)
    }
    contr <- array(0, c(n, n), list(levs, levs))
    contr[seq(1, n^2, n + 1)] <- 1
    if(contrasts) {
      if(n < 2)
        stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
      contr <- contr[,  - base, drop = F]
    }
    return(contr)
  }
  
  cut.string <- function(string)
  {
    if(length(string) > 1)
      string[-1] <- paste("\n", string[-1], sep = "")
    string
  }
  AIC.drop1 <- function(fit, Terms, scale, trace, ...)
  {
    n <- length(Terms)
    ans <- matrix(nrow = n + 1, ncol = 2)
    dimnames(ans) <- list(c("<none>", paste("-", Terms, sep = "")), c("df", "AIC"))
    ans[1,  ] <- extractCPUE(fit, scale, ...)
    if(n == 0)
      return(ans)
    i <- 2
    for(tt in Terms) {
      if(trace > 1)
        cat("trying -", tt, "\n")
      else cat(".")
      nfit <- update(fit, paste("~ . -", tt))
      ans[i,  ] <- extractCPUE(nfit, scale, ...)
      if (trace > 2) print(ans[i, ])
      i <- i + 1
    }
    ans
  }
  AIC.add1 <- function(fit, Terms, scale, trace, screen, ...)
  {
    n <- length(Terms)
    ans <- matrix(nrow = n + 1, ncol = 2)
    t2 <- if(length(Terms)) paste("+", Terms, sep = "") else NULL
    dimnames(ans) <- list(c("<none>", t2), c("df", "AIC"))
    ans[1,  ] <- extractCPUE(fit, scale, ...)
    if(n == 0)
      return(ans)
    i <- 2
    for(tt in Terms) {
      if(trace > 1)
        cat("trying +", tt, "\n")
      else cat(".")
      nfit <- update(fit, paste("~ . +", tt))
      ans[i,  ] <- extractCPUE(nfit, scale, ...)
      if (trace > 2) print(ans[i, ])
      i <- i + 1
    }
    ans
  }
  re.arrange <- function(keep)
  {
    namr <- names(k1 <- keep[[1]])
    namc <- names(keep)
    nc <- length(keep)
    nr <- length(k1)
    array(unlist(keep, recursive = F), c(nr, nc), list(namr, namc))
  }
  make.step <- function(models, fit, object)
  {
    change <- sapply(models, "[[", "change")
    rd <- sapply(models, "[[", "deviance")
    dd <- c(NA, diff(rd))
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, diff(rdf))
    AIC <- sapply(models, "[[", "AIC")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", "\nInitial Model:", deparse(as.vector(formula(object))), 
                 "\nFinal Model:", deparse(as.vector(formula(fit))), "\n")
    aod <- data.frame(Step = change, Df = ddf, Deviance = dd, "Resid. Df" = rdf, "Resid. Dev" = rd, "r.squared" = AIC, check.names = F)
    attr(aod, "heading") <- heading
    attr(aod, "class") <- c("anova", "data.frame")
    fit$anova <- aod
    fit
  }
  if(missing(direction))
    direction <- "both"
  else direction <- match.arg(direction)
  backward <- direction == "both" | direction == "backward"
  forward <- direction == "both" | direction == "forward"
  if(missing(scope)) {
    fdrop <- numeric(0)
    fadd <- NULL
  }
  else {
    if(is.list(scope)) {
      fdrop <- if(!is.null(fdrop <- scope$lower)) attr(terms(update.formula(object, fdrop)), "factor") else numeric(0)
      fadd <- if(!is.null(fadd <- scope$upper)) attr(terms(update.formula(object, fadd)), "factor")
    }
    else {
      fadd <- if(!is.null(fadd <- scope)) attr(terms(update.formula(object, scope)), "factor")
      fdrop <- numeric(0)
    }
  }
  if(is.null(fadd)) {
    backward <- T
    forward <- F
  }
  models <- vector("list", steps)
  if(!is.null(keep)) {
    keep.list <- vector("list", steps)
    nv <- 1
  }
  n <- length(object$residuals)
  fit <- object
  cf <- attributes(coef(object))  #check if any terms have zero df
  if(!is.null(cf$singular) && cf$singular > 0) {
    TT <- !match(TL <- attr(object$terms, "term.labels"), names(cf$assign), F)
    if(any(TT)) {
      upd <- eval(parse(text = paste(c(".~.", TL[TT]), collapse = "-")))
      fit <- update(fit, upd)
    }
  }
  bAIC <- extractCPUE(fit, scale, ...)
  edf <- bAIC[1]
  bAIC <- bAIC[2]
  nm <- 1
  Terms <- fit$terms
  cat("\n\nTesting for a change in r.squared of less than ", format(round(r2.change,4)),"\n")
  models[[nm]] <- list(deviance = bAIC - 2 * edf, df.resid = n - edf, change = "", AIC = bAIC)
  if(!is.null(keep))
    keep.list[[nm]] <- keep(fit, bAIC)
  AIC <- 0
  count.steps<-0
  while(steps > 0) {
    steps <- steps - 1
    AIC <- bAIC
    bfit <- fit
    ffac <- attr(Terms, "factor")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod.drop <- NULL
    aod.add <- NULL
    aod<-NULL
    change <- NULL
    if(backward && (ndrop <- length(scope$drop))) {
      aod.drop <- AIC.drop1(fit, scope$drop, scale = scale, trace = trace, ...)  
    }
    if(forward && (nadd <- length(scope$add))) {
      aod.add <- AIC.add1(fit, scope$add, scale = scale, trace = trace, screen = screen, ...)
    }
    if(is.null(aod.drop) && is.null(aod.add)) break
    if(!is.null(aod.drop) && nrow(aod.drop) > 1) {
      o <- rev(order(aod.drop[,"AIC"]))
      if((aod.drop[1,"AIC"]-aod.drop[o[2],"AIC"]) < r2.change) {
        change <- dimnames(aod.drop)[[1]][o[2]]
        cat(paste("\nDrop term",change,"\n"))
        if (is.null(aod.add)) {
          aod<-aod.drop
        } else {
          aod<-rbind(aod.drop,aod.add[-1,])
        }
      }
    }
    if (is.null(aod)) {
      o <- order(-aod.add[, "AIC"])
      if((aod.add[o[1],"AIC"]-aod.add[1,"AIC"]) >= r2.change) {
        change <- dimnames(aod.add)[[1]][o[1]]
        cat(paste("\nAdd term",change,"\n"))
        if (is.null(aod.drop)) {
          aod<-aod.add
        } else {
          aod<-rbind(aod.drop,aod.add[-1,])
        }
      }
    }
    if (is.null(aod)) {
      if(is.null(aod.drop))
        aod<-aod.add
      else if(is.null(aod.add))
        aod<-aod.drop
      else aod<-rbind(aod.add,aod.drop[-1,])
      cat("\n")
      print(data.frame("df"=aod[,1],"r.sqaured"=aod[,2]))
      cat("\n")
      break
    }
    if(trace > 1) {
      print(data.frame("df"=aod[,1],"r.sqaured"=aod[,2]))
    }
    fit <- update(fit, eval(parse(text = paste("~ .", change))))
    Terms <- fit$terms
    bAIC <- extractCPUE(fit, scale, ...)
    edf <- bAIC[1]
    bAIC <- bAIC[2]
    nm <- nm + 1
    edf <- models[[nm]] <- list(deviance = bAIC - 2 * edf, df.resid = n - edf, change = change, AIC = bAIC)
    if(!is.null(keep))
      keep.list[[nm]] <- keep(fit, bAIC)
  }
  if(!is.null(keep))
    fit$keep <- re.arrange(keep.list[seq(nm)])
  fit<-make.step(models = models[seq(nm)], fit, object)
  print(fit$anova)
  fit
  
}

# f_dharma() ----

# wrapper function for dharma residuals, plot as ggplot grob
f_dharma <- function(model, plot = T, path = NULL){
  simr <- simulateResiduals(fittedModel = model, plot = F, n = 1000)
  if(plot == T) {
    # make qq plot of simulatied residuals - should be uniform
    qq <- patchwork::wrap_elements(panel = ~plotQQunif(simr, testUniformity = T, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
    # plot residuals against ranked model predictions
    rf <- patchwork::wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.2, pch = 16), clip = F)
    # histogram of residuls
    hs <- patchwork::wrap_elements(panel = ~ {hist(residuals(simr), main = NULL, xlab = "DHARMa Residuals", probability = T); curve(dunif(x), from = -0.1, to = 1.1, add = T, col = 2)})
    ggsave(path, plot = (qq + rf)/ (hs+plot_spacer()), height = 9, width = 9, units = "in")
  }
  return(simr)
}
# f_step_plot() ----
## create step plot for cpue index terms

## args: model - final cpue standardization model, class gam or bam
##       term_labs - optional, alternative term labels for plot

f_step_plot <- function(model, term_labs = NULL){
  
  ## get terms
  form <- gsub(" ", "", strsplit(deparse1(model$formula), split = "\\+|\\~")[[1]])
  resp <- form[1]
  # get the focal (index) term
  index_term <- form[2]
  # update terms
  terms <- form[c(-1,-2)]
  
  # fit the null model and get a standardized index
  null <- update(model, paste0("~.", paste0("- ", terms, collapse = " ")))
  loc <- grep(index_term, names(coef(null)))
  yrs <- sort(model$model %>% pull(index_term) %>% unique)
  
  if(class(model)[1] %in% c("bam", "gam")){
    f_getCPUE_gam(null, loc, yrs) %>%
      mutate(model = index_term) %>%
      dplyr::select(6, 1:5) -> null_ind
    
    # fit models and get a list of std indices
    mods <- list(null)
    ind <- list(null_ind)
    for(i in 2:(length(terms)+1)){
      mods[[i]] <- update(mods[[i-1]], paste0("~ . + ", terms[i-1]))
      loc <- grep(index_term, names(coef(mods[[i]])))
      yrs <- sort(model$model %>% pull(index_term) %>% unique)
      f_getCPUE_gam(mods[[i]], loc, yrs) %>%
        mutate(model = paste("+", terms[i-1])) %>%
        dplyr::select(6, 1:5) -> ind[[i]]
    } 
  }
  if(class(model)[1] %in% c("glm", "lm")){
    f_getCPUE(null, loc, yrs) %>%
      mutate(model = index_term) %>%
      dplyr::select(6, 1:5) -> null_ind
    
    # fit models and get a list of std indices
    mods <- list(null)
    ind <- list(null_ind)
    for(i in 2:(length(terms)+1)){
      mods[[i]] <- update(mods[[i-1]], paste0("~ . + ", terms[i-1]))
      loc <- grep(index_term, names(coef(mods[[i]])))
      yrs <- sort(model$model %>% pull(index_term) %>% unique)
      f_getCPUE(mods[[i]], loc, yrs) %>%
        mutate(model = paste("+", terms[i-1])) %>%
        dplyr::select(6, 1:5) -> ind[[i]]
    } 
  }
  
  # plot
  do.call("rbind", ind) %>%
    mutate(model = factor(model, levels = c(index_term, paste("+", terms)))) %>%
    nest_by(model, .keep = T) %>% ungroup -> tmp
  
  plot_dat <- list(tmp$data[[1]] %>% rename(model_background = model))
  for(i in 2:nrow(tmp)) {
    tmp$data[[i]] %>%
      bind_rows(do.call("rbind", tmp$data[1:(i-1)])) %>%
      rename(model_background = model) -> plot_dat[[i]]
  }
  
  if(is.null(term_labs)){term_labs <- tmp %>% pull(model) %>% unique}
  names(term_labs) <- tmp %>% pull(model) %>% unique
  
  tmp %>%
    mutate(plot_dat = plot_dat) %>%
    transmute(model, plot_dat) %>%
    unnest(plot_dat) %>%
    mutate(alpha = model_background == model) %>%
    ggplot()+
    geom_point(aes(x = year, y = index, alpha = alpha), show.legend = F)+
    geom_line(aes(x = year, y = index, group = model_background, alpha = alpha), show.legend = F)+
    geom_hline(yintercept = 1, linetype = 2)+
    scale_alpha_manual(values = c(0.1, 1))+
    facet_wrap(~model, ncol = 1, strip.position = "left", labeller = labeller(model = term_labs))+
    labs(x = NULL, y = NULL)+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0, "lines")) -> plot
  
  return(plot)
  
}

# f_glm_r2() ----

# extract r2 from glm model
# arg: mod - glm model object
f_glm_r2 <- function(mod) {
  
  r2 <- (mod$null.deviance - mod$deviance) / mod$null.deviance
  return(r2)
  
}

# f_CAIC() ----

f_CAIC <- function(model){AIC(model, k = log(nrow(model$model)) + 1.0)}

# f_add_len_bin() ----

## add length bins to dataset containing size data
## args: data - dataset (tibble or dataframe)
##       col - column containing size data

f_add_len_bin <- function(data, col) {
  
  # add length bin mid point as defined by Siddeek
  data %>%
    mutate(bin = ceiling(col / 5) * 5 - 2,
           bin = ifelse(bin < 103, 103, bin),
           bin = ifelse(bin > 183, 183, bin)) %>%
    return(.)
}

# f_mid_date() ----

## compute mid date between two dates
## args: date_x - character string or date object in form of MMDDYYYY
##       date_y - character string or date object in form of MMDDYYYY

f_mid_date <- function(date_x, date_y) {
  
  x = date_x
  y = date_y
  # set date format
  if(is.character(date_x)) {x = mdy(date_x)}
  if(is.character(date_y)) {y = mdy(date_y)}
  
  # get interval between dates
  ndays_mid = as.numeric(y - x) / 2
  
  # mid date
  mid = x + ndays_mid
  
  return(mid)
  
}

# f_crab_year() ----

# args: x - observer or dockside data frame with "fishery" code column
#       date_correct - do crab_year correction, default = F
#       date_format - format of date field, 

f_crab_year <- function(x, date_correct = F, date_format = "mdy", data = "crab_obs") {
  
  if(data == "crab_obs") {
  x %>%
    mutate(crab_year = ifelse(as.numeric(substring(fishery, 3, 4)) < 60, 
                              as.numeric(substring(fishery, 3, 4)) + 2000, 
                              as.numeric(substring(fishery, 3, 4)) + 1900)) -> out
  
  if(date_correct == T) {
    out %>%
      # coerce sample date to dat format
      mutate(sampdate = as_date(sampdate, format = date_format)) %>%
      # adjust crab year
      mutate(crab_year = case_when(sampdate > mdy(paste0("6/30/", crab_year + 1)) ~ crab_year + 1,
                                   sampdate <= mdy(paste0("6/30/", crab_year + 1)) ~ crab_year,
                                   is.na(sampdate) ~ crab_year)) -> out
  }
  }
  
  if(data == "gf_obs") {
    x %>%
      mutate(crab_year = year,
             crab_year = ifelse(month(haul_date) <= 6, crab_year - 1, crab_year)) -> out
    }
  
  return(out)
  
}

# f_bias_correct () ----
# bias correction from log space
# args: ln_mu = mean of log(x)
#       ln_sigma = standard error of log(x)
f_bias_correct <- function(ln_mu, ln_sigma) {
  
  mu = exp(ln_mu + (ln_sigma^2)/2)
  sigma = sqrt((exp(ln_sigma^2) - 1) * exp(2*ln_mu + ln_sigma^2))
  
  return(c(mu = mu, sigma = sigma))
}
# length bin metadata ----

### bins are 101-105, 106-110, 111-115, 116-120...
tibble(min_cl = seq(100.5, 180.5, 5),
       max_cl = min_cl + 4.999,
       mid_cl = round((max_cl + min_cl) / 2),
       mean_t = c(581.515707, 679.3281689, 788.0323474,  908.2783077,  1040.724257, 1186.036294, 1344.888179, 1517.961114,
                  1705.943543, 1909.53096, 2129.425732, 2366.336933, 2620.980182, 2894.077494, 3186.357141, 3498.553516,
                  3993.657581) / 1e6) -> len_bins




# map data ----

#ai <- raster::getData("GADM", country = c("USA"), level = 1, path = "./AIGKC/data/maps")
#ai@data <- filter(ai@data, NAME_1 == "Alaska")
#eag_proj <- coord_quickmap(xlim = c(-173.9, -167.8), ylim = c(51.8, 53.5))
