# Format breakpoint estimates and plot
library(data.table); library(magrittr); library(ggplot2); library(patchwork)

break_ni <- as.Date("2020-10-16")
break_wales <- as.Date("2020-10-24")

# Format estimates --------------------------------------------------------
vars <- c("cases", "admissions", "deaths")
models <- c("breakpoint-only",
            "breakpoint-with-rw") #,
            #"gp-breakpoint")
  
# Single breakpoint        
breakpoint_only <- purrr::map(vars, 
                           ~ readr::read_csv(
                            paste0("rt-estimate/estimate-break/", models[1], "/", .x, "/summary/rt.csv")))
names(breakpoint_only) <- c("cases", "admissions", "deaths")
breakpoint_only <- dplyr::bind_rows(breakpoint_only, .id = "source") %>%
  dplyr::mutate(model = "single breakpoint")

# Random walk         
random_walk <- purrr::map(vars, 
                              ~ readr::read_csv(
                                paste0("rt-estimate/estimate-break/", models[2], "/", .x, "/summary/rt.csv")))
names(random_walk) <- c("cases", "admissions", "deaths")
random_walk <- dplyr::bind_rows(random_walk, .id = "source") %>%
  dplyr::mutate(model = "random walk + breakpoint")

# # GP      
# gp <- purrr::map(vars, 
#                  ~ readr::read_csv(
#                                 paste0("rt-estimate/estimate-break/", models[3], "/max-tree-depth-10/", .x, "/summary/rt.csv")))
# names(gp) <- c("cases", "admissions", "deaths")
# gp <- dplyr::bind_rows(gp, .id = "source") %>%
#   dplyr::mutate(model = "gp with breakpoint")
# 
# # Join all & clean
models <- dplyr::bind_rows(breakpoint_only, random_walk) %>%
    dplyr::select(-strat)



# Save to csv -------------------------------------------------------------
readr::write_csv(models, "rt-estimate/estimate-break/firebreak-breakpoints.csv")

