library("dplyr")
library("ggplot2")

# Check BP effect sizes
vars <- c("cases", "admissions", "deaths")
models <- c("breakpoint-only",
            "breakpoint-with-rw") #,
regions <- c("South West", "Wales", "Northern Ireland")

# Get summary
regions_fn <- function(model, var, region){
  estimates <-
    readRDS(here::here("rt-estimate", "estimate-break",
                       model, var, "region", region, "2020-11-02",
                       "summarised_estimates.rds"))
  estimates$region <- region
  estimates$source <- var
  estimates$model <- model
  estimates <- estimates[estimates$variable == "breakpoints",]
  return(estimates)
  }

m <- list()

for (model in models) {
  for (var in vars) {
    for (region in regions) {
      m[[length(m) + 1]] <-
        regions_fn(model = model, var = var, region = region)
    }
  }
}

summary <- dplyr::bind_rows(m) %>%
  dplyr::mutate(dplyr::across(median:upper_90, exp))

effects <- summary %>%
  dplyr::group_by(region, model) %>%
  dplyr::filter(strat == max(strat)) %>%
  dplyr::select(-c(variable, type, date, strat, mean, sd, lower_50:upper_50)) %>%
  dplyr::mutate(region = factor(region,
                                levels = c("South West", "Wales", "Northern Ireland"),
                                ordered = TRUE),
                model = factor(model,
                                levels = c("breakpoint-only", "breakpoint-with-rw"),
                               labels = c("Single breakpoint", "Random walk + breakpoint"),
                                ordered = TRUE))

effects_plot <- ggplot(effects) +
  geom_point(aes(y = median, x = source, colour = model),
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90,
                     x = source, colour = model),
                 alpha = 1, position = position_dodge(width = 0.2)) +
  geom_hline(aes(yintercept = 1), lty = 3) +
  facet_wrap(~region) +
  labs(y = NULL, x = "Effect size of intervention on Rt") +
  scale_colour_brewer("Model", type = "qual", palette = 2) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_rect(colour = "transparent")) +
  coord_flip()

ggsave(here::here("rt-estimate", "estimate-break", Sys.Date(),
                  "break-effect-size.png"), 
       height = 3, width = 5)

saveRDS(effects_plot, here::here("rt-estimate", "estimate-break", Sys.Date(),
                                 "break-effect-size.rds"))

