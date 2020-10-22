delays <- readRDS(here::here("rt-estimate", "delays", "data", "public_onset_to_report_delay.rds"))

delays$mean # 0.7909166
delays$sd # 1.458985

# From Epinow2:
purrr::map2_dbl(delays$mean, delays$sd, ~ exp(.x + .y^2/2)) # 6.393205

# Using mixR:
mixR::to_mu_sd_lnorm(delays$mean, delays$sd) # mu = 6.393205, sd = 17/39541

# source code:
# mixR::to_mu_sd_lnorm
# function (mulog, sdlog) 
# {
#   mu <- exp(mulog + sdlog^2/2)
#   var <- (exp(sdlog^2) - 1) * exp(2 * mulog + sdlog^2)
#   sd <- sqrt(var)
#   list(mu = mu, sd = sd)
# }

