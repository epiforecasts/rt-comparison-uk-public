# Sequence to update breakpoint analysis

if (!dir.exists(here::here("rt-estimate", "estimate-break", Sys.Date()))) {
  dir.create(here::here("rt-estimate", "estimate-break", Sys.Date()))
}

# Run Rt and format
source(here::here("rt-estimate", "estimate-break", "run-rt-breakpoint.R"))
source(here::here("rt-estimate", "estimate-break", "format-rt-breakpoint.R"))

# Create plots
source(here::here("rt-estimate", "estimate-break", "breakpoint-effects.R"))
source(here::here("rt-estimate", "estimate-break", "plot-breakpoint.R"))

# Run report
file.copy(here::here("rt-estimate", "estimate-break", "generic-report.R"),
          here::here("rt-estimate", "estimate-break", Sys.Date(), "report.R"))