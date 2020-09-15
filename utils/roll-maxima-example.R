# Explore identifying peaks using maximum within a rolling window 
# 
# Restrict to the two big peaks in England Rt estimates from cases
# True peaks are:
#  - 2020-06-29 or 2020-06-30 (plateau at peak)
#  - 2020-07-30 
#
# Get estimates
summary_wide <- readRDS("rt-estimate/summary_wide.rds")
# Restrict to last 2 peaks
data <- summary_wide[summary_wide$region=="England" &
                          summary_wide$date > as.Date("2020-06-01"),
                        c("date", "median_cases_blend")]
# Convert to zoo object
data_zoo <- zoo::zoo(data["median_cases_blend"], order.by = data$date)


# Find peaks
#
# For each date, returns TRUE if date == date at maximum found over window
#  e.g. for a window = 7, at some date ("centre date"),
#  - finds the date of the maximum estimate among 
#    a seven day period within (-3 days, centre date, +3 days),
#  - gives TRUE if date of centre date is equal to date of maximum estimate
# 
# Set an odd numbered window of days over which to take the max
window <- 3
# Set the centre position of that window
centre_index <- (window + 1) / 2
# Roll over that window, identify max value, and return TRUE on that date
rollmax <- zoo::rollapplyr(data_zoo, window, function(x) which.max(x)==centre_index)
# Get the dates at which the window maximums were found
peaks <- zoo::index(rollmax)[rollmax==T]

# Approximately right identification of peaks:
# peaks are given as "2020-06-30", "2020-07-29", "2020-07-31"

# Could also do any odd-number window
# - But choice of window impacts location of maximum:
# window = 5 gives "2020-07-01", "2020-08-01"
# window = 7 gives "2020-07-02", "2020-08-02"
# - As does choice of centre position when window > 3:
#   as centre_index can be (window +1 or -1) / 2
# window = 5 and centre_index = 2 gives "2020-07-02", "2020-08-02"
# window = 5 and centre_index = 3 gives "2020-07-01" "2020-08-01"
# window = 7 and centre_index = 3 gives "2020-07-03", "2020-08-03"
# window = 7 and centre_index = 4 gives "2020-07-02", "2020-08-02"

# Repeat for minima over rolling window
rollmin <- zoo::rollapplyr(data_zoo, window, function(x) which.min(x)==centre_index)
troughs <- zoo::index(rollmin)[rollmin==T]
