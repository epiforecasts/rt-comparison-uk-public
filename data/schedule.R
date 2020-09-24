# Schedule UK data downloads
# Nations and regions from covidregionaldata

taskscheduleR::taskscheduler_create(taskname = "save-uk-covid-data", rscript = "data/schedule-data-download.R", 
                     schedule = "DAILY", starttime = "14:00")

# taskscheduler_stop()
# taskscheduler_delete()