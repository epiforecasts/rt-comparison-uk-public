test_capacity <- readr::read_csv("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=England&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newPillarOneTestsByPublishDate%22:%22newPillarOneTestsByPublishDate%22,%22newPillarTwoTestsByPublishDate%22:%22newPillarTwoTestsByPublishDate%22,%22newPillarThreeTestsByPublishDate%22:%22newPillarThreeTestsByPublishDate%22,%22newPillarFourTestsByPublishDate%22:%22newPillarFourTestsByPublishDate%22,%22newTestsByPublishDate%22:%22newTestsByPublishDate%22,%22cumPillarOneTestsByPublishDate%22:%22cumPillarOneTestsByPublishDate%22,%22cumPillarTwoTestsByPublishDate%22:%22cumPillarTwoTestsByPublishDate%22,%22cumPillarThreeTestsByPublishDate%22:%22cumPillarThreeTestsByPublishDate%22,%22cumPillarFourTestsByPublishDate%22:%22cumPillarFourTestsByPublishDate%22,%22cumTestsByPublishDate%22:%22cumTestsByPublishDate%22%7D&format=csv")

test_capacity %>%
  ggplot(aes(x = date, y = newTestsByPublishDate)) +
  geom_line()

# Check back-filling cases over time
files <- list.files("data", full.names = TRUE)
files <- files[grep(".rds", files)]
files <- lapply(files, readRDS)

data <- bind_rows(files)

files <- list("201012", "201004", "200922", "200914", "200823")
names(files) <- files
files <- lapply(files, paste0, ".rds")
files <- lapply(files, readRDS,  file = "data/")
readRDS()