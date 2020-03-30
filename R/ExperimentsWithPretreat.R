library(readxl)
library(here)

CoRRE_proj <- read_excel("Data/CoRRE_project_descriptive_list.xlsx")
CoRRE_trts <- read.csv(here::here("Data", "CoRRE_treatment_summary.csv"), row.names = 1)
CoRRE_sp <- read.csv(here::here("Data", "CoRRE_relative_abundance_Nov2019.csv"), row.names = 1)
CoRRE_ANPP <- read.csv(here::here("Data", "CoRRE_anpp_raw.csv"), row.names = 1)


pretreat <-CoRRE_trts[CoRRE_trts$treatment_year == 0, ]
test <- unique(pretreat[,-c(3,4)])
df <- unique(pretreat[,c(1,2)])

years <- unique(CoRRE_sp[,c(1,2,4)])
years_p <- unique(CoRRE_ANPP[,c(1,2,4)])

                