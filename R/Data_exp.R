library(ggplot2)
library(ggpubr)
]CoRRE_abund <- read.csv(here("Data", "CoRRE_relative_abundance_Nov2019.csv"), row.names = 1)
pplots <- CoRRE_abund[CoRRE_abund$project_name == "pplots",]

pretreat <- pplots[pplots$treatment_year == 0,]
pretreat <- aggregate(pretreat$relcov, by = list(species = pretreat$genus_species, trt = pretreat$treatment), FUN = mean)
names(pretreat)[3] <- "yr0_abund"
yr12 <- pplots[pplots$treatment_year == 12,]
yr12 <- aggregate(yr12$relcov, by = list(species = yr12$genus_species, trt = yr12$treatment), FUN = mean)
names(yr12)[3] <- "yr12_abund"
df <- merge(pretreat, yr12, by = c("species", "trt"), all = TRUE)
df[is.na(df)] <- 0

ggplot(aes(x = yr0_abund, y = yr12_abund), data = df) + 
  geom_point(aes(color = species)) + 
  geom_vline(xintercept = 0, lty = "dashed") + 
  facet_wrap(~trt) +
  guides(color = FALSE) +
  xlab("Pretreatment Abundance") + ylab("Year 12 Abundance") +
  theme_pubr()


## I think we may want to look at some metric that looks at if the species was present 
# in yr 0 v its abundance in later years. Go by each plot?


