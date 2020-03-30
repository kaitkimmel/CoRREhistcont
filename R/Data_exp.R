library(ggplot2)
library(ggpubr)
library(here)
library(plyr)

CoRRE_abund <- read.csv(here::here("Data", "CoRRE_relative_abundance_Nov2019.csv"), row.names = 1)
CoRRE_sp <- read.csv(here::here("Data", "CoRRE_TRY_species_list.csv"), row.names = 1)
CoRRE_abund <- merge(CoRRE_abund, CoRRE_sp[,c(1,2)], by = "genus_species")
CoRRE_abund <- CoRRE_abund[,-1]
pplots <- CoRRE_abund[CoRRE_abund$project_name == "pplots",]
sev_EDGE <- CoRRE_abund[CoRRE_abund$site_code == "SEV" & CoRRE_abund$project_name == "EDGE",]

# Is the species present in the pretreatment year?
yr_0 <- pplots[pplots$treatment_year == 0, c(6,8,10)]
yr_0$pres_0 <- 1 # 1 yes
df <- merge(pplots, yr_0, by = c("treatment", "plot_id", "species_matched"), all = TRUE)
df[is.na(df)] <- 0 # 0 no
df$trt_plot <- paste(df$treatment, df$plot_id)

## IDEA: Species which were present in year 0 in some plots, but invaded in others. Success differences?
pdf(here::here("Figures", "Invaded_Rel_Cov.pdf"), height = 7, width = 14)
ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 0,]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Invaded after year 0") + 
  theme_classic()
dev.off()

pdf(here::here("Figures", "Present0_Rel_Cov.pdf"), height = 7, width = 14)
ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 1,]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Present Year 0") +
  theme_classic()
dev.off()

ggplot(aes(x = treatment_year, y = relcov), data =df) + 
  geom_line(aes(color = species_matched, lty = factor(pres_0))) +
  scale_linetype_manual(values = c("dotted", "solid")) + 
  facet_wrap(~trt_plot) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  theme_classic()

sp_freq <- count(yr_0, vars = c("treatment", "genus_species"))
uni_sp <- as.character(unique(sp_freq[sp_freq$freq <6 & sp_freq$treatment != "N1P0","genus_species" ]))

df1 <- df[df$genus_species %in% uni_sp & df$treatment != "N1P0",]
df1$trt_plot <- paste(df1$treatment, df1$plot)
df2 <- df1[df1$pres_0 == 1, ]
df3 <- df1[df1$pres_0 == 0, ]


ggplot(aes(x = treatment_year, y = relcov), data = df2) + 
  geom_line(aes(color = genus_species)) + 
  facet_wrap( ~ trt_plot) +
  guides(color = FALSE) +
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  theme_pubr()

ggplot(aes(x = treatment_year, y = relcov), data = df1) + 
  geom_line(aes(lty = treatment, color = plot_id)) + 
  facet_wrap( ~ genus_species) +
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  guides(color = FALSE) +
  theme_pubr()

ggplot(aes(x = treatment_year, y = relcov), data = df3) + 
  geom_line(aes(lty = treatment, color = plot_id)) + 
  facet_wrap( ~ genus_species) +
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  guides(color = FALSE) +
  theme_pubr()


controls <- df[df$treatment == "N1P0",]

ggplot(aes(x = treatment_year, y = relcov), data = controls) + 
  geom_point(aes(color=genus_species)) + 
  geom_smooth(aes(color = genus_species), method = "lm")

pretreat <- pplots[pplots$treatment_year == 0,]
pretreat <- aggregate(pretreat$relcov, by = list(species = pretreat$genus_species, trt = pretreat$treatment), FUN = mean)
names(pretreat)[3] <- "yr0_abund"
yr12 <- pplots[pplots$treatment_year == 12,]
yr12 <- aggregate(yr12$relcov, by = list(species = yr12$genus_species, trt = yr12$treatment), FUN = mean)
names(yr12)[3] <- "yr12_abund"
df4 <- merge(pretreat, yr12, by = c("species", "trt"), all = TRUE)
df4[is.na(df3)] <- 0

ggplot(aes(x = yr0_abund, y = yr12_abund), data = df4) + 
  geom_point(aes(color = species)) + 
  geom_vline(xintercept = 0, lty = "dashed") + 
  facet_wrap(~trt) +
  guides(color = FALSE) +
  xlab("Pretreatment Abundance") + ylab("Year 12 Abundance") +
  theme_pubr()



## SEV EDGE
sev_EDGE <- sev_EDGE[sev_EDGE$treatment != "del",]
yr_0 <- sev_EDGE[sev_EDGE$treatment_year == 0, c(6, 8,10)]
yr_0$pres_0 <- 1 # 1 yes
df <- merge(sev_EDGE, yr_0, by = c("treatment", "plot_id", "species_matched"), all = TRUE)
df[is.na(df)] <- 0 # 0 no
df$trt_plot <- paste(df$treatment, df$plot_id)

## IDEA: Species which were present in year 0 in some plots, but invaded in others. Success differences?

ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 0,]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Invaded after year 0") + 
  theme_classic()

ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 1,]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Present Year 0") +
  theme_classic()

ggplot(aes(x = treatment_year, y = relcov), data =df) + 
  geom_line(aes(color = species_matched, lty = factor(pres_0))) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  facet_wrap(~trt_plot) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  theme_classic()

sp_freq <- count(yr_0, vars = c("treatment", "species_matched"))
uni_sp <- as.character(unique(sp_freq[sp_freq$freq <6 & sp_freq$treatment != "con","species_matched" ]))

df1 <- df[df$species_matched %in% uni_sp & df$treatment != "con",]
df1$trt_plot <- paste(df1$treatment, df1$plot)
df2 <- df1[df1$pres_0 == 1, ]
df3 <- df1[df1$pres_0 == 0, ]


ggplot(aes(x = treatment_year, y = relcov), data = df2) + 
  geom_line(aes(color = species_matched)) + 
  facet_wrap( ~ trt_plot) +
  scale_x_continuous(breaks = c(0,1,3,5)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  theme_pubr()

ggplot(aes(x = treatment_year, y = relcov), data = df1) + 
  geom_line(aes( color = plot_id)) + 
  facet_wrap( ~ species_matched) +
  scale_x_continuous(breaks = c(0,1,3,5)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  guides(color = FALSE) +
  theme_pubr()

ggplot(aes(x = treatment_year, y = relcov), data = df3) + 
  geom_line(aes( color = plot_id)) + 
  facet_wrap( ~ species_matched) +
  scale_x_continuous(breaks = c(0,1,3,5)) +
  ylab("Relative Cover") + xlab("Treatment Year") +
  guides(color = FALSE) +
  theme_pubr()


controls <- df[df$treatment == "N1P0",]

ggplot(aes(x = treatment_year, y = relcov), data = controls) + 
  geom_point(aes(color=genus_species)) + 
  geom_smooth(aes(color = genus_species), method = "lm")
