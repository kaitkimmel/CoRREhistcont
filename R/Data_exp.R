library(ggplot2)
library(ggpubr)
library(here)
library(plyr)
library(colorspace)

CoRRE_abund <- read.csv(here::here("Data", "CoRRE_relative_abundance_Nov2019.csv"), row.names = 1)
CoRRE_sp <- read.csv(here::here("Data", "CoRRE_TRY_species_list.csv"), row.names = 1)
CoRRE_abund <- merge(CoRRE_abund, CoRRE_sp[,c(1,2)], by = "genus_species")
CoRRE_abund <- CoRRE_abund[,-1]
pplots <- CoRRE_abund[CoRRE_abund$project_name == "pplots",]
chy_EDGE <- CoRRE_abund[CoRRE_abund$site_code == "CHY" & CoRRE_abund$project_name == "EDGE",]

#### Is the species present in the pretreatment year? ####
yr_0 <- pplots[pplots$treatment_year == 0, c(6,8,10)] #species present in year 0 in any plot
yr_0$pres_0 <- "yes" # yes
# merge with full pplots data
df <- merge(pplots, yr_0, by = c("treatment", "plot_id", "species_matched"), all = TRUE)
df[is.na(df)] <- "no" # no
df$trt_plot <- paste(df$treatment, df$plot_id)

#### some exploratory plots ####

# species that invaded after year 0 #
ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 'no',]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Invaded after year 0") + 
  theme_classic()

# species present in pre-treatment year #
ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 'yes',]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Present Year 0") +
  theme_classic()
## looking at each plot
ggplot(aes(x = treatment_year, y = relcov), data =df) + 
  geom_line(aes(color = species_matched, lty = factor(pres_0), alpha = factor(pres_0))) +
  scale_linetype_manual(values = c("dotted", "solid")) + 
  scale_alpha_manual(values = c(1, 0.5)) +
  facet_wrap(~trt_plot) + 
  guides(color = FALSE) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  xlab("Treatment Year") + ylab("Relative Cover") +
  theme_classic()

# mean relative abundance in year 0
pretreat <- pplots[pplots$treatment_year == 0,]
pretreat <- aggregate(pretreat$relcov, by = list(species = pretreat$species_matched, trt = pretreat$treatment), FUN = mean)
names(pretreat)[3] <- "yr0_abund"
# mean relative abundance in year 12
yr12 <- pplots[pplots$treatment_year == 12,]
yr12 <- aggregate(yr12$relcov, by = list(species = yr12$species_matched, trt = yr12$treatment), FUN = mean)
names(yr12)[3] <- "yr12_abund"
# merge
df4 <- merge(pretreat, yr12, by = c("species", "trt"), all = TRUE)
df4[is.na(df4)] <- 0

ggplot(aes(x = yr0_abund, y = yr12_abund), data = df4) + 
  geom_point(aes(color = species)) + 
  #geom_abline(slope = 1, intercept = 0) + 
  geom_hline(yintercept = 0.25) + 
  geom_vline (xintercept = 0.05) + 
  facet_wrap(~trt) +
  guides(color = FALSE) +
  xlab("Pretreatment Abundance") + ylab("Year 12 Abundance") +
  theme_pubr()

## look at absolute instead of mean for species
year_0 <- pplots[pplots$treatment_year == 0,]
last_years <- pplots[pplots$treatment_year >9,]
last_years <- aggregate(last_years$relcov, by = list(plot_id = last_years$plot_id, treatment = last_years$treatment, species_matched = last_years$species_matched), FUN = mean)
names(last_years)[4] <- "relcov_last"
newdf <- merge(year_0, last_years, all = TRUE)
newdf$relcov[is.na(newdf$relcov)] <- 0
newdf$relcov_last[is.na(newdf$relcov_last)] <- 0
newdf$diff <- newdf$relcov_last - newdf$relcov

newdf$alp <- 1
newdf$alp[newdf$diff<0] <- 0
newdf$alp <- as.factor(newdf$alp)
poly_pts <- as.data.frame(cbind(x = c(0.0001, 0.0001, 0.1, 0.1), 
                  y = c(0.1, 0.7, 0.7, .1)))


ggplot(aes(x = relcov, y = relcov_last), data = newdf[newdf$relcov != 0 & newdf$treatment != "N1P0",]) + 
  geom_polygon (aes(x = x, y = y), data = poly_pts, fill = "yellow", color = "black") + 
  geom_point(aes(color = species_matched, alpha = as.factor(alp))) + 
  facet_wrap (~ treatment) + 
  scale_color_discrete_divergingx(palette = "Spectral") + 
  guides(color = FALSE, alpha = FALSE) + 
  labs(x = "Pretreatment Relative Cover", y = "Last 3 Years Mean Realtive Cover") + 
  theme_pubr()

priority_sp <- newdf[newdf$relcov >0,]
priority_sp <- priority_sp[priority_sp$relcov < 0.05,]
priority_sp <- priority_sp[priority_sp$relcov_last > 0.1,]
unique(priority_sp$species_matched)

invade_sp <- newdf[newdf$relcov == 0,]
invade_sp <- invade_sp[invade_sp$relcov_last >0.1,]

## chy EDGE
chy_EDGE$treatment <- factor(chy_EDGE$treatment, levels = c("con", "chr", "int"), labels = c("control", "chronic", "intense") )
yr_0 <- chy_EDGE[chy_EDGE$treatment_year == 0, c(6, 8,10)]
yr_0$pres_0 <- "yes" # yes
df <- merge(chy_EDGE, yr_0, by = c("treatment", "plot_id", "species_matched"), all = TRUE)
df[is.na(df)] <- "no" # no
df$trt_plot <- paste(df$treatment, df$plot_id)

ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 'no',]) + 
  geom_line(aes(color = plot_id, lty = treatment)) +
  facet_wrap(~species_matched) + 
  guides(color = FALSE) + 
  xlab("Treatment Year") + ylab("Relative Cover") +
  ggtitle("Invaded after year 0") + 
  theme_classic()

ggplot(aes(x = treatment_year, y = relcov), data = df[df$pres_0 == 'yes',]) + 
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

# mean relative abundance in year 0
pretreat <- chy_EDGE[chy_EDGE$treatment_year == 0,]
pretreat <- aggregate(pretreat$relcov, by = list(species = pretreat$species_matched, trt = pretreat$treatment), FUN = mean)
names(pretreat)[3] <- "yr0_abund"
# mean relative abundance in year 12
yr12 <- chy_EDGE[chy_EDGE$treatment_year == 4,]
yr12 <- aggregate(yr12$relcov, by = list(species = yr12$species_matched, trt = yr12$treatment), FUN = mean)
names(yr12)[3] <- "yr12_abund"
# merge
df4 <- merge(pretreat, yr12, by = c("species", "trt"), all = TRUE)
df4[is.na(df4)] <- 0

ggplot(aes(x = yr0_abund, y = yr12_abund), data = df4[df4$yr0_abund != 0,]) + 
  geom_point(aes(color = species)) + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~trt) +
  guides(color = FALSE) +
  xlab("Pretreatment Abundance") + ylab("Year 12 Abundance") +
  theme_pubr()

## look at absolute instead of mean for species
year_0 <- chy_EDGE[chy_EDGE$treatment_year == 0,]
last_years <- chy_EDGE[chy_EDGE$treatment_year == 4,]
names(last_years)[9] <- "relcov_last"
last_years <- last_years[,c(6,8:10)]
newdf <- merge(year_0, last_years, all = TRUE)
newdf$relcov[is.na(newdf$relcov)] <- 0
newdf$relcov_last[is.na(newdf$relcov_last)] <- 0
newdf$diff <- newdf$relcov_last - newdf$relcov

newdf$alp <- 1
newdf$alp[newdf$diff<0] <- 0
newdf$alp <- as.factor(newdf$alp)
poly_pts <- as.data.frame(cbind(x = c(0.0001, 0.0001, 0.1, 0.1), 
                                y = c(0.1, 0.4, 0.4, .1)))


ggplot(aes(x = relcov, y = relcov_last), data = newdf[newdf$relcov != 0,]) + 
  geom_polygon (aes(x = x, y = y), data = poly_pts, fill = "yellow", color = "black") + 
  geom_point(aes(color = species_matched, alpha = as.factor(alp))) + 
  facet_wrap (~ treatment) + 
  guides(color = FALSE, alpha = FALSE) + 
  scale_color_discrete_divergingx(palette = "Zissou 1") + 
  labs(x = "Pretreatment Relative Cover", y = "Year 4 Realtive Cover") + 
  theme_pubr()

priority_sp <- newdf[newdf$relcov >0,]
priority_sp <- priority_sp[priority_sp$relcov < 0.05,]
priority_sp <- priority_sp[priority_sp$relcov_last > 0.1,]
priority_sp <- priority_sp[priority_sp$treatment != "control",]
unique(priority_sp$species_matched)

invade_sp <- newdf[newdf$relcov == 0,]
invade_sp <- invade_sp[invade_sp$relcov_last >0.1,]



##############################################
## Old bits, may be useful again sometime ###
#############################################

# # getting counts of how many reps of each treatment every species is in
# sp_freq <- count(yr_0, vars = c("treatment", "species_matched"))
# sp_freq <- count(pplots, vars = c("species_matched", "treatment"))
# 
# uni_sp <- as.character(unique(sp_freq[sp_freq$freq <6 & sp_freq$treatment != "N1P0","species_matched" ]))
# 
# df1 <- df[df$species_matched %in% uni_sp & df$treatment != "N1P0",]
# df1$trt_plot <- paste(df1$treatment, df1$plot)
# df2 <- df1[df1$pres_0 == 1, ]
# df3 <- df1[df1$pres_0 == 0, ]
# 
# 
# ggplot(aes(x = treatment_year, y = relcov), data = df2) + 
#   geom_line(aes(color = species_matched)) + 
#   facet_wrap( ~ trt_plot) +
#   guides(color = FALSE) +
#   scale_x_continuous(breaks = c(0,3,6,9,12)) +
#   ylab("Relative Cover") + xlab("Treatment Year") +
#   theme_pubr()
# 
# ggplot(aes(x = treatment_year, y = relcov), data = df1) + 
#   geom_line(aes(lty = treatment, color = plot_id)) + 
#   facet_wrap( ~ species_matched) +
#   scale_x_continuous(breaks = c(0,3,6,9,12)) +
#   ylab("Relative Cover") + xlab("Treatment Year") +
#   guides(color = FALSE) +
#   theme_pubr()
# 
# ggplot(aes(x = treatment_year, y = relcov), data = df3) + 
#   geom_line(aes(lty = treatment, color = plot_id)) + 
#   facet_wrap( ~ species_matched) +
#   scale_x_continuous(breaks = c(0,3,6,9,12)) +
#   ylab("Relative Cover") + xlab("Treatment Year") +
#   guides(color = FALSE) +
#   theme_pubr()
# 
# 
# controls <- df[df$treatment == "N1P0",]
# 
# ggplot(aes(x = treatment_year, y = relcov), data = controls) + 
#   geom_point(aes(color=species_matched)) + 
#   geom_smooth(aes(color = species_matched), method = "lm")


