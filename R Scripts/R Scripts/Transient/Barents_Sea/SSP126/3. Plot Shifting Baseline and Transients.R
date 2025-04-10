#rm(list = ls())
library(patchwork)
library(tidyverse)

transient_years <- seq(2010,2050)
all_data <- readRDS("../Recovery Time Manuscript/Objects/Shifting_Baseline_Decadal_0_fishing.RDS")
biomasses <- all_data[["Biomasses"]]
all_data_yearly <- readRDS("../Recovery Time Manuscript/Objects/Shifting_Baseline_Yearly_0_fishing.RDS")
biomasses_yearly <- all_data_yearly[["Biomasses"]]


####



## Now the goal is to make the above plot for all guilds to facet them
useful <- c("Omnivorous_zooplankton",
            "Carnivorous_zooplankton",
            "Planktivorous_fish",
            "Migratory_fish",
            "Demersal_fish",
            "Birds",
            "Pinnipeds",
            "Cetaceans",
            "Maritime_mammals") # Name the guilds we will actually care about

## Compute baseline 
all_biomasses <- data.frame(Model_annual_mean = NA,
                            Units = NA,
                            Description = NA,
                            Year = NA)
for (i in 1:length(biomasses)) {
  tmp <- data.frame(Model_annual_mean = NA,
                    Units = NA,
                    Description = NA)
  tmp_join <- rbind(tmp,biomasses[[i]])
  tmp_join$Year <- transient_years[i]
  all_biomasses <- rbind(all_biomasses,tmp_join)
}

all_biomasses_yearly <- data.frame(Model_annual_mean = NA,
                            Units = NA,
                            Description = NA,
                            Year = NA)
for (i in 1:length(biomasses_yearly)) {
  tmp <- data.frame(Model_annual_mean = NA,
                    Units = NA,
                    Description = NA)
  tmp_join <- rbind(tmp,biomasses_yearly[[i]])
  tmp_join$Year <- transient_years[i]
  all_biomasses_yearly <- rbind(all_biomasses_yearly,tmp_join)
}


names(all_biomasses)[3] <- "Guild"
all_biomasses <- all_biomasses %>% filter(Guild %in% useful)
names(all_biomasses_yearly)[3] <- "Guild"
all_biomasses_yearly <- all_biomasses_yearly %>% filter(Guild %in% useful)

top_level <- readRDS("../Recovery Time Manuscript/Objects/biomass_transients.RDS")
top_level_smooth <- readRDS("../Recovery Time Manuscript/Objects/biomass_transients_smoothed_climate.RDS")
final_changing <- data.frame(FishingRate = NA,
                             Year = NA,
                             Biomass = NA,
                             Guild = NA)
final_changing_smooth <- data.frame(FishingRate = NA,
                             Year = NA,
                             Biomass = NA,
                             Guild = NA)

for (i in 1:length(top_level[[1]][["Biomasses"]])) {
  biomass <- sapply(transient_years, function(y) {
    sapply(top_level, function(tl) tl[["Biomasses"]][[as.character(y)]]$Model_annual_mean[i])
  })
  biomass_smoothed <- sapply(transient_years, function(y) {
    sapply(top_level_smooth, function(tl) tl[["Biomasses"]][[as.character(y)]]$Model_annual_mean[i])
  })
  colnames(biomass) <- transient_years
  colnames(biomass_smoothed) <- transient_years
  
  # Convert the matrix to a dataframe
  biomass_df <- biomass %>% as.data.frame()
  biomass_df$FishingRate <- 0:3
  biomass_smooth_df <- biomass_smoothed %>% as.data.frame()
  biomass_smooth_df$FishingRate <- 0:3
  
  # Convert to long format
  biomass_long <- biomass_df %>%
    pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass") %>% 
    mutate(Guild = top_level[[1]][["Biomasses"]][["2010"]]$Description[i] # add the name - ordering the same each year
    )
  
  biomass_smooth_long <- biomass_smooth_df %>%
    pivot_longer(cols = -FishingRate, names_to = "Year", values_to = "Biomass") %>% 
    mutate(Guild = top_level[[1]][["Biomasses"]][["2010"]]$Description[i] # add the name - ordering the same each year
    )
  # store
  final_changing <- rbind(final_changing,biomass_long)
  final_changing_smooth <- rbind(final_changing_smooth,biomass_smooth_long)
}

biomass_guilds <- final_changing %>% filter(Guild %in% useful)
biomass_smooth_guilds <- final_changing_smooth %>% filter(Guild %in% useful)

ggplot() +
  geom_line(data = all_biomasses,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 0.8) +
  geom_line(data = biomass_guilds,aes(x = as.numeric(Year),y = Biomass,color = as.character(FishingRate),group = FishingRate)) +
  labs(x = "Year",
       y = "Guild Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Guild,scales = "free_y") +
  NULL
ggsave("../Recovery Time Manuscript/Figures/Preliminary/all_guilds_shifting_baseline.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)


# Our recovery time is the time it takes for the 1x fishing line to reach the black line
# so, let's just use the one fishing rate (1x) and calculate that value

biomass_guilds_1x <- biomass_guilds %>% filter(FishingRate == 1)
biomass_guilds_1x_smooth <- biomass_smooth_guilds %>% filter(FishingRate == 1)
ggplot() +
  geom_line(data = all_biomasses,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 0.8) +
  geom_line(data = all_biomasses_yearly,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 0.2) +
  geom_line(data = biomass_guilds_1x_smooth,aes(x = as.numeric(Year),y = Biomass),color = "red",alpha = 1) +
  geom_line(data = biomass_guilds_1x,aes(x = as.numeric(Year),y = Biomass),color = "red",alpha = 0.2) +
  labs(x = "Year",
       y = "Guild Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Guild,scales = "free_y") +
  NULL
ggsave("../Recovery Time Manuscript/Figures/Preliminary/all_guilds_shifting_baseline_1x.png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 200)

## We can also add on the yearly data in a faint colour. We need to load it first
all_data_yearly <- readRDS("../Recovery Time Manuscript/Objects/Shifting_Baseline_Yearly.RDS")


biomasses_yearly <- all_data_yearly[["Biomasses"]]
all_biomasses_yearly <- data.frame(Model_annual_mean = NA,
                            Units = NA,
                            Description = NA,
                            Year = NA)
for (i in 1:length(biomasses_yearly)) {
  tmp <- data.frame(Model_annual_mean = NA,
                    Units = NA,
                    Description = NA)
  tmp_join <- rbind(tmp,biomasses_yearly[[i]])
  tmp_join$Year <- transient_years[i]
  all_biomasses_yearly <- rbind(all_biomasses_yearly,tmp_join)
}
names(all_biomasses_yearly)[3] <- "Guild"
all_biomasses_yearly <- all_biomasses_yearly %>% filter(Guild %in% useful)

window_size <- 11

# Compute the sliding window average
biomass_guilds_1x <- biomass_guilds_1x %>%
  group_by(Guild) %>%
  mutate(SlidingWindowAvg = rollapply(Biomass, width = window_size, FUN = mean, align = 'left', fill = NA, na.rm = TRUE)) %>%
  ungroup()


## Also can add the smoothed line which has the experiment (red line)
ggplot() +
  geom_line(data = all_biomasses,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 1) + # smoothed baseline
  geom_line(data = all_biomasses_yearly,aes(x = Year,y = Model_annual_mean),color = "black",alpha = 0.2) + # yealy baseline
  geom_line(data = biomass_guilds_1x,aes(x = as.numeric(Year),y = Biomass),color = "red",alpha = 0.2) + # yearly crashed biomass
  geom_line(data = biomass_guilds_1x,aes(x = as.numeric(Year),y = SlidingWindowAvg),color = "red",alpha = 1,se = F,linewidth = 0.4) + # smoothed crashed biomass
  labs(x = "Year",
       y = "Guild Biomass",
       color = "Fishing Reintroduction Rate\n (DFHR and PFHR)") +
  theme(legend.position = "top") +
  facet_wrap(~ Guild,scales = "free_y") +
  NULL

