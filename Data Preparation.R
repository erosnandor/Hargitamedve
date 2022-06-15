#install.packages('remotes')
# remotes::install_gitlab('bonegabor/rivers-and-barriers', auth_token = 'glpat-_JUuPEv4JcxWDRwLzFHY')
library(rivers.and.barriers)

library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(sf)

# 1. I start with survey points data cleaning


survey_points <- read.csv("data_raw/survey_points_minden_Erdely.csv")
head(survey_points)

# What we don't need

survey_points$X <- NULL
survey_points$X.1 <- NULL
survey_points$X.2 <- NULL


# 2. Let's see the observations

observations <- read.csv("data_raw/observations_minden_Erdely.csv")
head(observations)

# What we don't need

observations$X <- NULL

# 3. Preparing the native/invasive species list

species_list <- openxlsx::read.xlsx("data_raw/species_list_minden.xlsx")
species_list <- species_list %>% filter(Verdict == "keep")

species_list$Invasive.species <- gsub("x", "invasive", species_list$Invasive.species)
species_list$Native.species <- gsub("x", "native", species_list$Native.species)
species_list$Natura.2000.species <- gsub("x", "Natura2000", species_list$Natura.2000.species)

# Decide status
species_list$Status = NA

for (i in seq_len(nrow(species_list))) {

  if (!is.na(species_list$Invasive.species[i])) {
    species_list$Status[i] <- species_list$Invasive.species[i]
  }
  else {
    species_list$Status[i] <- ifelse(!is.na(species_list$Natura.2000.species[i]), "N2000", "native")
  }
}

species_list$Status


# Expectations for the final data set
# Variables:
# Species_name
# Nr_indiv
# Invasive_native ==> Status
# N2000_species ==> Status
# Point_ID
# Year
# Inside_N2000
# N2000_code
# ASL (m)
# Surface (m^2)
# Water_speed
# CCM2_rank
# Autocorrelation

# 1st filtering
obs_filtered <- observations %>%
  select(survey_station_code, species, total_no) %>%
  filter(species != "0")

head(obs_filtered)

# Summarise the Sabanejewia species

# replace the strings

obs_filtered$species <- gsub("Sabanejewia [a-z]*", "Sabanejewia sp.", obs_filtered$species)

# summarise the no of indivs at each station
n_old = nrow(obs_filtered)

obs_filtered = obs_filtered %>%
  group_by(survey_station_code, species) %>%
  summarise(Nr_indiv = sum(total_no))

n_new = nrow(obs_filtered)

n_new < n_old # summarization was successfull

# Rename the columns
colnames(obs_filtered) = c("Point_ID", "Species", "Nr_indiv")
head(obs_filtered, 2)

# add the Status

species_list$Species <- gsub("Sabanejewia aurata", "Sabanejewia sp.", species_list$Species)
species_list$Species <- gsub("Sabanejewia balcanica", "Sabanejewia sp.", species_list$Species)
species_list$Species <- gsub("Sabanejewia bulgarica", "Sabanejewia sp.", species_list$Species)

species_list$Observations.HU = NULL
species_list$Observations.RO = NULL

which(species_list$Species == "Sabanejewia sp.")
species_list <- species_list[c(-56, -57),]

obs_filtered <- obs_filtered %>% left_join(species_list %>% select(Species, Status), by = "Species")


obs_filtered$Species <- gsub("Sabanejewia aurata", "Sabanejewia sp.", obs_filtered$Species)
obs_filtered$Species <- gsub("Sabanejewia balcanica", "Sabanejewia sp.", obs_filtered$Species)
obs_filtered$Species <- gsub("Sabanejewia bulgarica", "Sabanejewia sp.", obs_filtered$Species)

# Expectations for the final data set
# Variables:
# Inside_N2000
# N2000_code
# ASL (m)
# Surface (m^2)
# Water_speed
# CCM2_rank
# Autocorrelation
# X_coord
# Y_coord

head(survey_points)
survey_selected <- survey_points %>% select(survey_station_code,
                                            basin, river, area_surface, depth_mean, ws_stagnant, altitude, sci, "geometry.x", "geometry.y")

# Inside_Natura2000

survey_selected$Inside_Natura2000 <- ifelse(is.na(survey_selected$sci), 0, 1)

# N2000 code: 9 characters
survey_selected$N2000_code <- substring(survey_selected$sci, first = 1, last = 9)
survey_selected$N2000_code[is.na(survey_selected$N2000_code)] <- "None"

# Output 1: Dataset with species number per survey point
colnames(survey_selected)
colnames(survey_selected) <- c("Point_ID", "Basin", "River", "Surface", "Depth_mean", "Stagnant_prop",
                               "Altitude", "N2000_site", "X", "Y", "Inside_N2000", "N2000_code")


spec_number <- obs_filtered %>% left_join(survey_selected, by = "Point_ID")

View(spec_number)

spec_number_temp <- spec_number %>% group_by(Point_ID) %>% mutate(n = 1) %>% summarise(Sp_nr = sum(n))

spec_number <- spec_number_temp %>% left_join(survey_selected, by = "Point_ID")
spec_number$Sp_nr_norm <- spec_number$Sp_nr/spec_number$Surface


spec_number %>% filter(Sp_nr_norm <= 0.1) %>%
  ggplot() +
    geom_point(aes(x = Altitude, y = Sp_nr_norm), size = 1.5, alpha = 0.6) +
    geom_smooth(aes(x = Altitude, y = Sp_nr_norm), size = 1.3) +
  scale_x_continuous(limits = c(0, 1400), breaks = seq(0, 1400, by = 150)) +
  labs(x = "Altitude above sea level (m)", y = "Species number per sq. meters") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.line = element_line(color = "black"))

ggsave("visualizations/spec_nr.jpg", dpi = 300)


# Shannon diversity

shannon_div <- obs_filtered %>% left_join(survey_selected, by = "Point_ID") %>%
  group_by(Point_ID, Species) %>% summarise(Nr_indiv = sum(Nr_indiv)) %>% ungroup() %>%
  group_by(Point_ID) %>% mutate(Prop = Nr_indiv/sum(Nr_indiv)) %>% mutate(lnProp = log(Prop)) %>% ungroup() %>%
  group_by(Point_ID) %>% mutate(H = -1 * sum(Prop * lnProp)) %>% select(Point_ID, H) %>% distinct()

shannon_div <- shannon_div %>% left_join(survey_selected, by = "Point_ID")
shannon_div$H_norm <- shannon_div$H / shannon_div$Surface

shannon_div %>% filter(H_norm <= 0.02) %>%
  ggplot() +
  geom_point(aes(x = Altitude, y = H_norm), size = 1.5, alpha = 0.6) +
  geom_smooth(aes(x = Altitude, y = H_norm), size = 1.3) +
  scale_x_continuous(limits = c(0, 1400), breaks = seq(0, 1400, by = 150)) +
  labs(x = "Altitude above sea level (m)", y = "Shannon-Wiener diversity index \nper sq. meters") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.line = element_line(color = "black"))

ggsave("visualizations/shannon.jpg", dpi = 300)