# Author: Julia Liebell-McLean
# Date: November 19, 2025
# Title: Practicum 2
# Purpose: Reads in data about high points, county geometry, area, and population.
# Produces maps of high points, population, density, and presidential voting outcomes.

library(devtools)
library(maps)
library(tidyverse)
library(socviz)
library(scales)
library(cowplot)
library(tidycensus)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(sf)

# Set the working directory
setwd("C:\\Users\\julia\\OneDrive\\Documents\\GitHub\\Practicum-2")

# Read in the county shapefile data
county_all <- st_read("data/cb_2024_us_county_500k.shp")

# Filter to just New York
county <- county_all |>
  filter(STATEFP == 36)

# Select the values for GEOID, name, land area, and geometry
county <- county |>
  select(GEOID, NAME, ALAND, geometry)

# Load 2020 population for NY counties, without geometry
census <- get_decennial(
  geography = "county",
  variables = "P1_001N",   # total population
  state = "NY",
  year = 2020,
  geometry = FALSE
)

#Look at the data
census

# Rename the variable, drop columns that will be redundant after the merge
population <- census |>
  rename("population" = "value") |>
  select(-variable, -NAME)

# Merge, using FIPS code, the population data onto the shapefile data
county <- county |>
  left_join(population, by = "GEOID")

# Check the merge
county

# Identify the smallest and largest counties
county_order <- county |>
  arrange(desc(ALAND))
big <- county_order[1,2]
small <- county_order[62,2]

# Identify the most and least populous counties
county_order <- county |>
  arrange(desc(population))
most_pop <- county_order[1,2]
least_pop <- county_order[62,2]

# Calculate population density by dividing population by area
county <- county |>
  mutate(density = population/ALAND)

# Identify the most and least dense counties
county_order <- county |>
  arrange(desc(density))
most_dense <- county_order[1,2]
least_dense <- county_order[62,2]

# Draw a map with the 62 New York counties
p <- ggplot(county) + 
  geom_sf(fill = "gray", color = "white") +
  labs(title = "New York State Counties") + 
  theme_minimal() +
  theme(legend.position = "none")
p

ggsave("figures/counties.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)

# Draw a map of the population
# Specify the max population
vmax <- max(county$population, na.rm = TRUE)

#Build plot for population with scale transform, breaks, and legend simplification
p  <- ggplot(county, aes(fill = population)); p
p1 <- p + geom_sf(color = "white", linewidth = 0.05) + coord_sf(datum = NA); p1
p2 <- p1 +
  scale_fill_gradient(
    trans  = "log10",
    limits = c(1e3, vmax), 
    breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7),
    oob    = squish,  
    labels = label_number(scale_cut = cut_short_scale()),
    low    = "lightyellow",
    high   = "darkorange"
  ) +
  labs(title = "Population by County, 2020", fill = NULL); p2
p3 <- p2 +
  theme_map() +
  guides(fill = guide_colorbar(barheight = unit(60, "pt"))) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

ggsave("figures/population.png", plot = p3, width = 8, height = 6, units = "in", dpi = 300)

# Now do the same for population density
# Specify the max population density
vmin <- min(county$density)
vmax <- max(county$density, na.rm = TRUE)

#Build plot for population with scale transform, breaks, and legend simplification
p  <- ggplot(county, aes(fill = density)); p
p1 <- p + geom_sf(color = "white", linewidth = 0.05) + coord_sf(datum = NA); p1
p2 <- p1 +
  scale_fill_gradient(
    trans  = "log10",
    limits = c(vmin, vmax),
    oob    = squish,  
    labels = label_number(scale_cut = cut_short_scale()),
    low    = "lavender",
    high   = "purple4"
  ) +
  labs(title = "Population Density by County, 2020", fill = NULL); p2
p3 <- p2 +
  theme_map() +
  guides(fill = guide_colorbar(barheight = unit(60, "pt"))) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
ggsave("figures/density.png", plot = p3, width = 8, height = 6, units = "in", dpi = 300)

# Read in the data about highest points
points <- read.csv("data/nyshighpoints.csv")

# Rename St. Lawrence County to match the census and shapefile data
points[15,1] = "St. Lawrence"

# Rename the county column to NAME to match census data
points <- points |>
  rename("NAME" = "County")

# Merge the points data onto the county data
peaks <- county |>
  right_join(points, by= "NAME")

# Check the merge
peaks

# Drop some columns
peaks <- peaks |>
  select(-ALAND, -population, -density, -Elevation_Meters, -Data_Quality)

# Identify the highest and shortest high peaks
peak_order <- peaks |>
  arrange(desc(Elevation_Feet))
tall <- peak_order[1,]
short <- peak_order[63,]

# Plot a map of NYS with the counties and the highest peaks

# Specify the min and max elevations
pmin <- min(peaks$Elevation_Feet)
pmax <- max(peaks$Elevation_Feet)

#Build plot for the highest peaks
p <- ggplot(peaks) + 
  geom_sf(fill = "gray", color = "white") +
  labs(title = "NYS Highest Peaks") + 
  theme_minimal()
p

# Add on the points
p1 <- p + 
  geom_point(data = peaks,
             aes(x = Longitude, y = Latitude, color = Elevation_Feet),
             size = 3,
             shape = 17,
             inherit.aes = FALSE) +
  scale_color_gradientn(
    colors = c("lightgreen","yellow","brown", "white"))
p1

# Label the highest point
p2 <- p1 +  
  geom_text(data = peak_order[1,],
            aes(x = Longitude, y = Latitude, label = High_Point_Name),
            size = 3, vjust = 1.5, check_overlap = TRUE,
            fontface = "bold",
            inherit.aes = FALSE)
p2 

# Save the peaks figure
ggsave("figures/peaks.png", plot = p2, width = 8, height = 6, units = "in", dpi = 300)

# Create maps of presidential election results

# Load data about presidential election results from Carlos. This is publicly available 
# at https://dataverse.harvard.edu/file.xhtml?fileId=5028532&version=1.1. A copy 
# is saved to the data file.
load("data/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

# Specify which years we are interested in
years <- c(2000, 2004, 2008, 2012, 2016, 2020)

# Select the interesting categories
pres <- pres_elections_release |>
  select(election_year, fips, sfips, office, election_type,
         democratic_raw_votes, republican_raw_votes,
         pres_raw_county_vote_totals_two_party)
  
# Select for the right years
pres <- pres |>
  filter(election_year %in% years)

# Double check that the office and election_type categories are meaningless
# unique(pres$office)
# unique(pres$election_type)
# Yep, they're all "G" and "PRES"

# Drop some columns and filter to NY
pres <- pres |>
  filter(sfips == 36) |>
  select(-office, -election_type)

# Double check that the total two party vote number is correct
# pres1 <- pres |>
  # mutate(total = democratic_raw_votes + republican_raw_votes,
         # check = pres_raw_county_vote_totals_two_party - total)
# unique(pres1$check)

# Calculate the percent of Democrat and Republican votes and partisan lean
pres2 <- pres |>
  mutate(pct_dem = democratic_raw_votes/pres_raw_county_vote_totals_two_party,
         pct_rep = republican_raw_votes/pres_raw_county_vote_totals_two_party,
         d_points = pct_dem - pct_rep)|>
  # Drop everything except the lean, county ID, and the election years
  select(election_year, fips, d_points) |>
  # Rename the fips code for merging
  rename(GEOID = fips)
  
# Isolate the geometry and fips code
geo <- county |>
  select(GEOID, geometry)

# Add on the geometry to the election results
pres2 <- pres2 |>
  left_join(geo, by = "GEOID")

# I got the idea for using facet_wrap from Copilot and then adjusted the code it
# suggested. 

# Graph the partisan lean in a given year, by county, for New York state
p <- ggplot(pres2, aes(fill = d_points, geometry = geometry)) +
  geom_sf(color = "grey", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#CB454A", mid = "white", high = "#2E74C0",
    midpoint = 0,
    name = "Partisan Lean"
  ) +
  facet_wrap(~ election_year, ncol = 3) +
  labs(title = "Partisan Lean in New York Counties, 2000–2020") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
p

# Save the figure
ggsave("figures/partisan.png", plot = p, width = 9, height = 5, units = "in", dpi = 300)