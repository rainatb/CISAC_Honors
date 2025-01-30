# lit review plots

lepto_cases <- read.csv('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/finalized_lepto_cases_df.csv')
lepto_cases <- lepto_cases[, -1]

getwd()
setwd('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/month_municipality_data_2007_2024')

# adding in Brazil shapefiles
library(sf)
municipalities <- read_sf('/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/BR_Municipios_2021/BR_Municipios_2021.shp')
municipalities <- st_as_sf(municipalities)
municipalities$geometry <- st_transform(municipalities$geometry, 4326)
library(ggplot2)

library(dplyr)
library(tidyverse)
library(lubridate)
lepto_grouped_df <- lepto_cases %>%
  group_by(Month, Year) %>%
  summarise(Total_Cases = sum(Cases, na.rm = TRUE))

lepto_grouped_df <- lepto_grouped_df %>%
  mutate(Date = make_date(Year, Month, 1))

lepto_case_over_time <- ggplot(lepto_grouped_df, aes(x = Date, y = Total_Cases)) +
  geom_line(color = "blue", size = 1) +      
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +  
  labs(title = " Leptospirosis Cases in Brazil Over Time",
       x = "Month",
       y = "Total Cases") +
  theme_classic() +                           
  theme(axis.text.x = element_text(angle = 45, hjust = 1),   
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
        axis.title = element_text(size = 12),   
        axis.text = element_text(size = 10)) 

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/lepto_cases_over_time.png", lepto_case_over_time, width = 7, height=5, dpi = 300)



require(readxl)
require(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
library(tidyverse)
library(plm)
library(fixest)
library(geobr)
library(geojsonsf)
library(RColorBrewer)
library(viridis) 
library(cowplot)

brazil_geometries <- municipalities
brazil_geometries <- brazil_geometries[, -2]
brazil_geometries <- brazil_geometries[, -2]
brazil_geometries <- brazil_geometries[, -2]

remove_seventh_digit <- function(cd_mun) {
  cd_mun_str <- as.character(cd_mun)
  if (nchar(cd_mun_str) >= 7) {
    return(paste0(substr(cd_mun_str, 1, 6), substr(cd_mun_str, 8, nchar(cd_mun_str))))
  } else {
    return(cd_mun_str)  # Return as is if less than 7 characters
  }
}

brazil_geometries$CD_MUN <- sapply(brazil_geometries$CD_MUN, remove_seventh_digit)

brazil_geometries$CD_MUN <- as.numeric(brazil_geometries$CD_MUN)

lepto_cases_w_geo <- full_join(lepto_cases, brazil_geometries, by=c("Municipality" = "CD_MUN")) %>%
  st_sf

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

lepto_cases_w_geo_unique <- lepto_cases_w_geo %>%
  group_by(Municipality, geometry, Year) %>%
  summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
  ungroup()

lepto_wide <- lepto_cases_w_geo_unique %>%
  pivot_wider(names_from = Year, values_from = Cases)
lepto_wide <- lepto_wide[, -13]

lepto_wide[is.na(lepto_wide)] <- 0


lepto_2007 <- ggplot() +
  geom_sf(data=lepto_wide, aes(fill=`2007`), colour="grey50", linewidth=0.0001) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar",na.value="lightgrey") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/testing_lepto_2007_attempt.png", lepto_2007, width = 7, height=5, dpi = 300)

lepto_2012 <- ggplot() +
  geom_sf(data=lepto_wide, aes(fill=`2012`), colour="grey50", linewidth=0.0001) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar",na.value="lightgrey") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/testing_lepto_2012_attempt.png", lepto_2012, width = 7, height=5, dpi = 300)

lepto_2017 <- ggplot() +
  geom_sf(data=lepto_wide, aes(fill=`2017`), colour="grey50", linewidth=0.0001) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar",na.value="lightgrey") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/testing_lepto_2017_attempt.png", lepto_2017, width = 7, height=5, dpi = 300)

lepto_2023 <- ggplot() +
  geom_sf(data=lepto_wide, aes(fill=`2023`), colour="grey50", linewidth=0.0001) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar",na.value="lightgrey") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/testing_lepto_2023_attempt.png", lepto_2023, width = 7, height=5, dpi = 300)

library(gridExtra)

# Arrange the four plots in a 2x2 grid
lit_review_fig2_new <- grid.arrange(
  lepto_2007, lepto_2012,
  lepto_2017, lepto_2023,
  ncol = 2, nrow = 2
)

ggsave("/Users/rashitalwarbhatia/Downloads/Honors Thesis Lepto Work/grid_arranged_lepto_new.png", lit_review_fig2_new, width = 8, height=6, dpi = 400)

