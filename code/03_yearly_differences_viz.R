library(readxl)
library(sf)
library(tidyverse)
library(mapview)
library(ggpubr)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

# brazil amazon from IBGE
brazil_amazon <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Limites_Amazonia_Legal_2022_shp/Limites_Amazonia_Legal_2022.shp")
brazil_amazon$geometry <- st_transform(brazil_amazon$geometry, 4326)
brazil_amazon_municipalities <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Mun_Amazonia_Legal_2022_shp/Mun_Amazonia_Legal_2022.shp")
brazil_amazon_municipalities$geometry <- st_transform(brazil_amazon_municipalities$geometry, 4326) %>% 
  st_simplify(dTolerance = 75) 

# navigable waterways
waterways <- st_read("~/Desktop/doctorate/ch3 amazon network/data/BaseHidroHidrovias/fc_hidro_hidrovia_antaq.shp")
waterways$geometry <- st_transform(st_zm(waterways$geometry), 4326)
waterways_bool <- st_covers(brazil_amazon,waterways$geometry, sparse = FALSE)
waterways_amazon <- waterways[waterways_bool[1,],]
waterways_amazon_navegavel <- waterways_amazon[which(waterways_amazon$cla_icacao=="Navegável"),]
mapview(waterways_amazon_navegavel)

##################################################
# load in all yearly base maps and differences
##################################################
years <- c(2001:2023)
for (year in years){
  year_index <- match(year,years)
  year_plus_one <- years[year_index+1]
  year_plus_one <- ifelse(year==2023, 2024, year_plus_one)
  DNIT_year_unions <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/DNIT_", year,"_unions.shp"))
  DNIT_year_differences <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_differences/DNIT_", year, "_", year_plus_one, "_differences.shp"))
  
  year_diff_map <- ggplot() +
    geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
    geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
    geom_sf(data = DNIT_year_unions, aes(geometry = geometry), color='#ffc500', linewidth=0.8) +
    geom_sf(data = DNIT_year_differences, aes(geometry = geometry), color='#CD1076FF', linewidth=0.8) +
    ggtitle(year_plus_one) +
    theme_minimal() +
    no_axis +
    theme(legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position='right')
  new_map_name <- paste0("DNIT_", year, "_", year_plus_one, "_differences_map")
  assign(new_map_name, year_diff_map)
  
  #store for mapview
  union_name <- paste0("DNIT_", year, "_unions")
  assign(union_name, DNIT_year_unions)
  
  difference_name <- paste0("DNIT_", year, "_", year_plus_one, "_differences")
  assign(difference_name, DNIT_year_differences)
  
}

DNIT_2001_2024_differences <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_differences/DNIT_", 2001, "_", 2024, "_differences.shp"))


DNIT_2001_unions <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/DNIT_2001_unions.shp"))
just_2001_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_2001_unions, aes(geometry = geometry), color='#ffc500', linewidth=0.8) +
  ggtitle(2001) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')

plot_panel <- ggarrange(just_2001_map,
  DNIT_2001_2002_differences_map, DNIT_2002_2003_differences_map, DNIT_2003_2004_differences_map, DNIT_2004_2005_differences_map, DNIT_2005_2006_differences_map,
  DNIT_2006_2007_differences_map, DNIT_2007_2008_differences_map, DNIT_2008_2009_differences_map, DNIT_2009_2010_differences_map, DNIT_2010_2011_differences_map,
  DNIT_2011_2012_differences_map, DNIT_2012_2013_differences_map, DNIT_2013_2014_differences_map, DNIT_2014_2015_differences_map, DNIT_2015_2016_differences_map,
  DNIT_2016_2017_differences_map, DNIT_2017_2018_differences_map, DNIT_2018_2019_differences_map, DNIT_2019_2020_differences_map, DNIT_2020_2021_differences_map,
  DNIT_2021_2022_differences_map, DNIT_2022_2023_differences_map, DNIT_2023_2024_differences_map, ncol = 4, nrow = 6
)

ggsave("~/Desktop/panel_of_maps_425.pdf", plot_panel, width = 13, height = 17)
#ggsave("~/Desktop/panel_of_maps.png", plot_panel, width = 13, height = 17)


DNIT_year_unions <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_unions/DNIT_2015_unions.shp"))
DNIT_year_differences <- st_read(paste0("~/Desktop/doctorate/ch3 amazon network/data/DNIT_processed/DNIT_differences/DNIT_2015_2016_differences.shp"))

year_diff_map <- ggplot() +
  geom_sf(data = brazil_amazon_municipalities, fill='white', color='lightgrey', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_amazon, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = DNIT_year_unions, aes(geometry = geometry), color='#ffc500', linewidth=0.8) +
  #geom_sf(data = DNIT_year_differences, aes(geometry = geometry), color='#CD1076FF', linewidth=0.8) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='right')
year_diff_map

# Create output folder if it doesn't exist
output_folder <- "~/Desktop/doctorate/ch3 amazon network/presentation_figures/yearly_maps_pdfs"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Save 2001 map separately
ggsave(
  filename = file.path(output_folder, "DNIT_2001_map.pdf"),
  plot = just_2001_map,
  width = 6, height = 7
)

# Export maps for 2002–2024 (based on 2001–2023 differences)
years <- 2001:2023

for (year in years) {
  year_index <- match(year, years)
  year_plus_one <- ifelse(year == 2023, 2024, years[year_index + 1])
  
  map_name <- paste0("DNIT_", year, "_", year_plus_one, "_differences_map")
  
  if (exists(map_name)) {
    map_to_save <- get(map_name)
    filename <- file.path(output_folder, paste0(map_name, ".pdf"))
    ggsave(filename, map_to_save, width = 6, height = 7)
  } else {
    warning(paste("Map object", map_name, "does not exist. Skipping."))
  }
}
