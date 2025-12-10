install.packages("readxl")
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)

VDL_data <- read_excel("VDL_data.xlsx")

total_hectares <- sum(VDL_data$`Site Size (Hectares)`, na.rm = TRUE)
cat("Total VDL Coverage in Scotland:\n")
cat(sprintf("%.2f hectares\n\n", total_hectares))

#location type summary
location_summary <- VDL_data %>%
  group_by(`Location of Site`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))

cat("VDL Coverage by Location Type:\n")
print(location_summary)

#site type summ
site_type_summary <- VDL_data %>%
  group_by(`Site Type`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))
cat("VDL Coverage by Site Type:\n")
print(site_type_summary)

#site age
VDL_data <- VDL_data %>%
    mutate(Era = case_when(
        `Period when site became Vacant or Derelict` == "Unknown" ~ "Unknown",
        grepl("^Pre-1980|^197[0-9]", `Period when site became Vacant or Derelict`) ~ "Pre-1980",
        grepl("^198[0-9]", `Period when site became Vacant or Derelict`) ~ "1980-1989",
        grepl("^199[0-9]", `Period when site became Vacant or Derelict`) ~ "1990-1999",
        grepl("^200[0-9]", `Period when site became Vacant or Derelict`) ~ "2000-2009",
        grepl("^201[0-9]", `Period when site became Vacant or Derelict`) ~ "2010-2019",
        grepl("^202[0-9]", `Period when site became Vacant or Derelict`) ~ "2020+",
        TRUE ~ "Unknown"
    ))

era_summary <- VDL_data %>%
  group_by(Era) %>%
    summarise(
        Count = n(),
        Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
        Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    arrange(factor(Era, levels = c("Pre-1980", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020+", "Unknown")))

cat("VDL Coverage by Era (When Site Became Vacant or Derelict):\n")
print(era_summary)

#development potential summary
development_potential_summary <- VDL_data %>%
  group_by(`Development Potential`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))
cat("VDL Coverage by Development Potential:\n")
print(development_potential_summary)

#Shapefile based map creation where we plot above onto some maps of scotland :D
authority_summary <- VDL_data %>%
  group_by(`Planning Authority`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
    Derelict_Ha = sum(`Site Size (Hectares)`[`Site Type` == "Derelict"], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))

cat("\nVDL Coverage by Planning Authority (Top 15):\n")
print(head(authority_summary, 15))

las_shapefile <- st_read("Local_Authority_Boundaries_-_Scotland/pub_las.shp") #from spatialdata.gov.scot

las_with_vdl <- las_shapefile %>%
  left_join(authority_summary, by = c("local_auth" = "Planning Authority"))

top_10_authorities <- authority_summary %>%
  slice_max(Total_Hectares, n = 10) %>%
  pull(`Planning Authority`)

las_centroids <- las_with_vdl %>%
  filter(local_auth %in% top_10_authorities) %>%
  st_centroid() %>%
  mutate(coords = st_coordinates(.)) %>%
  mutate(x = coords[,1], y = coords[,2]) %>%
  select(local_auth, Total_Hectares, x, y)

#chloropleth map
cplthmap <- ggplot(las_with_vdl) +
  geom_sf(aes(fill = Total_Hectares), color = "black", linewidth = 0.4) +
  scale_fill_viridis_c(name = "Total VDL\n(Hectares)", direction = -1, na.value = "lightgrey") +
  theme_minimal() +
  labs(title = "Vacant and Derelict Land Distribution in Scotland",
       subtitle = "By Local Authority Area (darkest = highest concentration)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    legend.position = "right",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11)
  )
ggsave("VDL_distribution_map_labelled.png", plot = cplthmap, width = 12, height = 14, dpi = 300)

#Central belt map (zoomed basically)
vdl_all <- VDL_data %>%
  filter(!is.na(East) & !is.na(North)) %>%
  st_as_sf(coords = c("East", "North"), crs = 27700)

centralcoords <- st_bbox(c(xmin = 200000, ymin = 630000, xmax = 330000, ymax = 710000), crs = 27700)

lasshp <- st_crop(las_shapefile, centralcoords)
vdlctl <- st_crop(vdl_all, centralcoords)

central_belt_map <- ggplot() +
  geom_sf(data = lasshp, fill = "lightgrey", color = "black", linewidth = 0.5) +
  geom_sf(data = vdlctl, aes(color = `Site Type`), size = 2.5, alpha = 0.7) +
  scale_color_manual(
    name = "Site Type",
    values = c("Derelict" = "#e41a1c", "Vacant Land" = "#377eb8", "Vacant Land and Buildings" = "#4daf4a")
  ) +
  theme_minimal() +
  labs(title = "Vacant and Derelict Land in Scotland's Central Belt",
       subtitle = "All VDL types concentrated in former industrial regions (Glasgow, Lanarkshire, West Dunbartonshire)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3)
  ) +
  xlab("Easting") +
  ylab("Northing")

ggsave("VDL_central_belt_all_types.png", plot = central_belt_map, width = 11, height = 10, dpi = 300)

urban_rural_compare <- VDL_data %>%
  group_by(`Location of Site`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nUrban vs Rural Comparison:\n")
print(urban_rural_compare)

#Previous use check
prevusesumm <- VDL_data %>%
  group_by(`Previous Use of Site`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    Average_Size = mean(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))

cat("\nVDL by Previous Use of Site:\n")
print(prevusesumm)

#previous use x site type
prevuse <- VDL_data %>%
  group_by(`Previous Use of Site`, `Site Type`) %>%
  summarise(
    Count = n(),
    Total_Hectares = sum(`Site Size (Hectares)`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Hectares))
cat("\nPrevious Use by Site Type:\n")
print(prevuse)