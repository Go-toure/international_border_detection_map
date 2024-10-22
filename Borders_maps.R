setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/")

# Load necessary libraries
if(!require('sf')) {
  install.packages('sf')
  library('sf')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('gganimate')) {
  install.packages('gganimate')
  library('gganimate')
}
if(!require('gifski')) {
  install.packages('gifski')
  library('gifski')
}

# load the shapefile
all_countries <- readr::read_rds("global.ctry.rds")
all_provinces <- readr::read_rds("global.prov.rds")
all_districts <- readr::read_rds("global.dist.rds")
country_layer <-all_countries |>
  filter(ADM0_NAME %in% c("NIGERIA", "CAMEROON", "CHAD", "NIGER","CENTRAL AFRICAN REPUBLIC"))
province_layer <-all_provinces |>
  filter(ADM0_NAME %in% c("NIGERIA", "CAMEROON", "CHAD", "NIGER","CENTRAL AFRICAN REPUBLIC"))
district_layer <-all_districts |>
  filter(ADM0_NAME %in% c("NIGERIA", "CAMEROON", "CHAD", "NIGER","CENTRAL AFRICAN REPUBLIC"))


# Filter districts for the specified countries
districts <- all_districts |> 
  filter(ADM0_NAME %in% c("NIGERIA", "CAMEROON", "CHAD", "NIGER","CENTRAL AFRICAN REPUBLIC")) 
# %>%
#   filter(yr.end >= 2020)
# Ensure geometries are valid
districts <- st_make_valid(districts)

# Create a new column to identify the country
districts$country <- as.factor(districts$ADM0_NAME)

# Find border districts by checking if they touch districts from other countries
districts$border <- sapply(st_touches(districts), function(neighbors) {
  length(unique(districts$country[neighbors])) > 1
})

# Filter only the border districts
border_districts <- districts[districts$border == TRUE, ]

# Plot the districts, highlighting the border districts with different colors
plot1<-ggplot() +
  geom_sf(data = districts, fill = "grey", color = "NA") +
  geom_sf(data = border_districts, aes(fill = country), color = "NA") +
  scale_fill_manual(values = c("NIGERIA" = "cornflowerblue", "CAMEROON" = "green4", "CHAD" = "yellow4", "NIGER" = "purple1","CENTRAL AFRICAN REPUBLIC"= "#19CFBE")) +
  geom_sf(data = country_layer, fill = "NA", color = "black")+
  theme_minimal() +
  labs(title = "LCB district international Borders",
       fill = "Country")
plot1

plot2<-ggplot() +
  geom_sf(data = country_layer, fill = "NA", color = "black")+
  # geom_sf(data = districts, fill = "grey", color = "NA") +
  geom_sf(data = country_layer, aes(fill = ADM0_NAME), color = "NA") +
  scale_fill_manual(values = c("NIGERIA" = "cornflowerblue", "CAMEROON" = "green4", "CHAD" = "yellow4", "NIGER" = "purple1","CENTRAL AFRICAN REPUBLIC"= "#19CFBE")) +
  geom_sf(data = country_layer, fill = "NA", color = "black")+
  theme_minimal() +
  labs(title = "LCB country international Borders",
       fill = "Country")


plot2 | plot1


##############
#Animate plot

# Filter districts for the specified countries and year
districts <- all_districts %>%
  filter(ADM0_NAME %in% c("NIGERIA", "CAMEROON", "CHAD", "NIGER", "CENTRAL AFRICAN REPUBLIC"))

# Ensure geometries are valid and of supported type
districts <- st_make_valid(districts)
districts <- districts[st_geometry_type(districts) %in% c("POLYGON", "MULTIPOLYGON"), ]

# Create a new column to identify the country
districts$country <- as.factor(districts$ADM0_NAME)

# Find border districts by checking if they touch districts from other countries
districts$border <- sapply(st_touches(districts), function(neighbors) {
  length(unique(districts$country[neighbors])) > 1
})

# Filter only the border districts
border_districts <- districts[districts$border == TRUE, ]

# Create the plot with animation
plot1 <- ggplot() +
  geom_sf(data = districts, fill = "grey", color = "NA") +
  geom_sf(data = border_districts, aes(fill = country), color = "NA") +
  scale_fill_manual(values = c("NIGERIA" = "cornflowerblue", "CAMEROON" = "green4", "CHAD" = "yellow4", "NIGER" = "purple1", "CENTRAL AFRICAN REPUBLIC" = "#19CFBE")) +
  geom_sf(data = country_layer, fill = "NA", color = "black") +
  theme_minimal() +
  labs(title = "LCB district international Borders", fill = "Country") +
  transition_states(country, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

# Animate the plot using gifski_renderer
animated_plot <- animate(plot1, nframes = 100, fps = 10, renderer = gifski_renderer())
animated_plot
# Save the animation
anim_save("animated_border_districts.gif", animation = animated_plot)



