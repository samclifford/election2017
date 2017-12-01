library(sf)
qld <- sf::read_sf("Data/Boundaries/State_electoral_boundaries_2017.shp")

windows()
plot(qld)

library(stringi)
library(tidyverse)

electorate_names <- distinct(to_plot, Electorate) %>%
  mutate(NAME = stri_trans_toupper(Electorate)) %>%
  mutate(NAME = ifelse(NAME == "MCCONNEL", "McCONNEL", NAME))

qld <- inner_join(qld, electorate_names)

wide_results <- to_plot %>%
  select(Electorate, Party, Percent) %>%
  group_by(Electorate, Party) %>%
  summarise(Percent = sum(Percent)) %>%
  spread(Party, Percent)

qld <- inner_join(wide_results, qld)

ggplot() + geom_sf(data=qld)

ggplot(qld) +
  geom_sf(aes(fill = GRN)) +
  scale_fill_continuous(high=cols["GRN"], low="white", limits=c(0,70)) +
  theme_minimal() +
  coord_sf()

# let's try hexmap now

library(hexmapr)

input_file <-"Data/Boundaries/State_electoral_boundaries_2017.shp"
original_shapes <- read_polygons(input_file)
original_details <- get_shape_details(original_shapes)


raw <- read_polygons(input_file)

raw@data$xcentroid <- coordinates(raw)[,1]
raw@data$ycentroid <- coordinates(raw)[,2]


clean <- function(shape){
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = inner_join(shape.points, shape@data, by="id")
}

library(viridis)

result_df_raw <- clean(raw)
rawplot <- ggplot(result_df_raw) +
  geom_polygon( aes(x=long, y=lat, fill = SHAPE_Area, group = group)) +
  geom_text(aes(xcentroid, ycentroid, label = substr(NAME,1,4)), size=2,color = "white") +
  coord_equal() +
  scale_fill_viridis() +
  guides(fill=FALSE) +
  theme_void()

rawplot


par(mfrow=c(2,3), mar = c(0,0,2,0))
for (i in 1:6){
  new_cells <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal', i)
  plot(new_cells[[2]], main = paste("Seed",i, sep=" "))
}

new_cells_hex <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal', 2)
resulthex <- assign_polygons(original_shapes,new_cells_hex)

result_df_hex <- clean(resulthex)

wide_results %>%
  gather(Party, Percent, -Electorate) %>%
  mutate(Percent = if_else(is.na(Percent), 0, Percent)) %>%
  mutate(Percent = cut(Percent, breaks=c(0,1, 10, 20, 30, 40, 50, 60, 70), right = F)) %>%
  inner_join(electorate_names) %>%
  inner_join(result_df_hex) %>%
  ggplot(.) +
  geom_polygon( aes(x=long, y=lat, alpha = Percent, fill=Party, group = group)) +
  geom_text(aes(V1, V2, label = abbreviate(Electorate, minlength = 4)), size=2,color = "black") +
  coord_equal() +
  facet_wrap( ~ Party, nrow=2) +
  guides(fill=FALSE) +
  scale_fill_manual(values=cols) +
  theme_void()
