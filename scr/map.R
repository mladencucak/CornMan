

################################################ 
#Libraries
################################################# 

list.of.packages <-
  c(
    "here",
    "beepr",
    "tidyverse",
    "data.table",
    "dplyr",
    "stringr",
    "lubridate",
    "conflicted",
    "maps",
    "sf",
    "ggspatial"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}
conflict_prefer("here", "here")
conflict_prefer("map", "purrr")
conflict_prefer("year", "lubridate")
conflict_prefer("select", "dplyr")
rm(packages_load, list.of.packages, new.packages)

################################################ 
#Map
################################################ 

# browseURL("https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
head(states)

states <- cbind(states, st_coordinates(st_centroid(states)))
library("tools")
states$ID <- toTitleCase(as.character(states$ID)) %>% as.factor()
head(states)

prj_states <- subset(states, ID %in% 
                       c("Ohio",
                         "Pennsylvania", 
                         "North Dacota",
                         "North Carolina",
                         "Kentucky",
                         "Kansas",
                         "Delaware",
                         "Mississippi"))
val_states <- subset(states, ID %in% 
                       c("Iowa", "Kentucky", "Mississippi", "Ohio", "Pennsylvania"))




states$nudge_y <- 0
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "Delaware"] <- -.5
states$nudge_y[states$ID == "Maryland"] <- .5
states$nudge_y[states$ID == "Rhode Island"] <- .1
states$nudge_y[states$ID == "Massachusetts"] <- .1
states$nudge_y[states$ID == "Connecticut"] <- -.2

(mapus <- 
ggplot(data = world) +
    geom_sf(data = states, fill = "lightgray") + 
    geom_sf(
    data = prj_states,
    # color= "#F6F6B2",
    fill = "antiquewhite1"
  ) +
    geom_sf(data = val_states, fill = "lightblue") + 
    
   # geom_text(data = states, aes(X, Y, label = ID), size = 2) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  coord_sf(xlim =  c(-104, -70), ylim = c(30.5, 46), expand = FALSE)+
  theme_bw()+
    geom_text(data = states, aes(X, Y, label = ID),
             size = 3,
             # fontface = "bold",
             nudge_y = states$nudge_y) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Participating States"
          # subtitle = "(2 sites in Palm Beach County, Florida)"
          ) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
)


ggsave(
  file = here::here("out" , "map.png"),
   plot = mapus,
  width = 18,
  height = 18,
  units = "cm",
  dpi = 600
)

shell.exec(here::here("out" , "map.png"))

shell.exec(here::here("out"))











