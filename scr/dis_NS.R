
################################################333
#Libraries
#####################################################

list.of.packages <-
  c(
    "here",
    "beepr",
    "tabulizer",
    "tidyverse",
    "data.table",
    "dplyr",
    "stringr",
    "lubridate",
    "conflicted",
    "googlesheets4"
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

################################################333
#Load adn tidy data
#####################################################
# browseURL("https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/")

dta <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1apTGjnQ8a4IpDLTLUaWYqIO2u8nuXt47WyJlKNAbXKo/edit#gid=0")


(df_melted <- 
  reshape2::melt(dta, id.vars = c("Region", "Disease" ),
                 variable.name = "Year", 
                 value.name = "Rating",
                 factorsAsStrings = FALSE
                 )
)



df_melted %>% 
  ggplot(aes(Year, Rating, fill = Disease)) +
  geom_bar(color = "black",
           stat = "identity",
           position = position_dodge(),
           width = 0.9,
           size = 0.5)+
  facet_wrap(~Region)

df_melted %>% 
  ggplot(aes(Region, Rating, fill = Disease)) +
  geom_bar(color = "black",
           stat = "identity",
           position = position_dodge(),
           width = 0.9,
           size = 0.5)+
  facet_wrap(~Year, nrow = 1)+
  theme(
    # legend.position = c(.84, .87), #place position of the legend inside plotting area
    legend.position = "top")



df_melted %>% 
  ggplot(aes(Disease, Rating, fill = Region)) +
  geom_bar(color = "black",
           stat = "identity",
           position = position_dodge(),
           width = 0.9,
           size = 0.5)+
  facet_wrap(~Year, nrow = 4)

df_melted %>% 
  ggplot(aes(Disease, Rating, fill = Region)) +
  geom_bar(color = "black",
           stat = "identity",
           position = position_dodge(),
           width = 0.9,
           size = 0.5)+
  facet_wrap(~Year, nrow = 4)

df_melted %>% 
  ggplot(aes(Region, Rating, color = Disease)) +
  geom_point()+
  # facet_wrap(~Year, nrow = 1)+
  theme(
    # legend.position = c(.84, .87), #place position of the legend inside plotting area
    legend.position = "top")



df_melted %>%
  ggplot() +
  geom_point(aes(Year, factor(Rating), color = as.factor(Disease))) +
  geom_line(aes(Year, Rating, group = Disease, color = Disease)) +
  facet_wrap(~ factor(Region), nrow = 1) +
  theme(# legend.position = c(.84, .87), #place position of the legend inside plotting area
    legend.position = "top") +
  labs(y = "Disease Rating",
       colour  = "Disease",
       subtitle = "Variability of disease ratings over time and production zones (Adapted from  Mueller et al. 2016). ") +
  egg::theme_article() +
  theme(
    # legend.position = c(.84, .87), #place position of the legend inside plotting area
    legend.position = "bottom",
    #place position of the legend inside plotting area
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
    panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
    text = element_text(size = 11,
                        family = "TT Times New Roman")
  ) +
  ggsave(
    here::here("out", "Diseases_North_South.png"),
    width = 6.8,
    height = 3.3,
    dpi = 300
  )

shell.exec(here::here("out", "Diseases_North_South.png"))

shell.exec(here::here("out"))







































