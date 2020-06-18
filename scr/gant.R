
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

activities <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Sv8oodbmBMpNnFS5JyhijANo2tgQnYlxIqzLrpuCNGA/edit?usp=sharing")

## Set factor level to order the activities on the plot
activities$Activity <- 
  factor(activities$Activity,
         levels = activities$Activity[nrow(activities):1])
activities$Type <- 
  factor(activities$Type,
         levels = unique(activities$Type))

activities$`Start Date` <- lubridate::as_date(activities$`Start Date` )
activities$`End Date` <- lubridate::as_date(activities$`End Date` )

meet <- 
activities[activities$Type== "Meeting", ] %>% 
  dplyr::mutate( dif = difftime(`End Date`,`Start Date`, units = "days"),
                 dte =`Start Date` + dif/2 ) %>% 
  mutate(Activity = gsub(" ",  "\n", Activity, fixed = TRUE ))

df_melted <- 
  reshape2::melt(dplyr::filter(activities, Type!= "Meeting"),
                 measure.vars = c("Start Date", "End Date" ))




df_melted %>% 
  mutate(yr = year(value)) %>% 
ggplot(., aes(value, Activity, colour = Type)) +
  geom_line(size = 11) +
  labs(x = '', y = '') +
  egg::theme_article() +
  geom_point(
    data = meet,
    aes(    y = 0.5,
            x = dte),
    shape = 2,
    size = 2,
    color = "black"
  )+
  scale_fill_brewer( type = "qual")+
  geom_text(data = meet, 
            aes(
    label = Activity,
    y = 1,
    x = dte
  ),
  size = 2.5,
  color = "black")+
  # facet_wrap(~yr, ncol = 1)+
  # scale_colour_manual(name = "",
  #                     labels = c("Activity 1: Model\ndevelopment",
  #                                "Activity 2: Model\nintegration"),
  #                     values = c("#81E8C2", "#F6F6B2")) +
  theme(
    # legend.position = c(.84, .87), #place position of the legend inside plotting area
    legend.position = "top", #place position of the legend inside plotting area
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
    panel.grid.minor.x = element_line(colour = "gray", linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0),
    text = element_text(size = 14,
                        family = "TT Times New Roman")
  ) +
  scale_x_date(
    date_labels = "%b-'%y",
    # limits = c(activities$`Start Date`[1], activities$`End Date`[nrow(activities)]),
    date_breaks = '1 month',
    date_minor_breaks = "1 year"
  ) +
  ggsave(
    here::here("out", "gant.png"),
    width = 8.8,
    height = 2.8,
    dpi = 400
  )

shell.exec(here::here("out", "gant.png"))
