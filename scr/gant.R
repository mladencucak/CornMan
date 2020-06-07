library(ggplot2)
library(readr)

# browseURL("https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/")
library(googlesheets4)

activities <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1amBCSlrzJcF6c5C0Q840EHvccPHOpJ1VH4rvpW1MKMQ/edit#gid=0")

## Set factor level to order the activities on the plot
activities$Task <- 
  factor(activities$Task,
         levels = activities$Task[nrow(activities):1])
activities$Phase <- 
  factor(activities$Phase,
         levels = unique(activities$Phase))

activities$`Start Date` <- lubridate::as_date(activities$`Start Date` )
activities$`End Date` <- lubridate::as_date(activities$`End Date` )

df_melted <- 
  reshape2::melt(activities, measure.vars = c("Start Date", "End Date" ))





ggplot(df_melted, aes(value, Task, colour = Phase)) +
  geom_line(size = 11) +
  labs(x = '', y = '', title = 'Mummy Berry DSS Development Plan') +
  egg::theme_article() +
  scale_colour_manual(name = "",
                      labels = c("Phase 1: Model\ndevelopment",
                                 "Phase 2: Model\nintegration"),
                      values = c("#81E8C2", "#F6F6B2")) +
  theme(
    legend.position = c(.84, .87), #place position of the legend inside plotting area
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
    limits = c(activities$`Start Date`[1], activities$`End Date`[nrow(activities)]),
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
