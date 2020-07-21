
################################################333
#Stats about corn and wheat yeild 
#####################################################


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
    "conflicted"
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
conflict_prefer("filter", "dplyr")
rm(packages_load, list.of.packages, new.packages)

################################################333
#Load adn tidy data
#####################################################

dta<- 
read.csv(here::here("dat", "wheat_yields.csv"))

dta$Year <- as.Date(ISOdate(dta$Year, 1, 1))

#profitability margin
# 
dta %>% 
  mutate( d = Price.in.dollars.per.bu * .in.bu.per.ac %>% round(.,-1) )

dta %>%
  filter(crop == "Wheat") %>% 
  reshape2::melt(id.vars = c("Year","crop")) %>% 
  ggplot()+
  geom_point( aes(Year, value, color = variable))+
  geom_line( aes(Year, value, color = variable))+
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    expand = c(.03, .03)
  ) +
  theme_bw()+
  theme(legend.position = c(.16,.5),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        title = element_text("Wheat production and price received for the period, 2010-2018 at the US level.")
        )


dta %>%
  select( Year, crop , Price.in.dollars.per.bu) %>% 
  reshape2::melt(id.vars = c("Year","crop")) %>% 
  remove_missing() %>% 
  ggplot()+
  geom_point( aes(Year, value, color = crop))+
  geom_line( aes(Year, value, color = crop))+
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year"
    # expand = c(.0, .03)
  ) +
  theme_bw()+
  labs( y = "USD per bushel (25kg)", title = )+
  theme(legend.position = c(.86,.8),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
  )




#######################################################
#Surface plot :Profitability as function of yield and price
###########################################################

require(plotly)

yield = seq(5,10, 0.1)
price = seq(3,8, 0.1)

fun <-  function(x, y){x*y}
profit <- outer(price, yield, fun)


f1 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "grey"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 12,
  color = "black"
)
tit<- list(
  title = "Profit/Yield/Disease Management",
  titlefont = f1,
  tickfont = f2
)
x <- list(
  title = "Yield (bu/acre)",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  exponentformat = "E",
  nticks= 8,
  range= c(min(yield), max(yield)),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255)"
)
y <- list(
  title = "Price ($/bu)",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  exponentformat = "E",
  nticks = 8,
  range= c(min(price), max(price)),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255)"
)
z <- list(
  title = "Profit ($)",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = f2,
  exponentformat = "E",
  # nticks= 10,
  range= c(min(profit),max(profit)),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=T,
  zerolinecolor="rgb(255,255,255"
)

library(viridis)
color_var <- rev(plasma(256))

zz <- profit 
zz[zz>0] <- 30

color_var <- seq(1,256,1)
color_var <- ifelse(z>30, "green", "red" )

p <- 
  plot_ly() %>%
  add_surface(x= yield,y=price,z=profit, 
              type="surface",
              # colors = c("red","yellow", "green"),
              colorscale = list(c(0, .2, 1), c( "#f26c64" ,"#ffdd71", "#9fcd99")),
              name = "Projected profit") %>% 
  add_surface(x= yield,y=price,z=zz,
              type="surface",
              colors = "green",
              opacity = .6,
              name = "Threshold (30$)") %>%
  plotly::layout(
    title = tit,
    scene = list(
      xaxis = x,
      yaxis = y,
      zaxis = z
    )) 




##Custom ticks
axx <- list(
  ticketmode = 'array',
  ticktext = c("Huey", "Dewey", "Louie"),
  tickvals = c(0,25,50),
  range = c(-25,75)
)

htmlwidgets::saveWidget(p, here::here("out", "prof.html"))

shell.exec(here::here("out", "prof.html"))











































