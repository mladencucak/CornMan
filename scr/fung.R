
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
rm(packages_load, list.of.packages, new.packages)

################################################333
#Load adn tidy data
#####################################################

# Location of WARN notice pdf file
location <-
  # here::here("dat" , "fng", "fungicide-efficacy-for-control-of-corn-diseases-filename-2020-03-18-150007.pdf")
"https://crop-protection-network.s3.amazonaws.com/publications/fungicide-efficacy-for-control-of-corn-diseases-filename-2020-03-18-150007.pdf"

# Extract the table
fng <- 
  extract_tables(location,
                 pages = "2",
                 output = "data.frame")
fng <- as.data.frame(fng[, -4])
# Column names
names(fng) <- c(
  "Class","Active ingredient",
  "Trade Name","Anthracnose Leaf Blight",
  "Common Rust",  "Eyespot",
  "Gray Leaf Spot",  "Northern Corn Leaf Blight",
  "Southern Rust",  "Tar Spot",
  "Harvest Restriction"
)


head(fng)
