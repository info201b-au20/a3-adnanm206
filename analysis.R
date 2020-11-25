library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(reshape2)
library(lintr)
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

################################Summary Information#############################

summary_info <- list()

# State with highest black jail population
summary_info$state_highest_black_jail_pop <- data %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(state)

# County with highest black jail population
summary_info$county_highest_jail_pop_in_tx <- data %>%
  filter(state == "TX") %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  pull(county_name)
 
# Total population in Texas
summary_info$total_pop_in_tx <- data %>%
  filter(state == "TX") %>%
  filter(year == "2018") %>%
  filter(total_pop == max(total_pop, na.rm = T)) %>%
  pull(total_pop)

# State with highest fips
summary_info$state_highest_fips <- data %>%
  filter(year == "2018") %>%
  filter(fips == max(fips)) %>%
  pull(state)

# County with highest fips
summary_info$county_highest_fips <- data %>%
  filter(year == "2018") %>%
  filter(fips == max(fips)) %>%
  pull(county_name)

########################### Trends over time chart#############################

# Get the top 10 unique counties in Texas with black jail populations every 5
# years from 1990 up until 2010
top <- data %>%
  select(year, state, county_name, black_jail_pop) %>%
  filter(state == "TX", year %in% c(1990, 2000, 2010)) %>%
  group_by(county_name) %>%
  summarize(m = mean(black_jail_pop, na.rm = T)) %>%
  top_n(10)


# This selects the year, state, county, and black_jail_pop
df <- data %>%
  select(year, state, county_name, black_jail_pop) %>%
  filter(state == "TX", year %in% c(1990, 2000, 2010),
         county_name %in% top$county_name)
  colnames(df) <- c("Year", "State",  "County", "black_jail_pop")

# This then creates the comparison chart that shows the black and white
# population in each county of Texas.
  ggplot(df, aes(x = Year, y = black_jail_pop)) +
  geom_line(aes(color = County), size = 1) +
  theme_minimal() +
  labs(x = "year", y = "black Jail Population",
       title = "Black Jail Population in Texas between 1990 and 2010 ")

##########################Variable comparison chart#############################
# This selects the black and white population in each county of Texas
  population <- data %>%
  group_by(county_name) %>%
  filter(state == "TX") %>%
  summarise(black_pop = sum(black_pop_15to64, na.rm = T),
            white_pop = sum(white_pop_15to64, na.rm = T))
            
# This arranges to get the black and white population in each county
xdata <- group_by(population["county_name"], population["black_pop"],
                  population["white_pop"]) %>%
  arrange(desc(population["black_pop"]))

# This selects only 10 counties in Texas
ydata <- xdata[1:10, ]
        
# This combines the data we need to graph the number of black and white
# population in each county
dfm <- melt(ydata[, c("county_name", "black_pop", "white_pop")], id.vars = 1)
colnames(dfm) <- c("Counties", "Race",  "population")

# This creates a bar chart of the data
ggplot(dfm, aes(x = population, y = Counties)) +
  geom_bar(aes(fill = Race), stat = "identity", position = "dodge") +
  labs(x = "Population", y = "Counties",
     title = "Black Vs white population from 10 counties in Texas")

#####################################MAP#######################################

### This shows me filtering the incarceration data set
sample_data <- data %>%
  select(total_pop, native_pop_15to64, year, fips, state, county_name) %>%
  mutate(perc_native_15to64 = native_pop_15to64 / total_pop) %>%
  filter(year == 2018)

### This creates a data table of map data with the fips code
shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

### This merges the map data along with the filtered incarceration data
map_data <- shapes %>%
  left_join(sample_data, by = "fips") %>%
  filter(state == "TX")

### This shows the blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

### Finally this creates the map
ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = perc_native_15to64),
    color = "grey", size = 1.2
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$perc_native_15to64)),
                        na.value = "white") +
  blank_theme +
  ggtitle("Populations per county in TX")
