######## COVID19 Dashboard #########
# Covid19 Dashboard based on the COVID-19 Data Analysis with R - Worldwide analysis report of the Novel Coronavirus.
# Find original study here: http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf
# COVID 19 API: https://covid19-api-access.herokuapp.com/
# Contact: chafiq.512@gmail.com
#          khalil.dida01@gmail.com
####################################

library(magrittr)
library(lubridate)
library(rpivotTable)
library(tidyverse)
library(plotly)
library(gridExtra)
library(ggforce)
library(kableExtra)
library(leaflet)
library(highcharter)
library(imputeTS)
library(mongolite)


# exemple of a country document in the mongodb
# {
#     "_id": {
#         "$oid": "5e8e454552f4eb70f61b13cd"
#     },
#     "country_name": "Portugal",
#     "statistics_date": "08/04/2020",
#     "country_flag": "https://www.worldometers.info/img/flags/small/tn_po-flag.gif",
#     "country_total_cases": "13,141",
#     "country_total_deaths": "380 ",
#     "country_total_recovered": "196"
# }

# create a connection to mongodb
dbconnection <- mongo(collection = "countries", db = "covid", url = "<MONGODB_CONNECTION_CHANGE_IT_TO_YOUR_OWN_DBCONECTION>")


# create a dataset for countries
countries <- dbconnection$find()

# Renaming the columnes
countries <- countries %>% rename(country = country_name,
                                  date = statistics_date,
                                  confirmed = country_total_cases,
                                  deaths = country_total_deaths,
                                  recoverd = country_total_recovered)

# Convert confirmed, deaths and recovered columns to numeric
countries <- transform(countries,confirmed = as.numeric(gsub(",","",countries$confirmed)))
countries <- transform(countries,deaths = as.numeric(gsub(",","",countries$deaths)))
countries <- transform(countries,recoverd = as.numeric(gsub(",","",countries$recoverd)))

# Convert  dates
countries$date <- as.Date(countries$date,format = "%d/%m/%y")

# Date min
date.min <- min(countries$date)
date.min.format <- format(date.min, "%d/%m/%Y")

# Date max
date.max <- max(countries$date)
date.max.format <- format(date.max, "%d/%m/%Y")

# Get countries flag to use later
countries.flag <- countries %>% filter(date == date.max)
countries.flag$date<- NULL
countries.flag$confirmed <- NULL
countries.flag$deaths <- NULL
countries.flag$recoverd <- NULL
colnames(countries.flag) <- c("country", "flag")
countries$country_flag <- NULL





# Replace NA with 0
# Identify numeric columns
i <- sapply(countries, is.numeric)
# Creation of a temporary dataframe
countriesNA <- countries[i]
# Replace NA with 0
countriesNA %<>% mutate_all(~replace(., is.na(.), 0))
# Modifier countries dataframe
countries$confirmed <- countriesNA$confirmed
countries$deaths <- countriesNA$deaths
countries$recoverd <- countriesNA$recoverd

# Remove extra spaces
countries$country <- trimws(countries$country)

# Group by country and date
countries <- countries[order(countries$country,countries$date),]

# Select data in a date
selected_date <- date.max
countries_in_date <- filter(countries, date == selected_date)

# Add latitude and longitude to the data frame
countriesLatLong <- read.csv("countriesLatLong.csv", sep=";")
countriesLatLong$country <- trimws(countriesLatLong$country)
countries_in_date$country <- trimws(countries_in_date$country)
countries_for_map <- merge(countries_in_date,countriesLatLong, by = "country",
                           all.x = TRUE)
# Add country's flag
countries_for_map <- merge(countries_for_map,countries.flag, by = "country",
                           all.x = TRUE)

# Prepare the data to display on the map
countries_for_map %<>% select(c(country, confirmed, deaths, recoverd,
                                latitude,longitude,flag)) %>%
  mutate(txt=paste0('<img src="',flag,'" width="20" height="15" border="1 px solid #aaa">',
                    '<span style = "margin-left: 5px">','<b>',country,'</b>','</span>',
                    '<hr style = "margin-top: 5px;margin-bottom: 5px">',
                    '<span style="color:orange">','Confirmed: ','<b>', format(confirmed, big.mark=" "),'</b>','</span>',
                    '<br>',
                    '<span style="color:green">','Recovered: ','<b>', format(recoverd, big.mark=" "),'</b>','</span>',
                    '<br>',
                    '<span style="color:red">','Deaths: ','<b>', format(deaths, big.mark=" "),'</b>','</span>'
                    ))

# Creation of the map

map <- leaflet(width="100%") %>% addTiles() %>% setView(lng = 0, 
                                                                  lat = 25,
                                                                  zoom = 2)

# Creation of statistical circles on the map
map %<>% addCircleMarkers(countries_for_map$longitude,
                          countries_for_map$latitude,
                          radius=2+log2(countries_for_map$confirmed), stroke=F,
                          color='red', fillOpacity=0.3,
                          popup=countries_for_map$txt)

# The last cases in the world
world <- countries_in_date %>% summarise(confirmed = sum(confirmed, na.rm = TRUE),
                                         deaths = sum(deaths, na.rm = TRUE),
                                         recoverd = sum(recoverd, na.rm = TRUE))
# Cases around the world group by date
world_per_date <- countries %>% group_by(date) %>% summarise(country = 'World',
                                                             confirmed = sum(confirmed, na.rm = TRUE),
                                                             deaths = sum(deaths, na.rm = TRUE),
                                                             recoverd = sum(recoverd, na.rm = TRUE))
countries %<>% rbind(world_per_date)

# Current confirmed cases
countries %<>% mutate(current.confirmed = confirmed - deaths - recoverd)

## Daily increase in deaths and recovered cases
# Set NA on increases on day 1
data <- countries
data %<>% arrange(country, date)
n <- nrow(data)
day1 <- min(data$date)
data %<>% mutate(new.confirmed = ifelse(date == day1, NA, confirmed - lag(confirmed, n=1)),
                 new.deaths = ifelse(date == day1, NA, deaths - lag(deaths, n=1)),
                 new.recoverd = ifelse(date == day1, NA, recoverd - lag(recoverd, n=1)))
# Change negative numbers to 0
data %<>% mutate(new.confirmed = ifelse(new.confirmed < 0, 0, new.confirmed),
                 new.deaths = ifelse(new.deaths < 0, 0, new.deaths),
                 new.recoverd = ifelse(new.recoverd < 0, 0, new.recoverd))
# Increase in deaths based on total deaths and confirmed cases
data %<>% mutate(rate.upper = (100 * deaths / (deaths + recoverd)) %>% round(1))
# Lower bound for deaths based on confirmed cases
data %<>% mutate(rate.lower = (100 * deaths / confirmed) %>% round(1))
# Mortality rate per day
data %<>% mutate(rate.daily = (100 * new.deaths / (new.deaths + new.recoverd)) %>% round(1))

data[is.na(data)] <- 0

# Prepare data for making the plots
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recoverd, deaths)) %>%
  gather(key=type, value=count, -c(country, date))

data.long %<>% mutate(type=recode_factor(type, confirmed='Total Confirmed',
                                         current.confirmed='Current Confirmed',
                                         recoverd='Recovered',
                                         deaths='Deaths'))
# Data frame for the rates of each country
rates.long <- data %>%
  select(c(country, date, rate.upper, rate.lower, rate.daily)) %>%
  gather(key=type, value=count, -c(country, date))

rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily',
                                          rate.lower='Lower bound',
                                          rate.upper='Upper bound'))

dataLong <- function(countryName){
  long <- data.long %>% filter(country == countryName)
  return(long)
}

## Cases around the world
world.long <- dataLong("World")

# confirmed cases
numberOfCases <- function(world.long){
date.min <- format(min(world.long$date), "%d/%m/%Y")
date.max <- format(max(world.long$date), "%d/%m/%Y")
plot <- world.long %>% filter(type != 'Total Confirmed') %>%
  ggplot(aes(x=date, y=count)) +
  geom_area(aes(fill=type), alpha=0.5) +
  labs(title=paste0('Numbers of Cases - from ', date.min,' to ',date.max)) +
  scale_fill_manual(values=c('red', 'green', 'black')) +
  theme(legend.title=element_blank(), legend.position='bottom',
        plot.title = element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size=unit(0.2, 'cm'),
        legend.text=element_text(size=12),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle=45, hjust=1))
return(plot)

}

numberOfCasesLogScale <- function(world.long){
  date.min <- format(min(world.long$date), "%d/%m/%Y")
  date.max <- format(max(world.long$date), "%d/%m/%Y")
  plot <- world.long %>%
    ggplot(aes(x=date, y=count)) +
    geom_line(aes(color=type)) +
    labs(title=paste0('Numbers of Cases - from ', date.min,' to ',date.max)) +
    scale_color_manual(values=c('purple', 'red', 'green', 'black')) +
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=10),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'),
          legend.text=element_text(size=12),
          axis.text=element_text(size=10),
          axis.text.x=element_text(angle=45, hjust=1)) +
    scale_y_continuous(trans='log10')
  return(plot)
}

options(scipen = 999)

# Get country data by name
selectCountryData <- function(CountryName){
data.country <- data %>% filter(country == CountryName)
data.country[is.na(data.country)] <- 0
return(data.country)
}

# Current confirmed cases and new daily confirmed cases
currentConfirmedCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  # n <- nrow(selected.data)
  plot <- ggplot(selected.data, aes(x=date, y=current.confirmed)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('Current Confirmed Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select daily new confirmed cases by country and return the plot
DailyNewConfirmedCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  # n <- nrow(selected.data)
  plot <- ggplot(selected.data, aes(x=date, y=new.confirmed)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('Daily New confirmed Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select new deaths by country and return the plot
newDeaths <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  plot <- ggplot(selected.data, aes(x=date, y=new.deaths)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('New Deaths from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# Select new recovered cases by country and return the plot
newRecoveredCases <- function(CountryName){
  selected.data <- selectCountryData(CountryName)
  date.min <- format(min(selected.data$date), "%d/%m/%Y")
  date.max <- format(max(selected.data$date), "%d/%m/%Y")
  plot <- ggplot(selected.data, aes(x=date, y=new.recoverd)) +
    geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title=paste0('New Recovered Cases from ', date.min,' to ',date.max)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

## Top 10 countries
# Ranking by confirmed cases after selecting the current date
data.latest.all <- data %>% filter(date == max(date)) %>%
  select(country, date,
         confirmed, new.confirmed, current.confirmed,
         recoverd, deaths, new.deaths, death.rate=rate.lower) %>% 
  mutate(ranking = dense_rank(desc(confirmed)))
# dataTable
dt <- data.latest.all %>% filter(country!='World')
dt$ranking <- NULL
dt <- dt %>% mutate(ranking = dense_rank(desc(confirmed)))
dt$death.rate <- paste(dt$death.rate,"%" , sep=" ")
# add flags
dt <- merge(dt,countries.flag, by = "country",
      all.x = TRUE)
dt <- dt[order(dt$ranking),]
# Selecting top 10 countries including World (11)
k <- 10
top.countries <- data.latest.all %>% filter(ranking <= k+1) %>%
  arrange(ranking) %>% pull(country) %>% as.character()

# top.countries %>% setdiff('World') %>% print()
# add Others
top.countries %<>% c('Others')
# put all other countries in a single groupe of others
data.latest <- data.latest.all %>% filter(!is.na(country)) %>%
  mutate(country=ifelse(ranking <= k + 1, as.character(country), 'Others')) %>%
  mutate(country=country %>% factor(levels=c(top.countries)))
data.latest %<>% group_by(country) %>%
  summarise(confirmed=sum(confirmed), new.confirmed=sum(new.confirmed),
            current.confirmed=sum(current.confirmed),
            recovered=sum(recoverd), deaths=sum(deaths), new.deaths=sum(new.deaths)) %>%
  mutate(death.rate=(100 * deaths/confirmed) %>% round(1))
data.latest %<>% select(c(country, confirmed, deaths, death.rate,
                          new.confirmed, new.deaths, current.confirmed))

# Preparing data for plotes
data.latest.long <- data.latest %>% filter(country!='World') %>%
  gather(key=type, value=count, -country)
data.latest.long %<>% mutate(type=recode_factor(type,
                                                confirmed='Total Confirmed',
                                                deaths='Total Deaths',
                                                death.rate='Death Rate (%)',
                                                new.confirmed='New Confirmed (compared with one day before)',
                                                new.deaths='New Deaths (compared with one day before)',
                                                current.confirmed='Current Confirmed'))
# Creating the bar chart
barChart <- data.latest.long %>% ggplot(aes(x=country, y=count, fill=country, group=country)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=count, y=count), size=2, vjust=0) +
  xlab('') + ylab('') +
  labs(title=paste0('Top 10 Countries with Most Confirmed Cases - ', date.max.format)) +
  scale_fill_discrete(name='Country', labels=aes(count)) +
  theme(legend.title=element_blank(),
        legend.position='none',
        plot.title=element_text(size=11),
        axis.text=element_text(size=7),
        axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~type, ncol=1, scales='free_y')
# Plot for total confirmed and deaths in top 10 countries
linetypes <- rep(c("solid", "dashed", "dotted"), each=8)
colors <- rep(c('black', 'blue', 'red', 'green', 'orange', 'purple', 'yellow', 'grey'), 3)
df <- data %>% filter(country %in% setdiff(top.countries, c('World', 'Others'))) %>%
  mutate(country=country %>% factor(levels=c(top.countries)))
p <- df %>% ggplot(aes(x=confirmed, y=deaths, group=country)) +
  geom_line(aes(color=country, linetype=country)) +
  xlab('Total Confirmed') + ylab('Total Deaths') +
  scale_linetype_manual(values=linetypes) +
  scale_color_manual(values=colors) +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.key.size=unit(0.5, 'cm'))
plot7 <- p + labs(title=paste0('Total confirmed and deaths for Top 10 Countries'))
plot8 <- p + scale_x_log10() + scale_y_log10() +
  labs(title=paste0('Total confirmed and deaths for Top 10 Countries (log scale)'))

# New Confirmed and death cases for top 10 countries
df <- data.latest %>% filter(country %in% setdiff(top.countries, 'World'))
breaks.confirmed <- c(5e3, 1e4, 2e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7)
plot9 <- df %>% ggplot(aes(x=new.confirmed, y=new.deaths, col=death.rate, size=current.confirmed)) +
  scale_size(name='Current Confirmed', trans='log2', breaks=breaks.confirmed) +
  geom_text(aes(label=country), size=2.5, check_overlap=T, vjust=-1.6) +
  geom_point() +
  xlab('New Confirmed') + ylab('New Deaths') +
  labs(col="Death Rate (%)") +
  scale_color_gradient(low='#56B1F7', high='#132B43') +
  scale_x_log10() + scale_y_log10() +
  labs(title=paste0('Top 10 Countries - New Confirmed vs New Deaths (log scale)'))

# Get more data worldwide for the current date
data.world.date.max <- selectCountryData('World') %>% filter(date == date.max)

## Death rate
# Overall death rate for a selected country. returns a plot
deathRateOverall <- function(CountryName){
  ds <- selectCountryData(CountryName)
  date.min <- format(min(ds$date), "%d/%m/%Y")
  date.max <- format(max(ds$date), "%d/%m/%Y")
  plot <- ggplot(ds, aes(x=date))+
    geom_line(aes(y=rate.upper, colour='Upper bound')) +
    geom_line(aes(y=rate.lower, colour='Lower bound')) +
    geom_line(aes(y=rate.daily, colour='Daily')) +
    xlab('') + ylab('Death Rate (%)') + labs(title=paste0('Death Rate Overall from ',date.min,' to ',date.max)) +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=45, hjust=1))
  return(plot)
}

# death rate in the last 2 weeks for a selected country. returns a plot
deathRateLast2weeks <- function(CountryName){
  ds <- selectCountryData(CountryName)
  n <- nrow(ds)
  y.max <- ds[n-(14:0), ] %>% select(rate.upper, rate.lower, rate.daily) %>% max()
  plot <- ggplot(ds[n-(14:0),], aes(x=date)) +
    geom_line(aes(y=rate.upper, colour='Upper bound')) +
    geom_line(aes(y=rate.lower, colour='Lower bound')) +
    geom_line(aes(y=rate.daily, colour='Daily')) +
    xlab('') + ylab('Death Rate (%)') + labs(title='Last Two Weeks') +
    theme(legend.position='bottom', legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.key.size=unit(0.5, 'cm'),
          axis.text.x=element_text(angle=45, hjust=1)) +
    ylim(c(0, y.max))
  
  return(plot)
}

# Select the right flag for a country
getSelectedCountryFlag <- function(CountryName){
  country <- countries.flag %>% filter(country == CountryName)
  return(country$flag)
}

# Get all data for a selected country
getSelectedCountryData <- function(CountryName){
  country <- data %>% filter(country == CountryName)
  country$country <- NULL
  return(country)
}






