## Importing libraries
## ============================================================================
library(dplyr)
library(sf)
library(stringr)
library(stringi)
library(mapview)



## Importing listing data, changing to spatial data format
## ============================================================================

temporary_file <- tempfile()

download.file("https://data.insideairbnb.com/united-states/or/portland/2024-03-16/data/listings.csv.gz", temporary_file)
listings_raw <- read.csv(temporary_file)

unlink(temporary_file)

## Creating point representing Pioneer Courthouse Square as a rough measure of 
## the city center
## ============================================================================
Pioneer_Square <- data.frame(45.5188803,-122.6818477) %>%
  st_as_sf(., coords = c("X.122.6818477","X45.5188803")) %>%
  st_set_crs(., 4326)

## First round of data exploration
## ============================================================================

# Checking raw data
listings_raw

# Focusing on entire units being rented, as comparability between different
# room types is likely out of the scope of this inference
listings_2 <- listings_raw %>% filter(room_type == "Entire home/apt")

# Checking for multicolinearity between bedrooms and bathrooms using R squared
lm(data = listings_2, formula = bedrooms ~ bathrooms) %>%
  summary()

# Imputing missing bathroom values using bathrooms_text column
listings_2$bathrooms_text %>% unique()
listings_3 <- listings_2 %>% mutate(bathrooms = case_when(
  is.na(bathrooms) & bathrooms_text == "Half-bath" ~ .5,
  is.na(bathrooms) & bathrooms_text != "Half-bath" ~ as.numeric(str_extract(bathrooms_text, "\\d+.?\\d?")),
  .default = bathrooms
) )

# Imputing missing bathrooms using the "accommodates" and bathrooms variables
lm(data = listings_3, formula = bedrooms ~ accommodates) %>%
  summary()
bed_impute_df <- listings_3 %>% group_by(accommodates, bathrooms) %>% 
  summarise(med_bed = median(bedrooms, na.rm = T))

listings_4 <- left_join(listings_3, bed_impute_df, by = join_by(accommodates == accommodates, bathrooms == bathrooms) ) %>%
  mutate(bedrooms = coalesce(bedrooms, med_bed))

# Checking histograms of bedrooms, bathrooms, accommodates
hist(listings_4$bedrooms)
hist(listings_4$bathrooms)
hist(listings_4$accommodates)

# Extracting the price variable from its text format
listings_5 <- listings_4 %>% filter(!is.na(price) & price != "") %>%
  mutate(price = as.numeric(stri_replace_all_regex(price, 
                                                   pattern = c("\\$", "\\,"), 
                                                   replacement = c("",""), 
                                                   vectorize = F)) )

# Filtering out apparently inactive or inaccurate listings, removing most
# variables
listings_6 <- listings_5 %>% filter(number_of_reviews_l30d > 0, 
                      review_scores_accuracy >= 3) %>%
  select(longitude, latitude, property_type, accommodates, bathrooms, 
         bedrooms, price) %>% mutate(property_type = as.factor(property_type))

# Plotting home type against price to see if it is worth keeping
plot(listings_6$property_type, listings_6$price)

# Computing distance from city center, removing property type
listings_7 <- st_as_sf(listings_6, coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  mutate(distance = as.numeric(st_distance(geometry, Pioneer_Square)) ) %>%
  select(-property_type)

# Spatially visualize listings, spot check distance variable
mapview(listings_7, zcol = "distance")

# Remove geometry from data frame
listings_8 <- st_drop_geometry(listings_7)

## Exploration with reduced data set
## ============================================================================

pairs(listings_8)

for (i in 1:ncol(listings_8)) {
  graphics::hist(listings_8[,i], xlab = names(listings_8)[i], 
                 main = paste("Histogram of", names(listings_8)[i]))
}

## Checking assumptions for linear regression
## ============================================================================

# Checking that residuals are normally distributed

lm(data = listings_8, formula = price ~ .) %>% plot()

# Checking for multicollinearity

cor(listings_8 %>% select(-price))

# Removing accommodates because it is multicollinear with bedrooms

listings_9 <- listings_8 %>% select(-accommodates)

# Checking normality of residuals again

lm(data = listings_9, formula = price ~ .) %>% plot()

# Checking for multicollinearity again

cor(listings_9 %>% select(-price))

# Checking homoscedacity

lm(data = listings_9, formula = price ~ .) %>% plot()

# Tranforming data to achieve homomscedacity

lm(data = listings_9, formula = log(price) ~ .) %>% plot()

## Model fitting
## ============================================================================

mod1 <- lm(data = listings_9, formula = log(price) ~ .) %>% summary()

point_est <- exp(mod1$coefficients[4,1])

upper_bound <- exp(mod1$coefficients[4,1] - (mod1$coefficients[4,1] * 1.96))
lower_bound <- exp(mod1$coefficients[4,1] + (mod1$coefficients[4,1] * 1.96))

point_est
lower_bound
upper_bound

# With 95% confidence, a 1 meter increase of a full-unit AirBnB listing's 
# distance from Pioneer Courthouse Square is associated with between a 
# $0.0000518 decrease and a $0.0000168 increase in the price of the listing,
# when accounting for the number of bedrooms and bathrooms.