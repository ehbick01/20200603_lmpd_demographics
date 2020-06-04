#### Setup #### 

## Define Packages
library(tidyverse)
library(lubridate)
library(patchwork)
library(shadowtext)
library(rgdal)
library(tidycensus)
library(tigris)
library(geosphere)

## Source functions
source("script/functions/custom_functions.R")

## Set theme  
theme_set(
  theme_bw(base_family = 'Roboto Condensed', base_size = 11) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10),
      text = element_text(colour = '#445359'),
      panel.background = element_rect('#F6F6F7'),
      strip.background = element_rect('#F6F6F7', colour = '#F6F6F7'),
      plot.background = element_rect('#F6F6F7'),
      panel.border = element_rect(colour = '#F6F6F7'),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.background = element_rect('#F6F6F7'),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.key = element_blank(),
      strip.text = element_text(
        colour = "#445359",
        face = 'bold',
        size = 10
      ),
      axis.text = element_text(colour = "#445359", size = 10),
      axis.title = element_text(
        colour = "#445359",
        face = 'bold',
        size = 10
      ),
      axis.ticks = element_blank()
    )
)

#### Pull Data ####

#### Grab imported data files
if(length(list.files("data/imports")) != 0) {
  data_files <-
    list.files("data/imports", full.names = T)
  csv_strings <- list()
  for( i in 1:length(data_files)) { 
    csv_strings[i] <- list.files("data/imports", full.names = T)[i]
  }
  names(csv_strings) <- basename(data_files)
}

### Read data

#### Read CSV files

#### Salary data
salary_raw <- read_csv(
  'https://data.louisvilleky.gov/sites/default/files/27096/SalaryData.csv'
)

#### LMPD demographics
lmpd_demographics <- read_csv(
  'https://data.louisvilleky.gov/sites/default/files/LMPD_Demographics_15.csv'
)

#### Load in shapefiles

#### Pull tract centroids
tract_geography <- tracts('21', '111')

#### Keep just centroid info
tract_centroids <- tract_geography %>% 
  as_tibble() %>% 
  mutate(
    latitude = as.numeric(gsub('+', '', INTPTLAT)),
    longitude = as.numeric(INTPTLON)
  ) %>% 
  select(
    GEOID,
    latitude,
    longitude
  )

#### LMPD division shapefiles
lmpd_shapes <-
  readOGR(dsn = 'data/imports/shapefiles', 
          layer = 'lmpd_beat')

#### Change projection
lmpd_shapes <- spTransform(lmpd_shapes, CRSobj = tract_geography@proj4string)

##### Manipulate LMPD shapefile
lmpd_shapes@data$id <- rownames(lmpd_shapes@data)

##### Convert polygons in zip to a data frame for plotting
lmpd_shapes.df <- fortify(lmpd_shapes)

##### Join columns
lmpd_shapes.df <- lmpd_shapes.df%>% 
  left_join(
    lmpd_shapes@data, 
    by="id"
  )

##### Remove non-`Louisville Metro Police Department' districts
lmpd_shapes.df <- lmpd_shapes.df %>% 
  filter(grepl('Louisville Metro Police Department', DIST_DESC))

##### Clean up shapes.df distric description
lmpd_shapes.df$LMPD_DIVISION <- sapply(lmpd_shapes.df$DIST_DESC, function(x) { 
  gsub("^\\s+", '', strsplit(as.character(x), ",")[[1]][2])
})

#### Louisville tract division shapefiles
vars <- load_variables(
  year = 2018,
  dataset = 'acs5'
)

tract_demographics <- get_acs(
  geography = 'tract',
  state = '21',
  county = '111',
  year = 2018,
  survey = 'acs5',
  table = 'B03002',
  geometry = FALSE
) %>% 
  left_join(
    vars %>% select(name, label),
    by = c('variable' = 'name')
  )

#### Manipulate / Analyze Data ####

#### Clean up LMPD demo
lmpd_demographics_police <- lmpd_demographics %>% 
  filter(
    `Job Title` == 'Police Officer',
    grepl('DIVISION', `Assigned Division`)
  ) %>% 
  group_by(
    `Assigned Division`, 
    `Ethnic Group`, 
    Gender
  ) %>% 
  summarise(
    total = n()
  ) %>% 
  group_by(
    `Assigned Division`, 
    `Ethnic Group`
  ) %>% 
  summarise(
    total = sum(total)
  ) %>% 
  mutate(
    lmpd_share = total / sum(total)
  ) %>% mutate(
    division = factor(`Assigned Division`, levels = c('FIRST DIVISION',
                                                      'SECOND DIVISION',
                                                      'THIRD DIVISION',
                                                      'FOURTH DIVISION',
                                                      'FIFTH DIVISION',
                                                      'SIXTH DIVISION',
                                                      'SEVENTH DIVISION',
                                                      'EIGHTH DIVISION')),
    lmpd_ethnicity = case_when(
      `Ethnic Group` %in% c('White', 'Black', 'Hispanic') ~ `Ethnic Group`,
      TRUE ~ 'Other'
    ),
    lmpd_ethnicity = factor(lmpd_ethnicity, levels = c('White', 'Black', 'Hispanic', 'Other'))) %>% 
  ungroup() %>% 
  select(
    division,
    lmpd_ethnicity,
    lmpd_share
  ) %>% 
  group_by(
    division,
    lmpd_ethnicity
  ) %>% 
  summarise(
    lmpd_share = sum(lmpd_share)
  )

#### Clean up tract files
tract_demographics_final <- tract_demographics %>% 
  filter(
    grepl('!!', label),
    !(label %in% c(
      'Estimate!!Total', 
      'Estimate!!Total!!Two or more races!!Two races excluding Some other race, and three or more races'))
  ) %>% 
  mutate(
    label = gsub('Estimate!!Total!!', '', label),
    ethnicity = case_when(
      grepl('Not Hispanic', label) ~ 'Not Hispanic',
      TRUE ~ 'Hispanic'
    ),
    race = case_when(
      grepl('White', label) ~ 'White',
      grepl('Black', label) ~ 'Black',
      TRUE ~ 'Other'
    ),
    race = case_when(
      ethnicity == 'Hispanic' ~ 'Hispanic',
      TRUE ~ race
    )
  ) %>% 
  filter(
    label != 'Hispanic or Latino',
    label != 'Not Hispanic or Latino'
  )

##### Calculate tract totals
tract_demographics <- tract_demographics_final %>% 
  as_tibble() %>% 
  group_by(
    GEOID,
    race
  ) %>% 
  summarise(
    total = sum(estimate)
  ) %>% 
  pivot_wider(
    id_cols = GEOID,
    names_from = race,
    values_from = total
  )

##### Join back together
tract_demographics_final <- tract_centroids %>% 
  left_join(
    tract_demographics,
    by = 'GEOID'
  )

#### Model Data ####

#### Find tracts within divisions

#### Division data
division_grid <- lmpd_shapes.df %>%
  filter(
    DISTRICT %in% c(1, 2, 3, 4, 5, 6, 7, 8)
  ) %>% 
  group_by(
    DISTRICT
  ) %>% 
  summarise(
    lmpd_latitude = mean(lat),
    lmpd_longitude = mean(long)
  )

tract_grid <- tract_demographics_final %>% 
  select(
    GEOID, 
    latitude, 
    longitude
  )

#### Create grid of all combinations of stores and cities
division_tracts_grid <- expand_grid(
  division_grid,
  tract_grid
) %>%
  drop_na() %>%
  rowwise() %>%
  # Calculate distances
  mutate(
    distance = distm(c(lmpd_longitude, lmpd_latitude), c(longitude, latitude), fun = distHaversine) %>% as.numeric() / 1609
  )

#### Identify nearest division
tract_divisions <- division_tracts_grid %>% 
  ungroup() %>% 
  group_by(GEOID) %>% 
  top_n(
    -1,
    distance
  )

#### Join tract demographics
division_citizen_demographics <- tract_divisions %>% 
  select(
    GEOID,
    DISTRICT
  ) %>% 
  left_join(
    tract_demographics_final,
    by = 'GEOID'
  ) %>% 
  pivot_longer(
    cols = c('White', 'Black', 'Hispanic', 'Other'),
    names_to = 'citizen_ethnicity',
    values_to = 'total'
  ) %>% 
  group_by(
    DISTRICT,
    citizen_ethnicity
  ) %>% 
  summarise(
    total = sum(total)
  ) %>% 
  mutate(
    citizen_share = total / sum(total),
    division = case_when(
      DISTRICT == 1 ~ 'FIRST DIVISION',
      DISTRICT == 2 ~ 'SECOND DIVISION', 
      DISTRICT == 3 ~ 'THIRD DIVISION',
      DISTRICT == 4 ~ 'FOURTH DIVISION',
      DISTRICT == 5 ~ 'FIFTH DIVISION',
      DISTRICT == 6 ~ 'SIXTH DIVISION',
      DISTRICT == 7 ~ 'SEVENTH DIVISION',
      DISTRICT == 8 ~ 'EIGHTH DIVISION',
      TRUE ~ 'OTHER'
    )
  ) %>% 
  # Remove 'other' divisions (e.g. independnent cities)
  filter(
    division != 'OTHER'
  )

#### Join LMPD and citizen demographics
division_combined_demographics <- division_citizen_demographics %>% 
  ungroup() %>% 
  select(
    division,
    citizen_ethnicity,
    citizen_share
  ) %>% 
  left_join(
    lmpd_demographics_police,
    by = c('division', 'citizen_ethnicity' = 'lmpd_ethnicity')
  ) %>% 
  mutate(
    lmpd_relative_representation = lmpd_share - citizen_share,
    division = factor(division, levels = c('FIRST DIVISION', 'SECOND DIVISION', 'THIRD DIVISION', 'FOURTH DIVISION', 'FIFTH DIVISION', 'SIXTH DIVISION', 'SEVENTH DIVISION', 'EIGHTH DIVISION')),
    citizen_ethnicity = factor(citizen_ethnicity, levels = c('White', 'Black', 'Hispanic', 'Other'))
  )

#### Visualize / Output ####

lmpd_representation_plt <- division_combined_demographics %>% 
  ggplot(
    aes(
      x = citizen_ethnicity, 
      y = lmpd_relative_representation, 
      group = division
    )
  ) + 
  geom_col() + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) + 
  geom_text(
    aes(
      y = lmpd_relative_representation + 0.015 * sign(lmpd_relative_representation),
      label = scales::percent(lmpd_relative_representation, accuracy = 1)
    ),
    size = 3,
    colour = '#000000',
    family = 'Roboto Condensed'  
  ) +
  facet_wrap(
    division ~ ., nrow = 2
  ) +
  labs(
    title = 'There is a big difference in racial representation in First and Second Divisions',
    subtitle = 'Difference in Share of LMPD and Citizens',
    x = NULL,
    y = NULL
  )

## Save plot
ggsave(
  'outputs/reporting/lmpd_representation_plt.png', 
  lmpd_representation_plt,
  height = 3.25, 
  width = 6.25, 
  units = 'in',
  device = 'png'
)

