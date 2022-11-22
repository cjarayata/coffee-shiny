# to do:
# 1) churn raw coffee data to get new methods
# 2) edit coffee_data to read from the below sheet
# 3) move "old" coffees to historic section?

# ripped from https://github.com/cjarayata/cjarayata.github.io/blob/7-write-draft-coffee-data-story/coffee_story.Rmd
# it makes more sense for it to be part of the shiny vs. part of the portfolio...

library(tidyverse)
library(lubridate)
library(ggrepel)

raw_coffee_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFRdo6gr0uTa3LNSdcMXdAq0MGQcb3OLKKnbpVxYOMogTvnZEHhiEvlQo4SZLJfHXaBVtCAjxZGX7J/pub?gid=3766389&single=true&output=csv")

# retrieve the current stash that's dialed
dialed_coffee <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFRdo6gr0uTa3LNSdcMXdAq0MGQcb3OLKKnbpVxYOMogTvnZEHhiEvlQo4SZLJfHXaBVtCAjxZGX7J/pub?gid=561718715&single=true&output=csv")

# names(raw_coffee_data)

processed_coffee_data <- raw_coffee_data %>% 
        select(brew_date = "Timestamp",
               coffee = "What coffee are you making?",
               roast_date = "What's the roast date?",
               ratio = "What ratio are you using?",
               water_volume = "What's your target total volume?",
               water_type = "What water are you using?",
               coffee_weight = "How much coffee did you use?",
               brew_method = "What brewing method did you use?",
               v60_dripper = "If V60, what dripper did you use?",
               water_temp = "What water temperature did you use?",
               grind_size = "What grind size did you use?",
               grinder = "What grinder did you use?",
               total_brew_time = "What was the total brew time?",
               rating = "Rating (Scale of 1-10)",
               notes = "Any notes to add?"
               ) %>% 
        # do a bit of cleaning
        mutate(brew_date = mdy_hms(brew_date)) %>%
        arrange(desc(brew_date)) %>% 
        mutate(brew_date = as_date(brew_date),
               water_type = case_when(is.na(water_type) & brew_date < "2022-05-25" ~ "Philly Tap",
                                      is.na(water_type) ~ "Pur Filter",
                                      TRUE ~ as.character(water_type)),
               v60_dripper = case_when(is.na(v60_dripper) & str_detect(brew_method, "V60") ~ "Size 03 V60",
                                   TRUE ~ as.character(v60_dripper)),
               grinder = case_when(is.na(grinder) | str_detect(grinder, "Encore") ~ "Encore",
                                   str_detect(grinder, "Q2") ~ "Q2",
                                   TRUE ~ as.character(grinder)),
               roast_date = case_when(coffee == "Elixr - Wilton Benitez" ~ as_date("2022-05-12"),
                                      TRUE ~ mdy(roast_date)),
               # set some factor levels to make easier for plotting
               water_temp = factor(water_temp, levels = c("195 F", "200 F", "205 F")),
               ratio = factor(ratio),
               ratio = fct_reorder(ratio, as.numeric(ratio), min),
               temp_roast_date = mdy(str_extract(coffee, "(?<=\\().+?(?=\\))")),
               roast_date = coalesce(roast_date, temp_roast_date),
               days_since_roast = brew_date - roast_date
               )

# bag open date is generally the first date you roasted the coffee
first_brew_dates <- processed_coffee_data %>% 
        group_by(coffee, roast_date) %>%
        summarise(bag_open_date = min(brew_date), .groups = "drop")

processed_coffee_data <- processed_coffee_data %>% 
        left_join(first_brew_dates) %>% 
        mutate(days_since_bag_open = brew_date - bag_open_date)

# for each coffee
grouped_data <- processed_coffee_data %>% 
        group_by(coffee, ratio, brew_method, water_temp, grind_size) %>% 
        summarise(number_times_brewed = n(),
                  mean_rating = round(mean(rating), 2),
                  max_rating = max(rating)) %>% 
        arrange(desc(number_times_brewed))


# get the coffees brewed in the past week
recent_coffee_list <- processed_coffee_data %>% 
                filter(between(brew_date, today() - 7, today())) %>% 
                select(coffee_brand = coffee) %>% 
                unique()

# join the lists. for any coffee that's brewed in the past week but NOT dialed, you must still
# be dialing it in!
recent_coffee_list <- dialed_coffee %>% 
        full_join(recent_coffee_list) %>% 
        mutate(currently_brewing = case_when(is.na(currently_brewing) ~ TRUE,
                                             TRUE ~ as.logical(currently_brewing))) %>% 
        filter(currently_brewing)

not_dialed <- anti_join(recent_coffee_list, dialed_coffee) %>% 
        pull(coffee_brand)

# pull the brews for this list
not_dialed_brews <- processed_coffee_data %>% 
        filter(coffee %in% not_dialed)

# for a specific coffee that you have NOT dialed, show some graphs?
processed_coffee_data %>% 
        filter(coffee %in% not_dialed,
               str_detect(brew_method, "V60")) %>% 
        make_faceted_ggplot()

test_coffees <- c("Push x Pull - Rwanda - Gatare Natural",
                  "Passenger - Honduras - Mario Moreno")

processed_coffee_data %>% 
        filter(coffee %in% test_coffees,
               str_detect(brew_method, "V60")) %>% 
        make_faceted_ggplot()
