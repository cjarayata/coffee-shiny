coffee_data <- read_csv("coffee_data.csv", show_col_types = F)

# Grab the appropriate ratio for the method given. If the coffee doesn't exist,
# return sensible defaults.
find_ratio <- function(coffee_type, brew_method, coffee_lookup){
        
        coffee_row <- coffee_lookup %>% 
                filter(coffee_brand == coffee_type)
        
        # if coffee cannot be found, use 0.055 to start
        if(nrow(coffee_row) == 0 && brew_method == "hoffman"){
                ratio <- 0.055
        }
        if(nrow(coffee_row) == 0 && brew_method == "french press"){
                ratio <- 0.07
        }
        
        if(nrow(coffee_row) == 0 && brew_method == "onyx"){
                ratio <- 0.065
        }
        
        if(nrow(coffee_row) >= 1 && brew_method == "hoffman"){
                ratio <- coffee_row %>% pull(pourover_ratio)
        }
        if(nrow(coffee_row) >= 1 && brew_method == "french press"){
                ratio <- coffee_row %>% pull(french_press_ratio)
        }
        if(nrow(coffee_row) >= 1 && brew_method == "onyx"){
        ratio <- coffee_row %>% pull(onyx_ratio)
        }
        
        return(ratio)
}

find_grind_size <- function(brew_method){
        grind_size <- case_when(brew_method == "hoffman" ~ 14,
                                brew_method == "french press" ~ 32,
                                brew_method == "onyx" ~ 18)
        return(grind_size)
}

# Generate a data.frame that has pour phases and approximate times.
# This will be the chart on which we'll bind the amounts calculated.
pour_timing <- function(target_volume){
        
        # Determine the target brew time based on final volume.
        # Larger quantities will have their pour phases slightly delayed to allow for draining.
        target_brew_time <- case_when(target_volume >= 801 ~ "~ 4:30+",
                       target_volume >= 800 ~ "~ 4:30",
                       target_volume >= 700 ~ "~ 4:15",
                       target_volume >= 600 ~ "~ 4:00",
                       target_volume >= 500 ~ "~ 3:30",
                       target_volume >= 250 ~ "~ 3:00",
                       TRUE ~ "< 3:00")
        
        # This is currently only written for Hoffman, but can be expanded for
        # Onyx and French Press methods
        df <- data.frame(brew_phase = c("Bloom",
                                        "1st Pour",
                                        "2nd Pour",
                                        "Target Brew Time"))
        
        if(target_volume >= 600){
        df <- df %>% mutate(time = c("0:00 - 0:45",
                                  "0:45 - 1:15",
                                  "1:30 - 2:00",
                                  target_brew_time))
        return(df)
        
        } else {
        df <- df %>% mutate(time = c("0:00 - 0:30",
                                  "0:30 - 1:00",
                                  "1:15 - 1:45",
                                  target_brew_time))
        return(df)
}
}

# Function to output a nice gt() brew guide for making the defined coffee, volume, and brew method.
# Currently only written for James Hoffman v60.
give_me_coffee <- function(coffee, target_volume, brew_method = "hoffman", coffee_lookup){
        
        # first, find ideal ratio for given brew method using function
        ratio <- coffee %>% find_ratio(brew_method, coffee_lookup)
        
        # find grind size for given brew method
        grind_size <- find_grind_size(brew_method)
        
        # take that ratio, and multiply by volume to get coffee needed.
        coffee_needed <- round(ratio * target_volume, 0)
        bloom_amt <- coffee_needed * 2
        
        # use 60/40 split to get intermediate volumes for pour phases
        pour_one_vol <- target_volume * .6
        pour_two_vol <- target_volume * .4
        
        # use volume to determine what target brew time should be
        new_df <- pour_timing(target_volume) %>% 
                mutate(volume = c(bloom_amt,
                                        pour_one_vol,
                                        pour_two_vol,
                                        target_volume))
        
        # return a gt() table that has this information for you!
        new_df %>% gt() %>% 
                tab_header(title = md(glue("**Brew Guide for {str_to_title(brew_method)}**")),
                           subtitle = md(glue("**{coffee_needed}g** of {coffee}, Grind Size #{grind_size}"))) %>% 
                cols_label(brew_phase = "Brew Phase",
                           time = "Time",
                           volume = "Volume (g)") %>% 
                cols_align(align = c("center"),
                           columns = c(time, volume)) %>% 
                opt_row_striping(TRUE)
}
