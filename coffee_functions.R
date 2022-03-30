coffee_data <- read_csv("coffee_data.csv", show_col_types = F)

# Grab the appropriate ratio for the method given. If the coffee doesn't exist,
# return sensible defaults.
find_ratio <- function(coffee, brew_method, coffee_lookup){
        
        coffee_row <- coffee_lookup %>% 
                filter(coffee_brand == coffee)
        
        # if coffee cannot be found, use 0.055 to start
        if(nrow(coffee_row) == 0 && brew_method == "hoffmann v60"){
                ratio <- 0.055
        }
        if(nrow(coffee_row) == 0 && str_detect(brew_method, "french press")){
                ratio <- 0.07
        }
        
        if(nrow(coffee_row) == 0 && brew_method == "onyx"){
                ratio <- 0.065
        }
        
        if(nrow(coffee_row) >= 1 && brew_method == "hoffmann v60"){
                ratio <- coffee_row %>% pull(pourover_ratio)
        }
        if(nrow(coffee_row) >= 1 && str_detect(brew_method, "french press")){
                ratio <- coffee_row %>% pull(french_press_ratio)
        }
        if(nrow(coffee_row) >= 1 && brew_method == "onyx"){
        ratio <- coffee_row %>% pull(onyx_ratio)
        }
        
        return(ratio)
}

find_grind_size <- function(brew_method, coffee){
        grind_size <- case_when(# nizza needs a larger grind to obtain target brew times
                                brew_method == "hoffmann v60" &
                                        coffee == "La Colombe - Nizza - Medium Roast" ~ 18,
                                brew_method == "hoffmann v60" ~ 14,
                                str_detect(brew_method, "french press") ~ 30,
                                brew_method == "onyx" ~ 18)
        return(grind_size)
}

find_water_temp <- function(brew_method, coffee){
        # give me 205 for everything, except when using nizza
        # and brazilian
        water_temp <- case_when(brew_method == "hoffmann v60" &
                                        coffee == "La Colombe - Nizza - Medium Roast" ~ 195,
                                coffee == "La Colombe - Brazil - Beleza" ~ 200,
                                TRUE ~ 205)
        return(water_temp)
}

# Generate a data.frame that has pour phases and approximate times.
# This will be the chart on which we'll bind the amounts calculated.
pour_timing <- function(target_volume, brew_method){
        
        if(brew_method == "hoffmann v60"){
        # Determine the target brew time based on final volume.
        # Larger quantities will have their pour phases slightly delayed to allow for draining.
        target_brew_time <- case_when(target_volume >= 801 ~ "~ 4:30+",
                       target_volume >= 800 ~ "~ 4:30",
                       target_volume >= 700 ~ "~ 4:15",
                       target_volume >= 600 ~ "~ 4:00",
                       target_volume >= 500 ~ "~ 3:30",
                       target_volume >= 250 ~ "~ 3:00",
                       TRUE ~ "< 3:00")
        
        # This is currently only written for Hoffmann v60 and french press methods
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
        # if brew method is french press, volume becomes irrelevant
        if(brew_method == "hoffmann french press"){
        df <- data.frame(brew_phase = c("Initial Pour",
                                        "Stir & Scoop Crust",
                                        "Waiting Time",
                                        "Total Brew Time"))
        df <- df %>% mutate(time = c("4 minutes",
                                  "At 4 minutes",
                                  "5 to 10 minutes",
                                  "Up to 15 minutes"))
        return(df)
        }
        
        if(brew_method == "french press"){
        df <- data.frame(brew_phase = c("Bloom",
                                        "Pour",
                                        "Plunge",
                                        "Total Brew Time"))
        df <- df %>% mutate(time = c("30 seconds",
                                  "After bloom",
                                  "At 4 minutes",
                                  "4 to 5 minutes"))
        return(df)        
        }
}

# Function to output a nice gt() brew guide for making the defined coffee, volume, and brew method.
# Currently only written for James Hoffmann v60.
give_me_coffee <- function(coffee, target_volume, brew_method = "hoffmann v60", coffee_data){
        
        # first, find ideal ratio for given brew method using function
        ratio <- coffee %>% find_ratio(brew_method, coffee_data)
        
        water_temp <- find_water_temp(brew_method, coffee)
        
        # find grind size for given brew method
        grind_size <- find_grind_size(brew_method, coffee)
        
        # take that ratio, and multiply by volume to get coffee needed.
        coffee_needed <- round(ratio * target_volume, 0)
        bloom_amt <- coffee_needed * 2
        
        if(brew_method == "hoffmann v60"){
        # use 60/40 split to get intermediate volumes for pour phases
                pour_one_vol <- target_volume * .6
                pour_two_vol <- target_volume * .4
                
                # use volume to determine what target brew time should be
                new_df <- pour_timing(target_volume, brew_method) %>% 
                        mutate(volume = c(bloom_amt,
                                                pour_one_vol,
                                                pour_one_vol + pour_two_vol,
                                                target_volume))
                
        }
        if(brew_method == "hoffmann french press"){
        new_df <- pour_timing(target_volume, brew_method) %>% 
                mutate(volume = target_volume)
        
        }
        if(brew_method == "french press"){
        new_df <- pour_timing(target_volume, brew_method) %>% 
                mutate(volume = c(bloom_amt,
                                  target_volume,
                                  target_volume,
                                  target_volume))
        
        }
        
        # return a gt() table that has this information for you!
        gt_table <- 
                new_df %>% 
                gt() %>% 
                tab_header(title = md(glue("**Brew Guide for {str_to_title(brew_method)}**")),
                           subtitle = md(glue("**{coffee_needed}g** of {coffee}. Grind Size #{grind_size}. Water @ {water_temp} F"))) %>% 
                cols_label(brew_phase = "Brew Phase",
                           time = "Time",
                           volume = "Volume (g)") %>% 
                cols_align(align = c("center"),
                           columns = c(time, volume)) %>% 
                opt_row_striping(TRUE) %>% 
                tab_source_note(HTML("Grind sizes are for Baratza Encore. Refer to <a href = 'https://honestcoffeeguide.com/guides/coffee-grind-size-chart' target = '_blank'> Honest Coffee's Grind Size Tool</a> for guidance for your particular grinder."))
        
        if(brew_method == "hoffmann v60"){
        gt_table <- gt_table %>%
                tab_source_note("Note: Swirl the V60 during bloom phase and after second pour for even extraction.")
        }
        if(brew_method == "hoffmann french press"){
        gt_table <- gt_table %>%
                tab_source_note("Note: Only plunge to top of liquid so as to not disturb grounds.")
        }
        if(brew_method == "french press"){
        gt_table <- gt_table %>%
                tab_source_note("Note: Plunge to bottom of carafe.")
        }
        
        return(gt_table)
}
