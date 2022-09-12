# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("skimr")
# install.packages("DataExplorer")


# libraries
{
  library(tidyverse)
  library(data.table)
  library(skimr)
  library(DataExplorer)
}

# UDFs
{
  # Define how to roll monster initiative
  roll_init <- function(encounter_dt = current_encounter, both_flag = F){
    # Roll init for monster unless both_flag ==T, then do players too
    if(both_flag == T)  temp_dt <- encounter_dt else temp_dt <- encounter_dt[player_flag == 0]
    
    # Create a roll for each character type
    temp_dt[, ":=" (init_roll = sample(1:20, 1:length(unique(unit_code)), replace =T)),.(unit_code)]
    # Add init modifier to show math
    temp_dt[, ":=" (current_init = init_roll + init_bonus)]
    temp_dt <- temp_dt[order(-current_init, -init_bonus, -player_flag)]
    
    # determine init order based on unit_code e.g. 10 orc hunters should have the same init
    init_temp_dt <- temp_dt[,.(current_init = unique(current_init)),.(unit_code)][, ":=" (current_init_order = rank(-current_init))]
    temp_dt <- temp_dt[,":="(current_init_order = NULL)] %>% merge(init_temp_dt[,.(current_init_order, unit_code)], by = "unit_code") %>% .[order(current_init_order)]
    # temp_dt[, ":=" (current_init_order = rank(-unique(current_init))), .(unit_code)]
    # temp_dt[, ":=" (current_init_order = rank(-current_init, ties.method = "min"))]
    # Return updated dataset depending on whether players were updated as well
    if(both_flag == T)  return_dt <- temp_dt else return_dt <- rbind(encounter_dt[player_flag == 1], temp_dt, fill =T)
    return_dt
  }
  
  # Function to add a monster to current encounter based on the unit_code
  add_monster <- function(encounter_dt = current_encounter, unit_code = NA_character_, count = 1){
    # Make sure unit_code is valid, if not fail
    code_corrected <- intersect(unique(unit_code), monster_table[,.N,.(unit_code)]$unit_code)
    code_fails <- setdiff(unique(unit_code), monster_table[,.N,.(unit_code)]$unit_code)
    new_monster_dt <- data.table()
    # As long as unit_code works, add that monster(s), else fail
    if(length(code_corrected) >0){
      
      for(i in seq_along(1:count)){
        new_monster_temp <- monster_table[code_corrected %in% unit_code][, ":=" (name = paste(name, count))] %>% dplyr::select(-senses, -skills)
        # If this new monster has a name already in the encounter, change it
          # Iterate until a new name is generated
          while(new_monster_temp$name %in% current_encounter$name | new_monster_temp$name %in% new_monster_dt$name){
            # Yank off the final 2 characters, convert to numeric
            tmp_name <- new_monster_temp$name %>% substr(., nchar(.)-1, nchar(.)) %>% as.numeric
            tmp_name <- tmp_name + 1
            new_monster_temp$name <- paste0(new_monster_temp$name %>% substr(., 1, nchar(.)-1), tmp_name)
            
            if(!(new_monster_temp$name %in% current_encounter$name | new_monster_temp$name %in% new_monster_dt$name)){
              new_monster_temp <- rbind(new_monster_dt, new_monster_temp)
              break
            } 
          }
         
        if(nrow(new_monster_dt) == 0) new_monster_dt <- new_monster_temp else new_monster_dt <- rbind(new_monster_dt, new_monster_temp, fill = T)
      }
      
      
    }else{
      stop("No valid 'unit_code' values in 'unit_code' variable. Check monster_table's valid unit_code values.")
    }
    
    new_monster_dt <- rbind(current_encounter, new_monster_temp, fill = T)
    return(new_monster_dt)
  }
  
}

# Data Instantiation/Import
{
  # Mock monster table
  monster_table <- data.table(unit_code = "K", name = "Kobold Skirmisher", type = "Skirmisher",
                              level = 1, init_bonus = 5, senses = "Perception +0; darkvision; trap sense",
                              max_hp = 27, ac = 15, fort = 11, ref = 14, will = 13, speed = 6, 
                              m_attack_bonus = 6, m_attack_desc = "Spear vs AC; +1 to roll per kobold ally adjacent", 
                              m_dam_bonus = "1d8+0", 
                              r_attack_bonus = 6, r_attack_desc = "Spear throw, 1/battle and unarmed after",
                              r_dam_bonus = "1d4+3",
                              other_dam_bonus = "1d6", other_dam_bonus_desc = "CA only",
                              move_power = "Shifty; shift 1 square as a minor; at-will",
                              skills = "Acrobatics +8; Stealth +10; Thievery +10",
                              action_point = NA_real_)
  monster_table[,":=" (blodied_threshold = floor(max_hp/2),
                       player_flag = 0)]
  
  
  # Player table
  player_table <- data.table(unit_code = "RC", name = "Scythor", type = "Striker",
                             level = 1, init_bonus = 4,
                             max_hp = 25, ac = 17, fort = 12, ref = 15, will = 12, speed = 6,
                             healing_surge_count_max = 7,
                             m_attack_bonus = 3,
                             m_dam_bonus = "2d4+0", 
                             r_attack_bonus = 7, 
                             r_dam_bonus = "1d10+4",
                             other_dam_bonus = "1d6", other_dam_bonus_desc = "Quarry Only",
                             action_point = 1)
  player_table[,":=" (blodied_threshold = floor(max_hp/2),
                      player_flag = 1,
                      healing_surge_hp_value = floor(max_hp/4),
                      healing_surge_count_current = healing_surge_count_max)]
}


# Create a new encounter
# TODO: Needs an add_player() command
current_encounter <- player_table[unit_code=="RC"]
# Add 2 kobold skirmishers the wrong way
current_encounter <- add_monster(unit_code = "K")
current_encounter <- add_monster(unit_code = "K", count = 2)
current_encounter <- add_monster(unit_code = "K")
current_encounter[,":=" (status = NA_character_)]





# Roll init and save to dataset
current_encounter <- roll_init(current_encounter, both = T) # Roll player as well w/ both = T

current_encounter[,.(name, status, current_init, max_hp, ac, fort, ref, will)]

