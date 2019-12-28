library(tidyverse)
library(gridExtra)
library(grid)


#' Replace Space
#'
#' Replace the space in column names of tibbles and dataframes
#'
#' @param df The dataframe or tibble with column names. 
#' 
#' @return Produce dataframe with column name spaces replaced with underscore.
replace_space <- function(df){
      col_names <- c()
      for (i in colnames(df)){
            col_names <- c(col_names, gsub(pattern = " ", replacement = "_", x = i)) 
      }
      colnames(df) <- col_names
      df
}

# load in the dataset for all  2018 candidates 
all_cands_raw <- read_csv("data/all_cands_2018.csv", skip = 1)


###### Data Cleaning ######
# replace the spaces in column names with underscores
all_cands_raw <- replace_space(all_cands_raw)



# replace the NA in election results with L for losses 
General_Elec_Result <- c()
for (i in all_cands_raw$General_Elec_Result){
      if (is.na(i)){
            General_Elec_Result <- c(General_Elec_Result, "L")
      } else {
            General_Elec_Result <- c(General_Elec_Result, "W")
      }
      
}

# replace the new election results column in all_cands
all_cands_raw$General_Elec_Result <- General_Elec_Result

# replace the NoCorpPAC column name with Pledge_Status
colnames(all_cands_raw)[2] <- "Pledge_Status"

# replace the values under Pledge_Status with NO = PAC YES = NO PAC
Pledge_Status <- c()
for (i in all_cands_raw$Pledge_Status){
      if (i == "NO"){
            Pledge_Status <- c(Pledge_Status, "PAC") 
      } else {
            Pledge_Status <- c(Pledge_Status, "NO PAC") 
      }
}

# add the new column values into the all_cands df
all_cands_raw$Pledge_Status <- Pledge_Status


# remove special characters from dollar and percent columns and convert to doubles

# define the columns to be converted (dollar signs and commas)
cols_2_convert <- c(colnames(all_cands_raw)[9], colnames(all_cands_raw)[11:21])

for (col_name in cols_2_convert){
   col <- all_cands_raw[[col_name]]
   col <- gsub(pattern = "$",
               replacement = "",
               x = col,
               fixed = TRUE)
   col <- gsub(pattern = ",",
               replacement = "",
               x = col,
               fixed = TRUE)
   col <- as.numeric(col)
   all_cands_raw <- all_cands_raw %>% 
      mutate({{col_name}} := col)
}


# remove % sign from VotePercent
col <- all_cands_raw$VotePercent
col <- gsub(pattern = "%",
            replacement = "", 
            x = col,
            fixed = TRUE)
all_cands_raw <- all_cands_raw %>% 
   mutate(VotePercent = col)



# save the cleaned dataset 
all_cands <- all_cands_raw
write_csv(x = all_cands, path = "data/all_cands_2018_clean.csv", col_names = TRUE)
