## code to prepare data goes here

# Function to copy data
copy_data <- function(src, dest, file) {

  file.copy(paste(src, file, sep = "/"),
            paste(dest, file, sep = "/"),
            overwrite = TRUE)

  message("Copied data: ", src, " → ", dest)
}



# Study level data from redcap
copy_data(here::here("../REDCap/1-data"),
          here::here("data-raw"),
          "study_level_data.rds")

load('data-raw/study_level_data.rds')

usethis::use_data(study_level_data, overwrite = TRUE) # save in the data folder

# Factor dictionary from redcap
copy_data(here::here("../REDCap/1-data/"),
here::here("data-raw/"),
"factor_dictionary.rds")

load('data-raw/factor_dictionary.rds')

usethis::use_data(factor_dictionary, overwrite = TRUE) # save in the data folder
