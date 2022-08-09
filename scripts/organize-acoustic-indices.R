# combine long-duration acoustic indices into one dataframe for plotting in R -----
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("purrr")
lp("lubridate")

# set these for a specific machine and dataset

# raw wav files
# where did we put the towsey outputs?
towsey_directory <- "odata/AcousticIndices"
# where to put the compiled dataframe?
output_parent_directory <- "odata/"

# requires get-acoustic-indices.R to have been run for all sites in this list
list_sites <- tribble(
  ~site_description, ~site_file_name,
  "Calc.5674.site.17.tray.3","AcousticIndices_Calc_Sum_22_5674",
  "Calc.5678.control.tray 3","AcousticIndices_Calc_Sum_22_5678",
  "Calc.5680.site 21.tray 3","AcousticIndices_Calc_Sum_22_5680",
  "LUM.5674.control.tray 4","AcousticIndices_LUM_Sum_22_5674",
  "LUM.5678.site OH4.tray 4","AcousticIndices_LUM_Sum_22_5678",
  "LUM.5680.site OH2.tray 4","AcousticIndices_LUM_Sum_22_5680")
list_sites$towsey_directory <- towsey_directory

# this function first moves summary csv to separate folder and then merges them into single dataframe

combine_towsey_summary_tabs <- function(file_directory, summary_tables) {
  # browser()
  # create file for summary tables
  dir.create(file.path(summary_tables))
  
  # move the summary tables to a separate folder
  files_to_move <- list.files(file_directory,
                              pattern = "Towsey.Acoustic.Indices.csv",
                              full.names = TRUE, recursive = T, include.dirs = T
  )
  
  filesstrings::file.move(files_to_move, summary_tables)
  
  # list remaining csv files, all should contain various indices with 3 letter code in file name
  files <- list.files(summary_tables,
                      pattern = ".csv",
                      full.names = TRUE, recursive = T, include.dirs = T
  )
  all_data <- map_df(files, ~ read.csv(.x) %>% mutate(file = basename(.x)))
  # browser()
  d <- all_data %>% separate(file,
                             # starting from right and using negatives to allow stid lengths to differ
                             into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "ext"),
                             # uses same thresholds as for index specific csv shifted by 4
                             sep = c(-37, -35, -33, -31, -29, -27, -25) - 4
  ) %>%
    # use to check if working
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)
    mutate(
      trap_id = as.numeric(trap_id),
      # this is the date-time when the file started recording
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, sec)),
      minintofile = ResultMinute,
      secintofile = ResultStartSeconds,
      # calculate datetime for each interval
      datetime = file_dt + (minintofile * 60),
      # convert interval to seconds and double to fill in gaps for plotting
      # works if half of each hr were actually sampled
      plot_time = file_dt + (minintofile * 60 * 2),
      # datetime of each interval
      yr = year(datetime),
      mnth = month(datetime),
      day = day(datetime),
      hr = hour(datetime),
      min = minute(datetime),
      sec = second(datetime)
    )
  d
}

# put all indices together in one file for plotting false colour spectrograms----
# combines remaining csv into one dataframe for each site
combine_towsey_data <- function(file_directory) {
  message(
    "Make sure you have first run 'combine_towsey_summary_tabs()' on this directory",
    " otherwise dataframe will contain flaws."
  )
  # list remaining csv files, all should contain various indices with 3 letter code in file name
  files <- list.files(file_directory,
                      pattern = ".csv",
                      full.names = TRUE, recursive = T, include.dirs = T
  )
  # files
  
  all_data <- map_df(files, ~ read.csv(.x) %>% mutate(file = basename(.x)))
  
  d <- all_data %>%
    separate(file,
             # starting from right and using negatives to allow stid lengths to differ
             into = c("trap_id", "yr", "mnth", "day", "hr", "min", "sec", "program", "index_type", "ext"),
             sep = c(-37, -35, -33, -31, -29, -27, -25, -7, -4)
    ) %>%
    # use to check if working
    # select(Index, trap_id, yr, mnth, d, hr, min, sec, program, index_type, ext)
    mutate(
      trap_id = as.numeric(trap_id),
      # this is the date-time when the file started recording
      file_dt = ymd_hms(paste(yr, mnth, day, hr, min, sec)),
      minintofile = Index,
      # calculate datetime for each interval
      datetime = file_dt + (minintofile * 60),
      # convert interval to seconds and double to fill in gaps for plotting
      # works if half of each hr were actually sampled
      plot_time = file_dt + (minintofile * 60 * 2),
      # datetime of each interval
      yr = year(datetime),
      mnth = month(datetime),
      day = day(datetime),
      hr = hour(datetime),
      min = minute(datetime),
      sec = second(datetime)
    ) %>%
    select(-Index, -ext)
  
  d_long <- d %>% pivot_longer(1:(ncol(d) - 13), names_to = "freq_bin", values_to = "score")
  d_long$freq_bin_num <- as.numeric(str_replace(d_long$freq_bin, "c", ""))
  d_long
}



combine_towsey_data_for_all <- function(site_description, site_file_name, towsey_directory) {
  file_directory <- paste0(towsey_directory, "/",site_file_name)
  
  ld <- combine_towsey_data(file_directory)
  ld$site <- site_description
  
  saveRDS(ld, paste0(output_parent_directory, "towsey-indices-", site_file_name, ".rds"))
}

purrr::pmap_dfr(list_sites, combine_towsey_data_for_all)
# because this saves each internally # A tibble: 0 × 0 is expected
