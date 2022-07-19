# Script to calculate acoustic indices

# get long-duration acoustic indices using Towsey's program

## for sarcher
# program_location <- 'C:\\Users\\sarcher\\AP\\AnalysisPrograms.exe'
# 
# for desktop
program_location <- 'C:\\Users\\Archertech\\AP\\AnalysisPrograms.exe'

# Set the directory containing the files
source_directory<-''

# Get a list of audio files inside the directory
files <- list.files(source_directory, pattern = "*.wav", full.names = TRUE)

# create output directory
dir.create(file.path(base_output_directory))
# iterate through each file
for(file in files) {
  message("Processing ", file) 
  
  # get just the name of the file
  file_name <- basename(file)
  
  # make a folder for results
  output_directory <- normalizePath(file.path(base_output_directory, file_name))
  dir.create(output_directory, recursive = TRUE)
  
  # prepare command
  command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', file, output_directory)
  
  # finally, execute the command
  system2(program_location, command)
}