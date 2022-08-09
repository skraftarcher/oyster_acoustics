# Script to calculate acoustic indices

# get long-duration acoustic indices using Towsey's program

## for sarcher
# program_location <- 'C:\\Users\\sarcher\\AP\\AnalysisPrograms.exe'
# 
# for desktop
program_location <- 'C:\\Users\\archertech\\AP\\AnalysisPrograms.exe'

# Set the directory containing the files
#source_directory<-'D:\\LUM_Sum_2022\\LUM_Sum_22_5674'
source_directory<-"D:\\Calc_Sum_2022\\Calc_Sum_22_5680"


# to create a file within the project that will hold the acoustic indices output
#base_output_directory<-"odata/AcousticIndices_LUM_Sum_22_5674"
base_output_directory<-"odata/AcousticIndices_Calc_Sum_22_5680"

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
