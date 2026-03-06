library(profvis)
library(shiny)
profvis({ runApp('App') }  
    , prof_output = '/App')

p <- profvis(prof_input = '/path_to_save_output/name.Rprof')
htmlwidgets::saveWidget(p, "/path_to_save_output/profile.html")



library(profvis)
library(shiny)

# Define output directory
prof_output_path <- "AppProfile.Rprof"

# Run profiling
profvis({
    runApp('App')
}, prof_output = prof_output_path)

# Load the profile data and save as HTML
p <- profvis(prof_input = prof_output_path)

# Define HTML output path
html_output_path <- "profile.html"

# Save profiling results
htmlwidgets::saveWidget(p, html_output_path)
