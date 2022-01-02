#
# Run by opening a terminal/CMD window in the folder Milkys2_pc
# Then run 
# R --vanilla <App_801_launch.R &
#
# The last line of the resonse should be "Listening on ...", e.g. http://127.0.0.1:6178
# In the browser, go to the given web adress, e.g. http://127.0.0.1:6178/ 
#
# The app is run by an R process in the background. To stop it, push Esc button. 
#
# From 
# https://stackoverflow.com/a/34317976

library(shiny)
runApp("App_801")