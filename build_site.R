#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 
setwd("C:/Documents/Coursera/CS_9_DP_ST/webSubmissions")

#render your sweet site. 
rmarkdown::render_site()