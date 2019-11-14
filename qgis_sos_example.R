# install.packages("RQGIS")
library("RQGIS")

####
# QGIS Connection
####

#Example from https://www.youtube.com/watch?v=rGNy3ux-LPc
set_env(dev = FALSE)
# search all algoritms in the QGIS
find_algorithms(search_term = "")
find_algorithms("interp", name_only = FALSE)

get_usage("grass:v.surf.idw")
get_args_man(alg = "grass:v.surf.idw")
