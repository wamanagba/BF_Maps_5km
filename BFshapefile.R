source_folder <- "C:/Users/youedraogo/Desktop/Yacoub/Verification/Burkina Faso/CRAFT_Schema_ByR/"

# Record the start time
start_time <- Sys.time()


library(sp)
library(raster)
library(rgeos)
fileloc <- paste0(source_folder)
##make projection of Shape file as defaulted projection in r
projs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
## define cellid for the whole world in 5 arc-min
rwrd <- raster(res = 1/12) 

rwrd[] <- 1:ncell(rwrd)

#plot(rwrd)


# Record the start time
start_time <- Sys.time()
dt = data.frame()
i=1
j=1

setwd(paste0(fileloc, "/Level", 2))

## read the shp file
tmp <- shapefile(paste0("Shape/",list.files(path = "Shape/", pattern = "*.shp$")))
plot(tmp)
## spatial projection
tmp <- spTransform(tmp, CRSobj = projs)
## attribute of Level names, here by the define of sname, we can select shp in just one district
sname <- paste0("Level",1:i,"Name")




## return name of the attribute
rname <- tmp@data[j,sname[i]]


## select
tmp2 <- tmp[tmp@data[,sname[i]] == rname,]
## build a raster for this polygon, and give cell ids 
rlay <- raster(xmn = floor(xmin(tmp2)), xmx = ceiling(xmax(tmp2)), ymn = floor(ymin(tmp2)), ymx = ceiling(ymax(tmp2)), res = 1/12)
rlay <- crop(rwrd,rlay)

## build fishnet
fishnet <- rasterToPolygons(rlay)
plot(fishnet)
fishnet <- spTransform(fishnet, CRSobj = proj4string(tmp2))

fish <- intersect(tmp2,fishnet)
plot(fish)
fishfine <- fishnet[fishnet@data$layer %in% fish@data$layer,]



#Sys.sleep(2)

order_fish <- order(fish@data$layer)
fish <- fish[order_fish, ]
# Order fishfine according to layer
order_fishfine <- order(fishfine@data$layer)
fishfine <- fishfine[order_fishfine, ]

plot(fishfine)

library(sf)

fish_sf <- st_as_sf(fish, coords = c("lon", "lat"), crs = st_crs(projs))
columns_to_remove = c("Level1Name", "Id", "ObjectID")
fish_sf <- fish_sf[, !(names(fish_sf) %in% columns_to_remove)]
names(fish_sf)[names(fish_sf) == "layer"] <- "CellID"
# Define the path and file name for your shapefile
output_shapefile <- "C:/Old__CCAFSToolkit/BF/schema_shapefile.shp"

# Save the data frame as a shapefile
st_write(fish_sf, output_shapefile,append=FALSE)
# Define the path to your shapefile
#shapefile_path <-"C:/CCAFSToolkit/InputFile/schema_shapefile.shp"

# Import the shapefile
fish_shapefile <- st_read(output_shapefile)
plot(fish_shapefile)





library(dplyr)
library(
  sf
)

# Charger le fichier CSV avec les coordonnées géographiques
data <- read.csv("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/BF/1997-01-01-2023-11-17-Western_Africa.csv")
data = data[data$iso=="854",]
data= data[data$year>2011,]
data= data[data$fatalities>0,]
data= data[data$fatalities<100,]

# Créer un objet sf à partir du fichier CSV
coordinates <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Charger le shapefile des limites administratives
shapefile <- st_read("Library/CloudStorage/OneDrive-CGIAR/CIAT/BF/gadm41_BFA_shp/gadm41_BFA_1.shp")
shapefile <- st_transform(shapefile, crs = st_crs(coordinates))

# Effectuer la jointure spatiale en fonction des limites administratives
merged_data <- st_join(coordinates, shapefile, join = st_within)

merged_data <- st_as_sf(merged_data)
library(tmap)
tmap_mode("view")  # Utilisez "plot" pour une carte statique


tmap_mode("plot")  # Utilisez "plot" pour une carte statique

borders <- st_as_sf(shapefile)  # Charger les données des frontières

map=tm_shape(borders) +
  tm_borders() +
  tm_shape(merged_data) +
  tm_dots("fatalities", title = "Nombre de décès", palette = "-Reds", size = 0.1) +
  tm_layout(title = "",legend.show = F)

fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 500,  width=15, height =10, units="in", filename=paste0(fpath, "deces.png"))



library(dplyr)
result <- merged_data %>%
  group_by(admin1) %>%
  summarise(total_fatalities = sum(fatalities))

admin1 = result$admin1
admin1= as.data.frame(ress)
admin1$total_fatalities= result$total_fatalities
library(openxlsx)
write.xlsx(admin1,"/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/fatalities.xlsx")
result <- merged_data %>%
  group_by(CellID) %>%
  summarise(total_fatalities = mean(fatalities))
# Sauvegarder le résultat dans un nouveau fichier

result <- result %>%
  filter(!is.na(CellID))
result= as.data.frame(result)
result <- result %>% 
  select(-geometry)

dd <- merge(result, shapefile, by = "CellID", all = TRUE)
dd <- dd %>%
  mutate(total_fatalities = ifelse(total_fatalities == 0, NA, total_fatalities))

dd <- dd %>%
  mutate(total_fatalities = ifelse(total_fatalities <=1 , NA, total_fatalities))

# Convert the 'File' dataframe to a spatial dataframe using 'st_as_sf'
dd_sf <- st_as_sf(dd)

# Set tmap mode to "plot"
library(tmap)
tmap_mode("plot")




p2 <- tm_shape(dd_sf) +
  tm_polygons(col = "total_fatalities", title = "total fatalities ",style = "quantile") +
  tm_layout(legend.outside = F)
p2

