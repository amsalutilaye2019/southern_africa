
packages_required <- c("terra", "geodata", "tidyverse", "sf", "readxl", "tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


data <- readxl::read_excel("E:/01CIAT/03Fertilizer/03southernAfricaRecom/data/input/not_all_countries_22_23_24_R1.xlsx")
dim(data)
glimpse(data)
#View(data)
head(data)
colnames(data)

#create a unique id for the data
data$id <- paste('sa', seq(1, nrow(data), 1), sep = "_")
loc <- read.delim("E:/01CIAT/03Fertilizer/03southernAfricaRecom/data/input/locations.txt")

#add the location data to the main data
data_loc <- data |> dplyr::mutate(lat = case_when(country == 'Zambia' & station == 'Msekera' ~ -13.645, 
                                                  country == 'Mozambique' & station == 'Ulongwe' ~ -14.7079516,
                                                  country == 'Malawi' & station == 'Chitedze' ~ -13.98333)) |>
  dplyr::mutate(long = case_when(country == 'Zambia' & station == 'Msekera' ~ 32.5585, 
                                country == 'Mozambique' & station == 'Ulongwe' ~ 34.3587921,
                                country == 'Malawi' & station == 'Chitedze' ~ 33.63333))
head(data_loc)  
#View(data_loc)

shp_pts <- data_loc |> terra::vect(geom = c('long', 'lat'), crs = 'epsg:4326')

#extract covariates based on points
cnames <- c("Malawi", "Zambia", "Mozambique")
clim_vars <- c('tmax', 'tmin', 'tavg', 'prec', 'srad', 'wind', 'vapr')
soil_vars <- c("Al", "bdr", "clay", "C.tot", "Ca", "db.od", "eCEC.f", "Fe", "K",
               "Mg", "N.tot", "oc", "P", "pH.H2O", "sand", "silt", "S", 
               "texture", "wpg2", "Zn")

#soil - no mosaic b/c it downloads the whole africa
soil_all <- data.frame()
for(i in 1:length(soil_vars)){
  soil_lyr <- geodata::soil_af_isda(country = cnames[1], var = soil_vars[i], path = tempdir())
  soil_pts <- soil_lyr |> terra::extract(shp_pts, ID = F)
  if(ncol(soil_all) == 0){
    soil_all <- soil_pts
  }else{
    soil_all <- cbind(soil_all, soil_pts)
  }
}

#clim
clim_all <- data.frame()
for(i in 1:length(soil_vars)){
  print(i)
  clim_lst <- list()
  for(j in 1:length(cnames)){
    clim <- geodata::worldclim_country(country = cnames[j], var = clim_vars[i], path = tempdir())
    clim_lst <- append(clim_lst, list(clim))
  }
  clim_mosaic <- do.call(mosaic, clim_lst)
  clim_pts <- clim_mosaic |> terra::extract(shp_pts, ID = F)
  if(ncol(clim_all) == 0){
    clim_all <- clim_pts
  }else{
    clim_all <- cbind(clim_all, clim_pts)
  }
}

head(clim_all)
colnames(clim_all)

#elevation
dem1 <- geodata::elevation_30s(country = "Malawi", path = tempdir())
dem2 <- geodata::elevation_30s(country = "Mozambique", path = tempdir())
dem3 <- geodata::elevation_30s(country = "Zambia", path = tempdir())
dem <- terra::mosaic(dem1, dem2, dem3)
slope <- terra::terrain(dem, v = "slope")
TPI <- terra::terrain(dem, v = "TPI")
TRI <- terra::terrain(dem, v = "TRI")
topography <- c(dem, slope, TPI, TRI)
names(topography) <- c("elevation", "slope", "TPI", "TRI")

topo_pts <- topography |> terra::extract(shp_pts, ID = F)
head(topo_pts)

model_data <- cbind(data_loc, soil_all, clim_all, topo_pts)
dim(model_data)
colnames(model_data)

na_counts <- model_data |>
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}"))

write.csv(model_data, "E:/01CIAT/03Fertilizer/03southernAfricaRecom/data/workspace/trial_covariate.csv", row.names = F, col.names = T)
