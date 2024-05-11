# install rayshader & rayrender from the source
#install.packages("devtools")
# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("tylermorganwall/rayrender")

# Install & load libraries -------------

# libraries we need
libs <- c(
  "tidyverse", "R.utils",
  "httr", "sf", "stars",
  "rayshader", "here"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# load dataset -------------

file_name <- "kontur_population_CO_20231101.gpkg.gz"
#R.utils::gunzip(file_name, remove = F)
load_file_name <- gsub(".gz", "", file_name)

# Lambert projection 
# might not be the best solution for non-European countries
# if it creates a skewed map, please use:
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_population_data <- function() {
  pop_df <- sf::st_read(
    load_file_name
  ) |>
    #sf::st_transform(crs = crsLAEA)
    sf::st_transform(crs = crsLONGLAT)
  
}

pop_sf <- get_population_data()

head(pop_sf)
ggplot() +
  geom_sf(
    data = pop_sf,
    color = "grey10", fill = "grey10"
  )

# load boundaries -------------

bd_admin <-
  st_read("kontur_boundaries_CO_20230628.gpkg") |>
  st_transform(crs = crsLONGLAT)

distinct_names <- bd_admin |>
  distinct(name_en)
print(distinct_names)

# Creating BD Boundary
bd_boundary <-
  bd_admin %>%
  st_geometry %>%
  st_union %>%
  st_sf %>%
  st_make_valid()

# plot -------------



### 3. SHP TO RASTER
### ----------------

bb <- sf::st_bbox(pop_sf)

get_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  
  return(list(width_ratio, height_ratio))
}
width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

# size <- 3000 # initial size
size <- 3000 
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

get_population_raster <- function() {
  pop_rast <- stars::st_rasterize(
    pop_sf |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  return(pop_rast)
}

pop_rast <- get_population_raster()
plot(pop_rast)

pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

cols <- rev(c(
  "#0b1354", "#283680",
  "#6853a9", "#c863b3"
))

texture <- grDevices::colorRampPalette(cols)(256)

# Create the initial 3D object
pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = F,
    soliddepth = 0,
    zscale = 15,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

rayshader::render_highquality(
  filename = "colombia_population_2023.png",
  use_extruded_paths = T,
  path_material = rayrender::diffuse,
  preview = T,
  light = T,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = F,
  width = width, height = height
)
