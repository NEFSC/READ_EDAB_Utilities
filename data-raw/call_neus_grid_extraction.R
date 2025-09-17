#convert neus_grid.rds to shape file then extract coordinates
grid.file = here::here('data-raw','neus_grid.rds')
grid.sf = readRDS(grid.file) 
grid.sf = sf::st_as_sf(grid.sf)
grid.shp = terra::vect(grid.sf)
grid.center =terra::centroids(grid.shp)

EDABUtilities::extract_daily_coord(input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
                                   input.prefix = 'GLORYS_REANALYSIS_',
                                   input.type = 'daily',
                                   output.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/bts_stations/',
                                   output.prefix = 'bottom_trawl_survey_stations_GLORYS_2022_',
                                   coordinates = readRDS(here::here('data-raw','station_locations.rds')) %>% rename(lat = 'LAT',lon = 'LON',date = 'EST_TOWDATE'),
                                   var.name = 'theao')