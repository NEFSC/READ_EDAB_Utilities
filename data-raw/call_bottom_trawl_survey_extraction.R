EDABUtilities::extract_daily_coord(input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_REANALYSIS_',
  input.type = 'daily',
  output.dir = 'C:/Users/joseph.caracappa/Documents/Data/GLORYS/bts_stations/',
  output.prefix = 'bottom_trawl_survey_stations_GLORYS_2022_',
  coordinates = readRDS(here::here('data-raw','station_locations.rds')) %>% rename(lat = 'LAT',lon = 'LON',date = 'EST_TOWDATE'),
  var.name = 'theao')