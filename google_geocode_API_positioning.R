library(ggmap)

calling <- dbConnect(odbc::odbc(), "DWH")


mobility_loc <- dbFetch(dbSendQuery(calling, 
                                    "   
                                    SELECT
                                    S.SUSG_REF_NUM,
                                    S.RECORDOPENINGTIME,
                                    S.SERVEDMSISDN,
                                    N.CELL_NAME,
                                    N.LATITUDE ,
                                    N.LONGITUDE 
                                
                                    FROM    DWH_PROD.SGSN_FACTS S
                                    LEFT OUTER JOIN  DWH_PROD.NETWORK_NODE N ON (S.NETWORK_NODE_ID = N.ID) 
                                    WHERE S.RECORDOPENINGTIME >= '2018-02-01'
                                    AND S.RECORDOPENINGTIME < '2018-03-01'
                                    AND SERVEDMSISDN = 'XXXXXXXX'
                                 
                                    "
), n=-1)


library(googleway)
google_geocode(address = "Tallinn",
               key = '')

head(mobility_loc)

# Get Map background
map <- get_map(location = c(lon = 24.67787, lat = 59.45713), 
               zoom = 10, source = "stamen", maptype = "toner-background")

# Get Google Map background
map <- get_map(location = c(lon = 24.67787, lat = 59.45713), 
               zoom = 10, source = "google", maptype = "hybrid", api_key="")

# API key 
register_google(key = '')

# plot
ggmap(map) + 
  geom_path(data = mobility_loc, aes(x= as.numeric(LONGITUDE), y= as.numeric(LATITUDE), color= SUSG_REF_NUM ), alpha=0.3) +
  coord_map(xlim = c(24.65, 24.85),ylim = c(59.40, 59.47)) +
  theme(legend.position="none") 

