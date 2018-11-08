########################################################## GOOGLE GEOLOCATION #########################################################

library(httr)
library(odbc)

API <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx"

vertica <- dbConnect(odbc::odbc(), "DWH")

system.time(R1 <- dbGetQuery(vertica,
                                               "
  select distinct
  SUBSTR(N.CELL_NAME, 1, 5) as CELL_NAME,
  ROUND(N.LATITUDE, 3) as LATITUDE,
  ROUND(N.LONGITUDE, 3) as LONGITUDE
        
  FROM    DWH_PROD.SGSN_FACTS S
  INNER JOIN  DWH_PROD.NETWORK_NODE N ON (S.NETWORK_NODE_ID = N.ID) 
          WHERE S.RECORDOPENINGTIME >= '2018-09-01'
        AND S.RECORDOPENINGTIME < '2018-10-01'
        and N.LATITUDE is not null
        and N.LONGITUDE is not null 
        
                                               ;
                                               ") 
)

write.csv2(R1,"~/MASTID.csv")

#R1 <- read.csv("~/MASTID.csv")

R <- R1

#rq <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=59.191944,24.982777&key=",API)
#test1 <- GET(rq)

i <- 1

R$address <- ""
R$linnaosa <- ""
R$Linn <- ""
R$MK <- ""
R$postal_code <-""

head(R)

for (i in 1:nrow(R)){
  
  rq <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",R$LATITUDE[i],",",R$LONGITUDE[i],"&key=",API)
  an <- GET(rq)
  an1 <- 
  
  apiResponse <- jsonlite::fromJSON(httr::content(an, "text"))
  ac <- apiResponse$results$address_components[[1]]
  
  if (content(an)$status == "OK") {
  
 
    # If address_components is available, look for 
    # the prefecture name. 
    if (!is.null(ac)) {
      
      R$address[i] <- content(an)$results[[1]]$formatted_address
      
      # Iterate the types of the current address_components.
      for (j in 1:length(ac$types)) {
        # Look for the administrative_area_level_1 in 
        # types of the address_components. In case of Japan, 
        # prefecture = administrative_area_level_1.  
        if (ac$types[[j]][[1]] == "administrative_area_level_1") {

          # If we find the MK from address_components,
          # pick the long_name and save it.
          R$MK[i] <- ac$long_name[[j]]
        }
        else if (ac$types[[j]][[1]] == "postal_code") {

          # If we find the post code from address_components,
          # pick the long_name and save it.
          R$postal_code[i] <- ac$long_name[[j]]
        }
        else if (ac$types[[j]][[1]] == "locality") {
          # If we find the city or town from address_components,
          # pick the long_name and save it.
          R$Linn[i] <- ac$long_name[[j]]
        }
        else if (length(ac$types[[j]])>1) if (ac$types[[j]][[2]] == "sublocality") {
          # If we find the civil entity below a locality from address_components,
          # pick the long_name and save it.
          R$linnaosa[i] <- ac$long_name[[j]]
        }
        
      }
    }
  
    print(R[i,])
    
    }
  
  else {
    an1 = content(an)$error_message 
    print(an1)
  }
  
}

# write results back to DWH    
# dbWriteTable(vertica, name="NETWORK_NODE_GEO_DETAILS", value=head(R), row.names=FALSE, overwrite=TRUE)
# dbGetQuery(vertica, "SELECT * FROM NETWORK_NODE_GEO_DETAILS")
    
# write results to CSV
write.csv2(R,"~/MASTID_REGIOONID.csv")    



