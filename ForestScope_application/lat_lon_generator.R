# Load necessary libraries
library(data.table)

# Define country_codes and country_names
country_codes <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,72,80,95,96)
country_names <- c("FR","BE","NL","DE","IT","UK","IE","DK","GR","PT","ES","LU","SE","AT","FI","CH","HU","RO","PL","SK","NO","LT","HR","CZ","EE","SI","MD","RU","BG","LV","BY","CY","CS","AD","TR","ME","cn","AZ")

# Create an empty data.table
result <- data.table(CountryCode = integer(), CountryName = character(), Site = integer(), Lat = numeric(), Lon = numeric())

# Iterate through the lat_lon list
for (i in 1:length(d$lat_lon)) {
  country_code <- country_codes[i]
  country_name <- country_names[i]

  for (j in 1:length(d$lat_lon[[i]])) {
    if(j==1) (j<- j+1)
    site <- names(d$lat_lon[[i]][j])
    if(!is.na(site)){
    lat <- d$lat_lon[[i]][[site]]$lat
    lon <- d$lat_lon[[i]][[site]]$lon

    # Append the current row to the result data.table
    result <- rbind(result, data.table(CountryCode = country_code, CountryName = country_name, Site = site, Lat = lat, Lon = lon), fill = TRUE)
    }
  }
}


# Write the data.table to a txt file
fwrite(result, "../IIASA_ICP_application/latlon_table.txt", sep = ",", quote = FALSE, row.names = FALSE)

# Print the result
print(result)
