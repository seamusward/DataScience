# Lab 3 - Amalgamating the NI crime dataset
# Auhor: Seamus Ward
# Purpose : To amalgamate and clean NI crimee data set


#### [a] Amalgamate all the crime csv files   ######
# Create csv file list
csv_file_list <- list.files(path = "Data", pattern = "*street.csv")
csv_file_list[1]
#Create data frame to store crime data
crime_data <- data.frame()

# Function to combine all csv data into one data frame
combine_results <- function(file_list) {
    comp_data <- data.frame()
    for (csv_file in file_list) {

        crime_data <- read.csv(header = TRUE, paste("Data/", csv_file, sep = ""), stringsAsFactors = FALSE)
        comp_data <- rbind(comp_data, crime_data)

    }
    return(comp_data)
}
# my_data holds combined data
my_data <- combine_results(csv_file_list)
head(my_data)  # Check first few data frame entries
tail(my_data) # Check last few data frame entries
str(my_data) # Check structure of data frame
# Save dataset into csv file called AllNICrimeData.csv, Don't write row index values
write.csv(my_data, file = "Data/AllNICrimeData.csv", row.names = FALSE)
nrow(my_data) # Count and Show the number of rows in the data frame
ncol(my_data) # Count and Show the number of columns in the data frame


##### [b] Modify structure of new AllNICrimeData.csv file #########
all_crime_data <- read.csv(header = TRUE, file = "Data/AllNICrimeData.csv", stringsAsFactors = FALSE)
str(all_crime_data)
head(all_crime_data)
# Remove required attributes "Crime.ID", "Reported.by","Falls.within","LSOA.code",            
# "LSOA.name", "Last.outcome.category" and "Context"
new_crime_data <- all_crime_data[, - c(1, 3, 4, 8, 9, 11, 12)]
head(new_crime_data)
str(new_crime_data)
write.csv(new_crime_data, file = "Data/AllNICrimeData.csv", row.names = FALSE)
AllNICrimeData <- read.csv(header = TRUE, file = "Data/AllNICrimeData.csv", stringsAsFactors = FALSE)
str(AllNICrimeData)

##### [c] Factorise the Crime Type Attribute   ##############
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
# Show factored crime type attribute
str(AllNICrimeData)
# Show factored levels
levels(factor(AllNICrimeData$Crime.type))
unique(AllNICrimeData$Crime.type)

#### [d] Modify AllNICrimeData  so location attribute only contains street name
head(AllNICrimeData$Location)
AllNICrimeData$Location <- gsub( "On or near\\s", "", AllNICrimeData$Location, ignore.case = TRUE)
# Modify resultant empty strings with suitable idenetifier - use NA
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
head(AllNICrimeData$Location)

#### [e] Create a function called find_a_postcode that uses location data to find suitable postcode
post_code_data <- read.csv(header = TRUE, file = "Data/CleanNIPostcodeData.csv", stringsAsFactors = FALSE)
find_a_postcode <- function(location) {
    x <- sapply(location, function(x) grepl(tolower(x), tolower(post_code_data$Primary_Thorfare)))
        postcodes <- post_code_data$Postcode[x == "TRUE"]
        max_postcode <- names(sort(table(postcodes), decreasing = TRUE)[1])
    loc_data <- c(location, max_postcode)
    return(loc_data)
}
all_loc_pc <- data.frame()
location <- unique(AllNICrimeData$Location)
for (loc in location) {
    #print(paste("Location is", loc))
    loc_postcode_data <- find_a_postcode(loc)
    all_loc_pc <- rbind(all_loc_pc, loc_postcode_data, stringsAsFactors=FALSE)
}
str(location)
loc_postcode_data
head(all_loc_pc)
tail(all_loc_pc)
str(all_loc_pc)
write.csv(all_loc_pc, file = "Data/AllLocationPostCodeData.csv", row.names = FALSE)

#### [f] Append the data output from find_a_postcode to AllNICrimeData dataset.  #######
head(AllNICrimeData)
postcode_data <- read.csv(header = TRUE, file = "Data/AllLocationPostCodeData.csv", stringsAsFactors = FALSE)
head(postcode_data)
colnames(postcode_data) <- c("Location", "Postcode")
AllNICrimeData <- merge(AllNICrimeData, postcode_data, by = "Location")
head(AllNICrimeData)
str(AllNICrimeData)

#### [g] Function to use longitude and latitude to find missing location information #########
tidy_location <- function(location) {
    x <- sapply(location, function(x) grepl(tolower(x), tolower(post_code_data$Primary_Thorfare)))
    postcodes <- post_code_data$Postcode[x == "TRUE"]
    max_postcode <- names(sort(table(postcodes), decreasing = TRUE)[1])
    loc_data <- c(location, max_postcode)
    return(loc_data)
}



#### [h] Append the AllNICrimeData dataset with new attributes Town, County and Postcode
head(AllNICrimeData)
head(post_code_data)
colnames(post_code_data)[colnames(post_code_data) == "Primary_Thorfare"] <- "Location"
str(AllNICrimeData)
AllNICrimeData <- merge(AllNICrimeData, post_code_data[ , c("Town","County")], by = "Location")
head(AllNICrimeData)

##merge(x = DF1, y = DF2[, c("Client", "LO")], by = "Client", all.x = TRUE)

#### [i] Save your modified AllNICrimeData dataset in a csv file called FinalNICrimeData
write.csv(AllNICrimeData, file = "Data/FinalNICrimeData.csv", row.names = FALSE)

#### [j] Search all crime data where data contains Strabane and postcode contains BT82
Strabane_crime_data <- AllNICrimeData[which(Town == "STRABANE" | grep("^BT48", Postcode)]
head(Strabane_crime_data, 10)
write.csv(Strabane_crime_data, file = "Data/StrabaneData.csv")
