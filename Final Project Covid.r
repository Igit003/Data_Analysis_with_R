<center>
<img src="https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/M5_Final/images/SN_web_lightmode.png" width="300">
</center>

# This lab requires 'httr' and 'rvest'packages, which are already pre-loaded into this lab environment.
# However, if you are working on your local RStudio, please uncomment the below codes and install the packages.

#install.packages("httr")
#install.packages("rvest")

install.packages("httr")

library(httr)
library(rvest)


get_wiki_covid19_page <- function() {
    
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
    # 1) base URL `https://en.wikipedia.org/w/index.php  
    # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
    
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  query_params <- list(title = "Template:COVID-19_testing_by_country")
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
  response <- GET(url = wiki_base_url, query = query_params)
  # Use the `return` function to return the response
  return(response)
}


# Call the get_wiki_covid19_page function and print the response
covid_response <- get_wiki_covid19_page()
print(covid_response)

library(rvest)
# Get the root html node from the http response in task 1 
root_node <- read_html(covid_response)
root_node

# Get the table node from the root html node
table_node <- html_nodes(root_node, "table")
table_node

# Read the table node and convert it into a data frame, and print the data frame for review
covid_data_frame <- as.data.frame(html_table(table_node[[2]]))
head(covid_data_frame)

# Print the summary of the data frame
summary(covid_data_frame)

preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
    data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units.b."] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}


# call `preprocess_covid_data_frame` function and assign it to a new data frame
covid_data_frame_processed <- preprocess_covid_data_frame(covid_data_frame)

# Print the summary of the processed data frame again
summary(covid_data_frame_processed)

# Export the data frame to a csv file
write.csv(covid_data_frame_processed, "covid.csv", row.names = FALSE)

# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
# File path
print(file_path)
file.exists(file_path)

## Download a sample csv file
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Read covid_data_frame_csv from the csv file
covid_data_frame_csv <- read.csv("covid.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# Get the 5th to 10th rows, with two "country" "confirmed" columns
covid_data_frame_csv[5:10, c("country", "confirmed")]
## TASK 5: Calculate worldwide COVID testing positive ratio

The goal of task 5 is to get the total confirmed and tested cases worldwide, and try to figure the overall positive ratio using `confirmed cases / tested cases`

# Get the total confirmed cases worldwide
total_confirmed <- sum(covid_data_frame_csv$confirmed, na.rm = TRUE)
# Get the total tested cases worldwide
total_tested <- sum(covid_data_frame_csv$tested, na.rm = TRUE)
# Get the positive ratio (confirmed / tested)
positive_ratio <- total_confirmed / total_tested

print(total_confirmed)
print(total_tested)
print(positive_ratio)

# Get the `country` column
country_col <- covid_data_frame_csv$country
# Check its class (should be character)
class(country_col)
# Sort the countries AtoZ
countries_atoz <- sort(country_col)
# Sort the countries ZtoA
countries_ztoa <- sort(country_col, decreasing = TRUE)
# Print the sorted ZtoA list
print(countries_ztoa)

# Use a regular expression `United.+` to find matches
united_matches <- grep("United.+", covid_data_frame_csv$country, value = TRUE)
# Print the matched country names
print(united_matches)

# Select a subset (should be only one row) of data frame based on a selected country name and columns
country_one <- covid_data_frame_csv[covid_data_frame_csv$country == "United States", 
                                    c("country", "confirmed", "confirmed.population.ratio")]
# Select a subset (should be only one row) of data frame based on a selected country name and columns
country_two <- covid_data_frame_csv[covid_data_frame_csv$country == "India", 
                                    c("country", "confirmed", "confirmed.population.ratio")]

print(country_one)
print(country_two)

# Use if-else statement
if (country_one$confirmed.population.ratio > country_two$confirmed.population.ratio) {
    print(paste(country_one$country, "has a lager ratio of confirmed case to population"))
 } else {
    print(paste(country_two$country, "has a lager ratio of confirmed case to population"))
 }


# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
threshold <- 1
low_risk <- covid_data_frame_csv[which(covid_data_frame_csv$confirmed.population.ratio < threshold),
                                c("country", "confirmed.population.ratio")]
print(low_risk)


