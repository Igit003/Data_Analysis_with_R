# Data_Analysis_with_R
R project that scrapes COVID-19 testing data by country from Wikipedia, cleans it, and analyzes it with httr, rvest, and base R.
COVID-19 Testing Data Analysis in R

This project scrapes COVID-19 testing statistics by country from a public Wikipedia page and turns them into a clean, analysis-ready dataset. Using httr and rvest, it extracts the HTML table into a data frame, then pre-processes it by renaming columns, converting data types, and removing irrelevant rows. The cleaned data is exported to CSV and used to:

Subset rows and columns
Calculate the worldwide positive test ratio (confirmed / tested)
Sort and filter countries, including regex pattern matching
Compare confirmed-case-to-population ratios between countries
Identify countries below a given infection-ratio threshold

Tools: R, Jupyter Notebook, httr, rvest

Built as the final project for the IBM Data Science / R programming course.
