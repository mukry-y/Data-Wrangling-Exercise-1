# Load libraries
library(dplyr)
library(tidyverse)
library(readr)

# read CSV file to dataframe
refine_original <- read.csv("/Users/Duane/Documents/Data Science- Springboard/Data Wrangling/Ex 1/refine_original.csv")
refine_original_df <- tbl_df(refine_original)

#2. Split product code and number
refine_original_df <- refine_original_df %>%
  separate(col="Product.code...number", into = c("product_code", "product_number"), "-" )

#3. Add a column with the product category for each record
refine_original_df <- refine_original_df %>%
  mutate(product_category = case_when(
    product_code == "p" ~ "Smartphone",
    product_code == "v" ~ "TV",
    product_code == "x" ~ "Laptop",
    product_code == "q" ~ "Tablet")
  ) 

#4. Create a new column full_address that concatenates the 
#   three address fields (address, city, country), separated by commas.
refine_original_df <- refine_original_df %>%
  unite(full_address, address:country, sep=", ")

glimpse(refine_original_df)

#5. Create dummy binary variables for each of Compnay and Product 
#   with the prefix company_ and product_
#Product Dummy
refine_original_df <- refine_original_df %>%
  mutate(product_smartphone = ifelse(product_code =="p", 1,0)) %>%
  mutate(product_tv = ifelse(product_code == "v", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_code == "x", 1, 0)) %>%
  mutate(product_tablet=  ifelse(product_code == "q", 1, 0))
         
#1. clean company column 
# using case_when

refine_original_df <- refine_original_df %>% 
  #  mutate(company = tolower(company)) %>%
  mutate(company = case_when(
    company == "p%" | company == "P%" | company == "f%" ~ "phillips"))


#1. clean company column 
#using ifelse

refine_original <- refine_original %>% 
  mutate(company_new = tolower(company),
         company_new = ifelse(
           company == "P%" | company == "p%" | company == "f%", "phillips", 
           ifelse(company == "a%" | company == "A%", "akzo",
                  ifelse(company == "v%" | company == "V%", "van houten",
                         ifelse(company == "u%" | company == "U%", "unilever", company)
                  )
                  
           )
         ) 
  )


write.csv (refine_original_df, file= "refine_clean.csv" )
