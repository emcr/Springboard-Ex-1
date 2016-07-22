#Set package location and load libraries
.libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.2")
library(dplyr)
library(tidyr)
library(readr)

#Read in dataset and convert to tbl class
refine_orig <- read.csv("refine_original.csv")
refine_orig <- tbl_df(refine_orig)

#Standardize company names
refine_2 <- refine_orig %>%
  mutate(company=tolower(company)) %>%
  mutate(company=ifelse(grepl('^ph|fi', company), 'phillips', company)) %>%
  mutate(company=ifelse(grepl('^ak', company), 'akzo', company)) %>%
  mutate(company=ifelse(grepl('^uni', company), 'unilever', company))


#Add Product_Category variable based on first character of Product.code...number variable
refine_3 <- refine_2 %>% 
  separate(Product.code...number, c("product_code", "product_number"), sep = "-") %>%
  mutate(product_category = 
           ifelse(product_code == "p", "Smartphone", 
                 ifelse(product_code == "v", "TV", 
                        ifelse(product_code == "x", "Laptop", 
                               "Tablet")
                        )
                 )
         )

#Add concatenated address variable
refine_4 <- refine_3 %>%
  unite(col = full_address, address:country, sep = ", ", remove = FALSE)

#Add dummy variables for company and product category
refine_5 <- refine_4 %>%
  cbind(model.matrix(~refine_4$company-1)) %>%
  cbind(model.matrix(~refine_4$product_category-1)) %>%
  tbl_df()

#Create refine_clean dataframe with renamed dummy variables
refine_clean <- refine_5 %>%
  select(company:product_number, product_category, full_address:name, 
         company_akzo = `refine_4$companyakzo`,
         company_phillips = `refine_4$companyphillips`,
         company_unilever = `refine_4$companyunilever`,
         company_van_houten = `refine_4$companyvan houten`,
         product_smartphone = `refine_4$product_categorySmartphone`,
         product_tv = `refine_4$product_categoryTV`,
         product_laptop = `refine_4$product_categoryLaptop`,
         product_tablet = `refine_4$product_categoryTablet`) %>%
  arrange(company)
  
#create refine_clean.csv file in working directory
write_csv(refine_clean, paste(getwd(),"/refine_clean.csv",sep = ""))

  