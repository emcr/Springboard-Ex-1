.libPaths("C:/Users/emily_rinaldi/Desktop/R/win-library/3.2")
setwd("C:/Users/emily_rinaldi/Desktop/Springboard/Exercise 1")
library(dplyr)
library(tidyr)
library(readr)

#Read in dataset and convert to tbl class
refine_orig <- read.csv("refine_original.csv")
refine_orig <- tbl_df(refine_orig)

#Create list of unique company names to normalize
unique_comps <- refine_orig %>% 
  transmute(lowers = tolower(company)) %>% 
  arrange(lowers) %>%
  distinct(lowers)

#Create a dataframe to lookup normalized company names
norm_names <- c(rep("akzo", times = 3), rep("phillips", times = 6), rep("unilever", times = 2), "van houten")
norm_comps_df <- data_frame(company = as.vector(unique_comps$lowers), norm_names)

#Lookup and replate normalized company names
refine_2 <- refine_orig %>% 
  mutate(company = tolower(company)) %>% 
  left_join(norm_comps_df) %>% 
  mutate(company = norm_names) %>% 
  select(-norm_names)


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

  