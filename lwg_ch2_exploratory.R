## Script for analysis on reserve conservation actions --------------------------------------
## Read in the excel sheet

xl_data<-"C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx"

excel_sheets(path = xl_data)

tab_names <- excel_sheets(path = xl_data)

list_all<- lapply(sheets, function(x) read_excel(path = xl_data, sheet = x))

# Convert from list into data frame

df<- bind_rows(list_all) # doesn't quite work because of year 1993. Try another approach

df_doCall <- do.call("rbind", list_all) ## same problem I think


## online version from Youtube video "Combine Multiple Excel Sheets into a Single Table using R Tidyverse"
a<-excel_sheets("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx") %>% 
  map(~read.xlsx("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx",.))

## Problem seems to be that I have named the year variables as actual numerical value. Instead I create a function and join them together 

## NEW APPROACH - It doesn't make senseto do it this way. We only need a few of the sheets so let's create a function that 
## extracts an individual sheet and converts it into tidy format

cons_sheet<-function(sheet_name, value_name) {
read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions.xlsx", sheet = sheet_name) %>% 
  pivot_longer(cols = 3:28,
               names_to = "year",
               values_to = value_name)

}

test1<-cons_sheet(sheet_name = "fox_control", value_name = "foxes_killed")
test1$


## Before continuing I need to find a clever way to fill out NA's and true zeros. 
## first read in the missing data sheet
status_sheet<-cons_sheet(sheet_name = "status_sheet", value_name = "comments")
