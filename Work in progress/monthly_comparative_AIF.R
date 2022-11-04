# This code uploads the monthly level of total public sector expenditure and income by type, 
    #usually the second sheet of Argentina's national Public Sector Savings-Investment-Funding Acount; concatenates
    #the bases in a single file; and outputs a time series of all the monthly comparison income and expenditure.
rm(list=ls())
gc()
# Open packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(rlist)
library(XML)
library(RCurl)
start.time=Sys.time()

## Set wd to the folder with AIF files

setwd("C:/Users/lcalcagno/Documents/Investigación/argentina_fiscal_pension_data")
setwd("AIF/")
getwd()
#Import the relevant excel files --------
#Code taken from https://stackoverflow.com/questions/32888757/how-can-i-read-multiple-excel-files-into-r
#The comparative sheet is only available starting from January 2017, and gives information starting from Jan. 2016
start.time=Sys.time()
list_xls<-list.files(pattern='*.xls')
df_list_xls <- as.data.frame(list.files(pattern='*.xls')) %>%
  rename(file_name=1) %>% 
  mutate(year=as.integer(substr(start=1,stop=4,file_name))) %>% 
  subset(year>=2017) %>%  
  select(c(file_name)) %>% 
  t()

list_xls<-as.character(df_list_xls)


view(list_xls)

read_third_sheet <- function(path) { #Sometimes, the correct table is in the third sheet
  x <-try(read_excel(path=path, sheet = 3)) #Some tables have less than 3 sheets: try() keeps the code running even through errors
}

read_second_sheet <- function(path) { #Sometimes, the correct table is in the second sheet
  x <-try(read_excel(path=path, sheet = 2)) #Some tables have only one sheet: try() keeps the code running even through errors
}
read_excel_as_df <- function(path) {
  x<-try(as.data.frame(read_excel(path=path)))  
}
df_list_xls <- sapply(list_xls, read_excel_as_df,simplify=FALSE)#This keeps the file names 
df_list_s2_xls <- sapply(list_xls, read_second_sheet,simplify=FALSE)
df_list_s3_xls <- sapply(list_xls, read_third_sheet,simplify=FALSE)



##The read_second_sheet() and read_third_sheet() function allow for errors, so the end result is a list with both characters
#and lists. We thus need to first remove non-list elements from the list of lists. 
keep_s2_xls<-sapply(df_list_s2_xls,is.list)
keep_s3_xls<-sapply(df_list_s3_xls,is.list)
df_list_s2_xls<- df_list_s2_xls [which(keep_s2_xls==TRUE)]
df_list_s3_xls<- df_list_s3_xls [which(keep_s3_xls==TRUE)]

##We only want the sheet with the year-on-year comparison. It usually has more than 80 lines and around 14 variables
n.cols_xls<-unlist(lapply(df_list_xls, function(t) dim(t) [2])) #[1] for rows, [2] for columns
n.cols_s2_xls<-unlist(lapply(df_list_s2_xls, function(t) dim(t) [2]))
n.cols_s3_xls<-unlist(lapply(df_list_s3_xls, function(t) dim(t) [2]))

n.rows_xls<-unlist(lapply(df_list_xls, function(t) dim(t) [1])) #[1] for rows, [2] for columns
n.rows_s2_xls<-unlist(lapply(df_list_s2_xls, function(t) dim(t) [1]))
n.rows_s3_xls<-unlist(lapply(df_list_s3_xls, function(t) dim(t) [1]))

#If "Variación anual is present somewhere, the grep() function returns "1"

is_monthly_comparative<-function(indata){
detect_string<-as.character(sapply(colnames(indata),function(x) grep("Dato mensual",indata[,x])))

detect_string<-as.data.frame(grepl("integer(0)",detect_string,fixed=TRUE)) %>% #integer(0) means there is no "Dato mensual" in the column
  rename(is_correct=1) %>% 
  mutate(correct_sheet=ifelse(grepl("TRUE",is_correct,fixed=TRUE),0,
                              1) #This means if there is one or more "Dato mensual" in the column, it identifies it as the correct sheet.
         ) %>% 
  summarise(correct_sheet=sum(correct_sheet)) 

}

list_detect_comparative<-sapply(df_list_xls,is_monthly_comparative,simplify=TRUE)
list_detect_comparative_2<-sapply(df_list_s2_xls,is_monthly_comparative,simplify=TRUE)
list_detect_comparative_3<-sapply(df_list_s3_xls,is_monthly_comparative,simplify=TRUE)


test1<- df_list_xls [which(list_detect_comparative>=1)]
test2<- df_list_s2_xls [which(list_detect_comparative_2>=1)]
test3<- df_list_s3_xls [which(list_detect_comparative_3>=1)]

list_files_1<-names(test1)
list_files_2<-names(test2)
list_files_3<-names(test3)
list_files<-c(list_files_1,list_files_2,list_files_3) %>% 
  as.data.frame() %>% 
  rename(file_name=1) %>% 
  arrange(file_name) %>% 
  group_by(file_name) %>% 
  tally() %>% 
  ungroup()
#There are many months where there are a lot of hidden sheets (from August 2021 to August 2022), that have off date
    #monthly comparison. Correcting these errors requires manual work, and cannot be done through a function.  


control_files<-list_files %>% 
  unique() %>% 
  mutate(is_present=1)
control_list<-as.data.frame(list_xls) %>% 
  rename(file_name=1) %>% 
  left_join(control_files)


