# This code downloads Argentina's National Public Sector Savings-Investment-Funding Account, 
#for the years required (possible since 1997)
rm(list=ls())
gc()
# Open packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(glue)
library(rlist)
start.time=Sys.time()

##Folder creation
setwd("C:/Users/lcalcagno/Documents/Investigación/argentina_fiscal_pension_data")

if(!file.exists("AIF")) {
  dir.create("AIF")
}

setwd("AIF/")
getwd()
#Select years to import -------
#We left here the code to import all months, since 1997. It takes a while. Feel free to 
#comment out the years you are not interested in.
year <- c("00","01","02","03","04","05","06","07","08","09",
          "10","11","12","13","14","15","16","17","18","19",
          "20","21","22"
) 
year_90<-c("97","98","99")
#year_short<-sapply(year,function(x) substr(start=3,stop=4,x))

#Possible imported months and years names
month <- c("ene", "feb", "mar", "abr", "may", "jun", "jul",
           "ago", "sep", "oct", "nov", "dic", 
           "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre", "octubre", "noviembre", "diciembre")


#Put different names for each option, or they get overwritten
numeric_month <- c("01", "02", "03", "04", "05", "06", "07",
                   "08", "09", "10", "11", "12", 
                   "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                   "agosto", "septiembre", "octubre", "noviembre", "diciembre")



###Download files 2000 onward-----
##Possible names and links, with and without .xls
urls_xls <- 
  tidyr::expand_grid(month, year) %>%
  glue_data("https://www.economia.gob.ar/onp/documentos/resultado/caja/c20{year}/archivos/{month}{year}.xls")

urls_xlsx <- 
  tidyr::expand_grid(month, year) %>%
  glue_data("https://www.economia.gob.ar/onp/documentos/resultado/caja/c20{year}/archivos/{month}{year}.xlsx")

# File names for xls y xlsx
names_xls <- 
  tidyr::expand_grid(numeric_month, year) %>%
  glue_data("20{year}_{numeric_month}.xls")

names_xlsx <- 
  tidyr::expand_grid(numeric_month, year) %>% 
  glue_data("20{year}_{numeric_month}.xlsx")

#---- Download excels with purrr library

walk2(urls_xls,names_xls,safely(download.file), mode = "wb")

walk2(urls_xlsx,names_xlsx,safely(download.file), mode = "wb")

###Download files 1997-1999-----
##Possible names and links, 1990s
urls_xls_90 <- 
  tidyr::expand_grid(month, year_90) %>%
  glue_data("https://www.economia.gob.ar/onp/documentos/resultado/caja/c19{year_90}/archivos/{month}{year_90}.xls")

# File names for xls, 1990s
names_xls_90 <- 
  tidyr::expand_grid(numeric_month, year_90) %>%
  glue_data("19{year_90}_{numeric_month}.xls")


#---- Download excels with purrr library

walk2(urls_xls_90,names_xls_90,safely(download.file), mode = "wb")


#September 2001 has a different URL, we download it manually


download.file(
  url = "https://www.economia.gob.ar/onp/documentos/resultado/caja/c2001/archivos/set01.xls", 
  destfile = "2001_09.xls", mode='wb'
)
rm(month,year,year_90,numeric_month,names_xls,names_xls_90,urls_xls,urls_xls_90,urls_xlsx,names_xlsx)


end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken)
