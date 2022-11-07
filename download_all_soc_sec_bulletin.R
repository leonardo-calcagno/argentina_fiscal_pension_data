
# # This code downloads Argentina's Monthly Social Security Bulletin from AFIP, 
#both monthly excels (from May 2003 to December 2016) and yearly excels (from January 2017 onward)
#Takes 3.3 min in 8 GB RAM laptop
library(xml2)
library(rvest)
library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(glue)
library(rlist)

rm(list=ls())
gc()
start.time=Sys.time()
setwd("C:/Users/lcalcagno/Documents/Investigación/MISSAR_private/R_files_for_MISSAR")

if(!file.exists("bol_men_ss")) {
  dir.create("bol_men_ss")
}
setwd("bol_men_ss/")
getwd()

detect_excel<-function(input_url){
  #This function detects in the parent link (constant over time) the URL that downloads an excel file
  
  URL <- input_url
  
  pg <- read_html(URL)
  
  list_urls<-as.data.frame(html_attr(html_nodes(pg, "a"), "href")) %>% 
    rename(list_href=1) %>% 
    mutate(detect_excel=ifelse(grepl("*.xls",list_href),1,
                               0)
    ) %>% 
    subset(detect_excel==1) %>% 
    select(c(1)) %>% 
    as.character()
}

download_yearly_ss_bulletin<-function(year){
  
  
  try_url<- 
    tidyr::expand_grid(year) %>%
    glue_data("https://www.afip.gob.ar/institucional/estudios/boletines-mensuales-de-seguridad-social/{year}.asp") %>% 
    detect_excel()
  
  
  # File names for xls y xlsx
  names_xls <- 
    tidyr::expand_grid(year) %>%
    glue_data("{year}.xls")
  
  download.file(try(try_url),destfile=names_xls,mode="wb")
}

download_yearly_ss_bulletin(2022)
download_yearly_ss_bulletin(2021)
download_yearly_ss_bulletin(2020)
download_yearly_ss_bulletin(2019)
download_yearly_ss_bulletin(2018)
download_yearly_ss_bulletin(2017)

#For the 2003-2016 period, the URL pattern is stable, but it is monthly. Also some links are
#broken in the AFIP page, so it is better to input the full URL. 


#Select years to import -------
year <- c("03","04","05","2006","2007","2008","2009",
          "2010","2011","2012","2013","2014","2015","2016"
) 

#Possible imported months and years names
month <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre","setiembre", "octubre", "noviembre", "diciembre",
           "ene", "feb", "mar", "abr", "may", "jun", "jul",
           "ago", "sep", "oct", "nov", "dic" 
)


#Put different names for each option, or they get overwritten
numeric_month <- c("01", "02", "03", "04", "05", "06", "07",
                   "08", "09", "09b", "10", "11", "12", 
                   "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                   "agosto", "septiembre", "octubre", "noviembre", "diciembre")



urls_xls <- 
  tidyr::expand_grid(month, year) %>%
  glue_data("http://contenidos.afip.gob.ar/institucional/estudios/archivos/{month}{year}.xls")


# File names for xls y xlsx
names_xls <- 
  tidyr::expand_grid(numeric_month, year) %>%
  glue_data("{year}_{numeric_month}.xls")

#---- Download excels with purrr library

walk2(urls_xls,names_xls,safely(download.file), mode = "wb")

###Download files 2005-2007-----
##Possible names and links, with and without .xls (these months follow a different URL pattern)

if(!file.exists("alternate_url")) {
  dir.create("alternate_url")
}

setwd("alternate_url/")
getwd() #This avoids overwriting months correctly imported in the previous routine

#Select years to import -------
year <- c("03","04","05","06",
          "2005","2006","2007"
) 

#Possible imported months and years names
month <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre","setiembre", "octubre", "noviembre", "diciembre",
           "ene", "feb", "mar", "abr", "may", "jun", "jul",
           "ago", "sep", "oct", "nov", "dic" 
)


#Put different names for each option, or they get overwritten
numeric_month <- c("01", "02", "03", "04", "05", "06", "07",
                   "08", "09", "09b", "10", "11", "12", 
                   "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                   "agosto", "septiembre", "octubre", "noviembre", "diciembre")


urls_xls <- 
  tidyr::expand_grid(month, year) %>%
  glue_data("https://contenidos.afip.gob.ar/institucional/estudios/archivos/boletin.{month}{year}.xls")


# File names for xls y xlsx
names_xls <- 
  tidyr::expand_grid(numeric_month, year) %>%
  glue_data("{year}_{numeric_month}.xls")


walk2(urls_xls,names_xls,safely(download.file), mode = "wb")


#March 2007 has a different URL, we input it manually
download.file("https://contenidos.afip.gob.ar/institucional/estudios/archivos/boletin.Marzo2007.xls",destfile="2007_03.xls")
###The first quarter of 2007 data has a different format 

list_xls <- list.files(pattern='*.xls')

file.copy(from=list_xls, to="../", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
#We copy these alternate URLs to the main monthly social security bulletin folder. We actively 
#want to overwrite files present in both folders, because the first walk instruction 
#downloaded some incorrect files for year 2005
setwd("../")
getwd()
unlink("alternate_url",recursive=TRUE)
unlink("05_09b.xls",recursive=TRUE) #There was one wrong september file left ove

end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken)