
###This code downloads Argentina's latest CPI index, and outputs excel files for each 
    #component of the CPI basket, with data by region. The base period is December 2016. 
rm(list=ls())
gc()

# Open packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(janitor)
##Generate temporary download folder----

setwd("C:/Users/lcalcagno/Documents/Investigación/argentina_fiscal_pension_data")

if(!file.exists("download_folder")) {
  dir.create("download_folder")
}
setwd("download_folder/")

###Download latest CPI excel from INDEC website -----

#We automate the URL for downloading the latest inflation figures: for figures of a given month (say, January), the URL is 
#named after the following month (here, February).
date<-Sys.Date()
year<-substr(date,start=3,stop=4)
month<-substr(date,start=6,stop=7)
#day<-substr(date,start=9,stop=10)
lag_month<-as.integer(month)-1
lag_year<-as.integer(year)-1
lag_year<- as.character(lag_year)

lag_month<- if(lag_month==0) {12
}else {lag_month
}

lag_month<- if (lag_month>9) {as.character(lag_month)
} else {
  paste0("0",lag_month)
}

CPI_url <- paste0("https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_",month,"_",year,".xls")
alt_CPI_url<- if (month=="01") {paste0("https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_",lag_month,"_",lag_year,".xls")
} else {paste0("https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_",lag_month,"_",year,".xls")
}
rm(date,year,month,day,lag_year,lag_month)

urlFileExist <- function(url){ #Source: https://stackoverflow.com/questions/60318926/how-to-check-if-file-exists-in-the-url-before-use-download-file-in-r
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}

current_month_exists<-urlFileExist(CPI_url)$exists
last_month_exists<-urlFileExist(alt_CPI_url)$exists

correct_CPI_url<- if(current_month_exists==TRUE){CPI_url
}else if(last_month_exists==TRUE) {alt_CPI_url
} else {"ERROR"
}

correct_CPI_url #Prints the URL, shows ERROR if neither exist

download.file(
  url = correct_CPI_url, 
  destfile = "latest_CPI.xls", mode='wb'
)

rm(correct_CPI_url,alt_CPI_url,CPI_url,current_month_exists,last_month_exists)

### CPI sheet import -----
df_latest_CPI<-read_excel("latest_CPI.xls",sheet="Índices IPC Cobertura Nacional") %>% 
  rename(CPI_index_type=1) #Rename the first column

#The date variable is with a wrong format, we correct it here, 
vector_dates<-df_latest_CPI %>% 
  subset(CPI_index_type=="Total nacional") %>%
 t() %>% 
  as.data.frame() %>% 
  rename(date=V1) %>% 
  subset (date!="Total nacional") %>% 
  mutate(date=as.integer(date),
         date=as.Date(date, origin = "1899-12-30"), 
         mergeid=row_number()
         )

### Function to get excel by CPI type ------

updated_CPI_table<-function(indata,CPI_type){
indata<-indata%>% 
  subset(CPI_index_type ==CPI_type) %>% # Change the subset if you want the CPI of another item.
  mutate(CPI_region=ifelse(row_number(CPI_index_type)==1, "Nacional",
                           ifelse(row_number(CPI_index_type)==2, "GBA", 
                                  ifelse(row_number(CPI_index_type)==3, "Pampeana", 
                                         ifelse(row_number(CPI_index_type)==4, "NOA", 
                                                ifelse(row_number(CPI_index_type)==5, "NEA", 
                                                       ifelse(row_number(CPI_index_type)==6, "Cuyo",
                                                              "Patagonia")
                                                )
                                         )
                                  )
                           )
  )
  ) %>% 
  select(c(CPI_region),everything()) %>% #Put the region name as first column
 select(-c(CPI_index_type))

indata<-as.data.frame(t(indata)) %>%
  janitor::row_to_names(row_number=1) %>% 
  mutate(mergeid=row_number())

indata<-vector_dates %>% 
  left_join(indata, by="mergeid") %>% 
  select(-c(mergeid)) %>% 
  mutate(CPI_index_type=CPI_type) %>% 
  select(c(date,CPI_index_type,everything())) #Put date and CPI type first
setwd("../")

if(!file.exists("results_folder")) {
  dir.create("results_folder")
}
setwd("results_folder/")
write.xlsx(indata,paste0("CPI_",CPI_type,".xlsx"))
}

####Getting excel files by CPI types


updated_CPI_table (df_latest_CPI,"Nivel general") #Launch this if you want it only for the general index

##Launch the following to get excel tables for each CPI type
list_wrong_types<-c("Período de referencia: Diciembre 2016=100", "Nivel general y divisiones COICOP","Total nacional",
                    "Bienes y servicios","Categorías","Región Patagonia","Región Cuyo","Región GBA","Región Noreste","Región Noroeste","Región Pampeana")

list_CPI_types<-as.data.frame(table(df_latest_CPI$CPI_index_type)) %>% 
  select(c(Var1)) %>%  
  mutate(Var1=as.character(Var1),
         length_description=nchar(Var1)
         ) %>% 
  subset(length_description<60) %>% 
  subset(!Var1 %in% list_wrong_types) %>%
  select(-c(length_description)) %>% 
  t()

for (i in list_CPI_types) #Launch this to get a distinct excel file, for each CPI item, by region
  {
   updated_CPI_table(df_latest_CPI,i)
}

#Cleanup -----
setwd("../")
unlink("download_folder",recursive=TRUE)
rm(list=ls())