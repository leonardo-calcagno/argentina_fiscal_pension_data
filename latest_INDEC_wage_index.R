
###This code downloads INDEC's latest wage index, and outputs an excel file with 
    #only wage indexes. 

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

####Find the URL ------
#URL: https://www.indec.gob.ar/ftp/cuadros/sociedad/variaciones_salarios_09_22.xls for July data

urlFileExist <- function(url){ #Source: https://stackoverflow.com/questions/60318926/how-to-check-if-file-exists-in-the-url-before-use-download-file-in-r
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}

#We automate the URL for downloading the latest INDEC wage index: for figures of a given month (say, January), the URL is 
#named two months after  (here, March).
date<-Sys.Date()
year<-substr(date,start=3,stop=4)
month<-substr(date,start=6,stop=7)
month<-as.integer(month)-1
month<- if(month==0){12
}else {month
}


#day<-substr(date,start=9,stop=10)
lag_month<-month-1
lag_month<- if(lag_month==0){12
}else {lag_month
}

month<- if (month>9) {as.character(month)
} else {
  paste0("0",month)
}
lag_year<-as.integer(year)-1
lag_year<- as.character(lag_year)

lag_month<- if (lag_month>9) {as.character(lag_month)
} else {
  paste0("0",lag_month)
}


wage_url <- paste0("https://www.indec.gob.ar/ftp/cuadros/sociedad/variaciones_salarios_",month,"_",year,".xls")

alt_wage_url<- if (month=="01") {paste0("https://www.indec.gob.ar/ftp/cuadros/sociedad/variaciones_salarios_",lag_month,"_",lag_year,".xls")
} else {paste0("https://www.indec.gob.ar/ftp/cuadros/sociedad/variaciones_salarios_",lag_month,"_",year,".xls")
}
rm(date,year,month,day,lag_year,lag_month)


current_month_exists<-urlFileExist(wage_url)$exists
last_month_exists<-urlFileExist(alt_wage_url)$exists

correct_wage_url<- if(current_month_exists==TRUE){wage_url
}else if(last_month_exists==TRUE) {alt_wage_url
} else {"ERROR"
}

correct_wage_url #Prints the URL, shows ERROR if neither exist

download.file(
  url = correct_wage_url, 
  destfile = "latest_INDEC_wage.xls", mode='wb'
)


rm(correct_wage_url,alt_wage_url,wage_url,current_month_exists,last_month_exists)

###Format and export the wage index dataset ------
df_latest_wage<-read_excel("latest_INDEC_wage.xls")
df_latest_wage<-df_latest_wage[,c(1,2,4,6,8,10,12)] #Keep only indexes
names(df_latest_wage)<-c("ano4","mes","privado_registrado","publico","total_registrado","no_registrado","indice_salarios")

df_latest_wage<-df_latest_wage %>% 
  subset(!is.na(mes)) %>% 
  fill(ano4,.direction="down") %>%  #Fill the year variable where missing
  mutate(across(!starts_with("mes"),~as.double(.x)))

#Export results
setwd("../")
if(!file.exists("results_folder")) {
  dir.create("results_folder")
}
setwd("results_folder/")
write.xlsx(df_latest_wage,"INDEC_wage_index.xlsx")


#Cleanup -----
setwd("../")
unlink("download_folder",recursive=TRUE)
rm(list=ls())