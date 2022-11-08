#This code uploads all monthly social security bulletins by AFIP, downloaded by download_all_soc_sec_bulletin; 
    #It reads only the sheet that lays out social security contributions distributed by organism. 
    #It currently outputs a monthly time series for social security transferred to ANSES from May 2003 (first available social 
    #security bulletin)
#To study other bodies, replace the "*Anses" pattern by the relevant body's name. 
#To study other parts of the social security bulletin, change the imported sheet. 

library(tidyverse)
library(readxl)
library(openxlsx)
library(glue)
library(rlist)
library(xml2)
library(rvest)

# Social security contributions----
#Set the working directory to the folder with the downloaded monthly social security bulletin excel files

#On 4 GB Ram laptop, 2 minutes. 
start.time=Sys.time()
setwd("C:/Users/lcalcagno/Documents/Investigación/argentina_fiscal_pension_data")
setwd("bol_men_ss/")
getwd()

#List all social-security bulletin excel files
list_xls<-list.files(pattern='*.xls')


###Import all downloaded excel files (from https://stackoverflow.com/questions/32888757/how-can-i-read-multiple-excel-files-into-r)

#Social security contributions for ANSES are always on sheet "Cuadro 9" from May 2003 to November 2008; 
#after that date, they are always on sheet "Cuadro 8". We thus identify for which df we import sheet "Cuadro 9", 
#and for which others we import sheet "Cuadro 8". 

df_list_xls <- as.data.frame(list.files(pattern='*.xls')) %>%
  rename(file_name=1) %>% 
  mutate(title_length=str_count(file_name), #File names have either 2 or 4 digit years, we identify them
         year=ifelse(title_length!=9, as.integer(substr(start=1,stop=4,file_name)), #4 digit years
                     as.integer(paste0("20",substr(start=1,stop=2,file_name)) #2 digit years
                     )
         ), 
         alt_name=gsub("[^0-9_]","",file_name), #deletes everything except _ and numbers
         month=as.integer(substr(start=str_count(alt_name)-1,stop=str_count(alt_name),alt_name))
  )

list_xls_pre_2008<-df_list_xls %>% 
  subset(year<=2007 | (year==2008 & month!=12)) %>%  #For all datasets up to November 2008
  #You can restrict imported years or months with another subset() instruction
  select(c(file_name)) %>% 
  t() %>% 
  as.character()


list_xls_2008_2016<-df_list_xls %>% 
  subset((year<2017 & year>=2009) | (year==2008 & month==12)) %>%  #For all datasets since December 2008
  #You can restrict imported years or months with another subset() instruction
  select(c(file_name)) %>% 
  t() %>% 
  as.character()

list_xls_post_2017<-df_list_xls %>% 
  subset(year>=2017) %>%  #For all datasets since December 2008
  #You can restrict imported years or months with another subset() instruction
  select(c(file_name)) %>% 
  t() %>% 
  as.character()

read_cuadro_8<-function(path){
  nm_8 <- try(grep("Cuadro 8", excel_sheets(path), 
                   ignore.case = TRUE, value = TRUE)
  )
  x <- try(read_excel(path=path, sheet = nm_8,skip=9))#No relevant information, and sometimes causes bugs
  
}

read_cuadro_9<-function(path){
  
  nm_9 <- try(grep("Cuadro 9", excel_sheets(path), 
                   ignore.case = TRUE, value = TRUE)
  )
  x <- try(read_excel(path=path, sheet = nm_9))
  x<-x %>% 
    filter(if_any(everything(), ~ grepl("*Anses", .x,ignore.case=TRUE))) %>%  #This keeps rows where the "Anses" pattern appears at least once
    rename(destino=3,
           monto=4) %>% 
    mutate(monto=as.double(monto)) %>% 
    select(c(destino,monto)) #Keeps only the destination and total pesos transferred
}

rm(list_xls,df_list_xls)
##Pre-2017 files----
gc()
df_list_pre_2008<-sapply(list_xls_pre_2008,read_cuadro_9,simplify=FALSE)#This keeps the file names 
gc()
df_list_2008_2016<-sapply(list_xls_2008_2016,read_cuadro_8,simplify=FALSE)#This keeps the file names 

format_cuadro_8_monthly<-function(x){
  x<-x %>% 
    filter(if_any(everything(), ~ grepl("*Anses", .x,ignore.case=TRUE))) %>%  #This keeps rows where the "Anses" pattern appears at least once
    rename(destino=3,
           monto=4) %>% 
    mutate(monto=as.double(monto)) %>% 
    select(c(destino,monto)) #Keeps only the destination and total pesos transferred
}

df_list_2008_2016<-sapply(df_list_2008_2016,format_cuadro_8_monthly,simplify=FALSE)
gc()

df_2003_2016<- bind_rows(c(df_list_pre_2008,df_list_2008_2016), .id = "file_name") %>% 
  mutate(title_length=str_count(file_name), #File names have either 2 or 4 digit years, we identify them
         year=ifelse(title_length!=9, as.integer(substr(start=1,stop=4,file_name)), #4 digit years
                     as.integer(paste0("20",substr(start=1,stop=2,file_name)) #2 digit years
                     )
         ), 
         alt_name=gsub("[^0-9_]","",file_name), #deletes everything except _ and numbers
         month=as.integer(substr(start=str_count(alt_name)-1,stop=str_count(alt_name),alt_name))
  ) %>% 
  select(c(year,month,monto)) %>% 
  mutate(monto=ifelse(year==2016 & month==12, monto*1000, #December 2016, the imported data is in millions, not in thousands
                      monto),
         monto=monto/1000 #Puts transfers in millions of pesos
  )
head(df_2003_2016)
gc()

##Post- 2017 files----
df_list_post_2017<-sapply(list_xls_post_2017,read_cuadro_8,simplify=FALSE)


format_cuadro_8_yearly<-function(x){
  x<-x %>% 
    filter(if_any(everything(), ~ grepl("*Anses", .x,ignore.case=TRUE)))     #This keeps rows where the "Anses" pattern appears at least once
  
  column_names<-as.data.frame(names(x)) %>% 
    rename(month=1) %>% 
    mutate(idmerge=row_number())
  
  x<-x %>% 
    t() %>% 
    as.data.frame()%>% 
    rename(monto=1) %>% 
    mutate(idmerge=row_number()) %>% 
    left_join(column_names) %>% 
    select(-c(idmerge))
  
  x<-x[(grepl("[0-9]", x$monto)),]#Keeps only lines with numeric data
  x<-x %>% 
    mutate(numeric_month=row_number(), #Puts numeric months, for compatibility with pre-2017 datasets
           monto=as.double(monto)
    )   
}

df_list_post_2017<-sapply(df_list_post_2017,format_cuadro_8_yearly,simplify=FALSE)

df_post_2017<-bind_rows(c(df_list_post_2017), .id="file_name") %>% 
  mutate(year=as.integer(substr(start=1,stop=4,file_name))) %>% 
  select(c(year,month,numeric_month,monto))

#table(df_post_2017$numeric_month,df_post_2017$month) #run this to verify the numeric months are correct
df_post_2017<-df_post_2017 %>% 
  select(-c(month)) %>% 
  rename(month=numeric_month)

#Time series of monthly ANSES contributions, in millions of current pesos
df_ANSES_contributions<-rbind(df_2003_2016,df_post_2017) 

##Export results----
setwd("../")

if(!file.exists("results_folder")) {
  dir.create("results_folder")
}

setwd("results_folder/")
getwd()


write.xlsx(df_ANSES_contributions,"ANSES_contributions_03_22.xlsx")

rm(df_2003_2016,df_post_2017,
   format_cuadro_8_monthly,format_cuadro_8_yearly,read_cuadro_8,read_cuadro_9)
rm(list=ls(pattern="*df_list"))
rm(list=ls(pattern="*list_"))

end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken)
