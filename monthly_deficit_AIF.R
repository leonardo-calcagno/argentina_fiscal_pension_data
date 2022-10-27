# This code downloads Argentina's National Public Sector Savings-Investment-Funding Account, 
    #for the years required (possible since 1997); concatenates the bases in a single file; 
    #and outputs a time series of the desired income or expense element, by type of institution
rm(list=ls())
gc()
# Open packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(glue)
library(rlist)
library(XML)
library(RCurl)
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
rm(month,year,year_90,numeric_month,names_xls,names_xls_90,urls_xls,urls_xls_90)

#Import all downloaded excel files --------
#Code taken from https://stackoverflow.com/questions/32888757/how-can-i-read-multiple-excel-files-into-r
#########WARNING:Jan. 2000##############
###The only month that does not work is January 2000, it is a weird xml file. You need to open it with 
#excel, and save it as an excel file in the AIF folder with a different name (2000_enero for instance). 

list_xls <- list.files(pattern='*.xls')
list_xls<-list_xls[!list_xls %in% "2000_01.xls"]
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

##We only want the sheet with the AIF table, which has 10 columns.
n.cols_xls<-unlist(lapply(df_list_xls, function(t) dim(t) [2])) #[1] for rows, [2] for columns
n.cols_s2_xls<-unlist(lapply(df_list_s2_xls, function(t) dim(t) [2]))
n.cols_s3_xls<-unlist(lapply(df_list_s3_xls, function(t) dim(t) [2]))

df_list_xls<-df_list_xls[which(n.cols_xls==10)]
df_list_s2_xls<-df_list_s2_xls[which(n.cols_s2_xls==10)]
df_list_s3_xls<-df_list_s3_xls[which(n.cols_s3_xls==10)]

#Sometimes, the second or third sheet has 10 columns, but is not the sheet with the AIF Table. 
#This causes duplicates, as the sheet with the AIF table is, in that case, correctly in  
#respectively the first and second sheet. 

names_first_sheet<-names(df_list_xls)

df_list_s2_xls<- df_list_s2_xls[- which(names(df_list_s2_xls) %in% names_first_sheet) ]

#For May 2018, the correct sheet is the third one, and the second one has 10 columns. It is the only case, so we correct it manually.
#If rbind fails because the sixth column is double for a given table, it means that table needs to be 
#reloaded manually. 
df_list_s2_xls<-within(df_list_s2_xls, rm("2018_05.xls"))

df_list_s3_xls<- df_list_s3_xls[- which(names(df_list_s3_xls) %in% names_first_sheet) ]
test<-df_list_s2_xls$`2021_junio.xlsx` 
last_column<-length(test)

test<-test %>% 
  select(-c(1,2,last_column)) #The last column is always the total for the whole national administration, so we delete it
list_patterns<-c("TES","AFECT","DESC","INST","CAJAS","OTROS","EMPRESAS")

detect_names<-test[(!grepl("[0-9]", test$...3)) & !is.na(test$...3), ]
temporary_names<-c("var1","var2","var3","var4","var5","var6","var7")
names(detect_names)<-temporary_names

check_pattern<-function(indata,pattern){
  
  
  
  is_name_in_pattern<-indata %>% 
    mutate(test_var1=ifelse(grepl(pattern,var1,ignore.case=TRUE), 1, 
                            0),
           test_var2=ifelse(grepl(pattern,var2,ignore.case=TRUE), 1, 
                            0),
           test_var3=ifelse(grepl(pattern,var3,ignore.case=TRUE), 1, 
                            0),
           test_var4=ifelse(grepl(pattern,var4,ignore.case=TRUE), 1, 
                            0),
           test_var5=ifelse(grepl(pattern,var5,ignore.case=TRUE), 1, 
                            0),
           test_var6=ifelse(grepl(pattern,var6,ignore.case=TRUE), 1, 
                            0),
           test_var7=ifelse(grepl(pattern,var7,ignore.case=TRUE), 1, 
                            0)
          ) %>% 
    summarise(is_var_1=sum(test_var1),
              is_var_2=sum(test_var2),
              is_var_3=sum(test_var3),
              is_var_4=sum(test_var4),
              is_var_5=sum(test_var5),
              is_var_6=sum(test_var6),
              is_var_7=sum(test_var7),
              )
  
  }
is_name_in_pattern<-detect_names [grepl("TES",detect_names$var1,ignore.case=TRUE),]

if(nrow(is_name_in_pattern)==1){
  detect_names<-detect_names %>%
    rename(tesoro_nacional=1)
}else{ detect_names<-detect_names
} #### This conditionally renames columns depending on whether they detect a correct pattern. 
    #This can be the basis of a dedicated function. 


is_name_in_pattern<-is_name_in_pattern[grepl("TES",is_name_in_pattern$1,ignore.case=TRUE),]

  mutate(test_var1=ifelse(grepl("TES",var1,ignore.case=TRUE), 1, 
                          0),
         test_var2=ifelse(grepl("TES",var2,ignore.case=TRUE), 1, 
                          0),
         test_var3=ifelse(grepl("TES",var3,ignore.case=TRUE), 1, 
                          0),
         test_var4=ifelse(grepl("TES",var4,ignore.case=TRUE), 1, 
                          0),
         test_var5=ifelse(grepl("TES",var5,ignore.case=TRUE), 1, 
                          0),
         test_var6=ifelse(grepl("TES",var6,ignore.case=TRUE), 1, 
                          0),
         test_var7=ifelse(grepl("TES",var7,ignore.case=TRUE), 1, 
                          0)
  ) %>% 
  summarise(is_var_1=sum(test_var1),
            is_var_2=sum(test_var2),
            is_var_3=sum(test_var3),
            is_var_4=sum(test_var4),
            is_var_5=sum(test_var5),
            is_var_6=sum(test_var6),
            is_var_7=sum(test_var7),
  )


  
  check_data<-indata[,column_number]
  check_data<-check_data %>% 
    rename(temp_name=1) %>% 
    grepl(pattern,temp_name,ignore.case = TRUE)
    
  print(column_number,patter,check_data)
}

check_data<-detect_names[,1]

check_data<-check_data %>% 
  rename(temp_name=1)
check_data<-check_data %>% 
  mutate(test=ifelse(grepl("TES",temp_name,ignore.case = TRUE), 1, 
                     0)
         ) %>% 
  summarise(is_pattern_name=sum(test))



check_pattern(detect_names,1,"TES")

for (i in list_patterns) #Launch this to get a distinct excel file, for each CPI item, by region
{
  check_pattern(check_col,i)
}



detect_names<-detect_names %>% 
  mutate(across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         across(everything(),~gsub("TESORO","TESORO NACIONAL",.x)),
         
         )

mutate(across(starts_with("alicuota"),~gsub("Exento","0",.x)), #Ponemos en 0 las alícuotas exentas

       detect_names<-test %>% 
  subset((!grepl("[0-9]","...3")))

test<-test %>% 
  rename(es_concepto=2)
test2<-test %>% 
  mutate(test=ifelse(grepl("CONC",es_concepto,fixed=TRUE),1, 
                     0)
         )
table(test2$test)

  grepl("CONCEPTO",fixed=TRUE)
head(test)

names(test)<-c("var1:var11")

test<-lapply(df_list_s2_xls,)
df_AIF <- bind_rows(c(df_list_xls), .id = "file")
df_AIF_s2<-bind_rows (c(df_list_s2_xls), .id="file")
df_AIF_s3<-bind_rows (c(df_list_s3_xls), .id="file")

df_AIF<-df_AIF[,1:11] #We keep only the first 11 columns: the file name and the 10 columns with relevant data.
df_AIF_s2<-df_AIF_s2[,1:11] 
df_AIF_s3<-df_AIF_s3[,1:11] 

df_AIF<-rbind(df_AIF,df_AIF_s2,df_AIF_s3) #We concatenate all AIF table into one dataset
###We check all periods are in the df. It is true for the 1997-Sep. 2022 period. The only missing month
    #is September 2007. 
table(df_AIF$file)

#df_2001_09<-read_excel("2001_09.xls") %>% 
 # mutate(file="2001_09.xls")
#df_AIF<-rbind(df_AIF,df_2001_09)


rm(list=ls(pattern="*df_list"))
rm(list=ls(pattern="*n.cols"))
rm(list_xls,keep_s2_xls,keep_s3_xls,df_AIF_s2,df_AIF_s3,names_first_sheet)

end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken)

#Format AIF dataset ------

df_AIF<-df_AIF %>% 
  mutate(file=gsub(".xlsx","",file),
         file=gsub(".xls","",file),
         year=as.integer(substr(start=1,stop=4,file)), 
         month=substr(start=5,stop=length(file),file), 
         month=gsub("_","",month),
         month=gsub("enero","01",month),  
         month=gsub("febrero","02",month),  
         month=gsub("marzo","03",month),  
         month=gsub("abril","04",month),  
         month=gsub("mayo","05",month),  
         month=gsub("junio","06",month),  
         month=gsub("julio","07",month),  
         month=gsub("agosto","08",month),  
         month=gsub("septiembre","09",month),  
         month=gsub("octubre","10",month),  
         month=gsub("noviembre","11",month),  
         month=gsub("diciembre","12",month) 
  ) %>%
  select(c(file,month,year,everything())) %>% 
  select(-c(4))

list_AIF<-c("concepto","tesoro_nac","recursos_afect","org_desc","ISS","Ex-cajas_prov","total_AN","PAMI_otros","total")
names(df_AIF)<-c("archivo","mes","ano4",list_AIF)
control<-df_AIF %>% 
  subset(!grepl("[0-9]",total)) %>% 
  subset(grepl("[0-9]",total_AN)) #We show there is no relevant information when the "total" variable is blank
head(control)
rm(control)

list_AIF<-list_AIF[list_AIF!="concepto"]
save<-df_AIF
head(save)

df_AIF<-df_AIF %>% 
  subset(grepl("[0-9]",total)) %>%  #We delete lines with no information (no numeric data on the total variable)
  mutate(across(all_of(list_AIF),~as.double(.x) #The remaining lines are converted to numeric
  )) %>% 
  group_by(archivo) %>% 
  mutate(numero_linea=row_number()) %>% 
  ungroup() %>%   #Row number keeps the integrity of each table even when arranged
  select(c("archivo","ano4","mes","numero_linea",everything()))

table_file_size<-df_AIF %>% 
  group_by(archivo) %>% 
  tally() %>% 
  ungroup() %>%  #The most recent monthly AIF lacks financial sources and applications, beware. 
  rename(total_lineas=n)



##This gives us a concatenation of each monthly Savings-Investment-Funding account for Argentina, 
#for the wanted period. Here we are only interested in fiscal income for social security


#Concept names may change; run this to check how the concept you are interested in may be named. 
#Combine with row number for some concepts that may be repeated (such as "transferencias corrientes)
concept_names<- as.data.frame(df_AIF$concepto)
view(concept_names)
#Here, we are only interested in ANSES fiscal income, used for pension mobiliy computation. 

df_ISS_fiscal_income<-df_AIF %>% 
  mutate(is_fiscal_income=ifelse(grepl("*INGRESOS TRIBUTARIOS",concepto), 1, 
                                 0)
  )%>% 
  subset(is_fiscal_income==1 ) %>% 
  mutate(mes_num=as.numeric(mes)) %>% 
  arrange(ano4,mes) %>% 
  select(c(ano4,mes,concepto,ISS))
#Here we check is_fiscal_income captures all rows that correspond to fiscal income, and only 
#those rows.
total_files<-nrow(as.data.frame(table(df_AIF$archivo)))
head(total_files)
table(df_ISS_fiscal_income$concepto) 

#We update ANSES fiscal income information in the global file
vector_ISS_fiscal_income<-df_ISS_fiscal_income %>% 
  subset(ano4>=2018) %>%  #We added manual corrections in 2017 to account for 2017 fiscal amnesty tax income
  select(c(ISS))

range_write(vector_ISS_fiscal_income,ss=id_globals,range="I284",col_names =FALSE,sheet="Pessimistic projection",reformat=FALSE) #

rm(total_files,control,list_AIF,table_file_size,concept_names,vector_ISS_fiscal_income,df_ISS_fiscal_income)

end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken)
#On 4 GB Ram laptop, 7.6 minutes. 
#Cleanup -----
rm(output_name,sheet_name)
setwd("C:/Users/lcalcagno/Documents/Investigación/MISSAR_private/R_files_for_MISSAR/")
unlink("download_folder",recursive=TRUE)
rm(list=ls())
