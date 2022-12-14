# This code uploads all Argentina's National Public Sector Savings-Investment-Funding Account, 
    #downloaded from download_all_AIF; concatenates the bases in a single file; 
    #and outputs a time series of the desired income or expense element, by type of institution
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
#Import all downloaded excel files --------
#Code taken from https://stackoverflow.com/questions/32888757/how-can-i-read-multiple-excel-files-into-r
#########WARNING:Jan. 2000##############
###The only month that does not work is January 2000, it is a weird xml file. You need to open it with 
#excel, and save it as an excel file in the AIF folder with a different name (2000_enero for instance). 

start.time=Sys.time()
list_xls <- list.files(pattern='*.xls')
list_xls<-list_xls[!list_xls %in% "2000_01.xls"]

####If you want to run this for only some years, run the following function:
load_some_years<-function(indata,min_year,max_year){
df_list_xls <- as.data.frame(list.files(pattern='*.xls')) %>%
  rename(file_name=1) %>% 
  mutate(year=as.integer(substr(start=1,stop=4,file_name))) %>% 
  subset(year>=min_year & year<=max_year) %>%  #The comparative sheet is only available starting from January 2017
  select(c(file_name)) %>% 
  t()

list_xls<-as.character(df_list_xls)
}
#list_xls<-load_some_years(2017,2100) Example run: all periods from 2017 onward.

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

end.time=Sys.time()
time.taken=end.time-start.time
head(time.taken) #10 minutes to load all dataframes in a 8GB RAM laptop

###Detect AIF columns, even when in disorder------

n.cols_xls<-unlist(lapply(df_list_xls, function(t) dim(t) [2])) #[1] for rows, [2] for columns
n.rows_xls<-unlist(lapply(df_list_xls, function(t) dim(t) [1])) #[1] for rows, [2] for columns
view(n.rows_xls)
#The fourth quarter of 2015 datasets have too many rows, we take it into account in the get_col_names() function

track_index<-0
##We detect AIF columns with the get_col_names() function
get_col_names<-function(indata){
track_index<<-track_index+1
print(track_index)
#Fourth quarter of 2015 dataframe have too many rows, which causes bugs: we take only the first 
      #160 rows 
if(nrow(indata)>160){
indata<-indata[1:160,]  
}else{
  indata<-indata
}
  last_column<-length(indata)
#The last column is always the total for the whole national administration; 
    #The first column is always useless, and the second has the row name 
row_name<-indata %>% 
  select(c(1,2))
names(row_name)<-c("sec_hacienda","concepto") 
row_name<-row_name %>% 
  mutate(concepto=ifelse(is.na(concepto), sec_hacienda, 
                         concepto)#Sometimes the row name ends up in the first, instead of second, column
         ) %>% 
  select(c(concepto))

total_APN<-indata %>% 
  select(c(all_of(last_column))
         )
names(total_APN)<-c("total")

indata<-indata %>% 
  select(-c(1,2,all_of(last_column)))
    
list_patterns<-c("TES","AFECT","DESC","INST","CAJAS","TOTAL","OTROS|EMPRESAS")
detect_names<-indata[(!grepl("[0-9]", indata$...3)) & !is.na(indata$...3), ] #Only keep rows with characters
correct_names<-c("tesoro_nac","recursos_afect","org_desc","ISS","Ex_cajas_prov","total_AN","PAMI_otros")
#head(detect_names)
#detect_names<-detect_names %>% 
 # select(c("...5",everything())) #We check this function works with out-of-order datasets

for(i in 1:7) {
  for (j in 1:7){
  #This keeps only rows where the ith pattern is present in the jth column; there, we rename the  
      #jth column following the ith correct name
  #First we keep rows where the ith pattern is present in the jth column  
  is_name_in_pattern<-detect_names [grepl(list_patterns[i],detect_names[[j]],ignore.case=TRUE),]
  #Second, we keep only the column where the ith pattern is present
  is_name_in_pattern<-is_name_in_pattern[,grepl(list_patterns[i],detect_names,ignore.case=TRUE)]
  is_name_in_pattern<-as.data.frame(is_name_in_pattern)
#  print(is_name_in_pattern)
  
  #When the ith pattern is present in the jth column, we get a 1x1 df 
  newcol<-correct_names[i]
  oldcol<-colnames(detect_names)[j]
 # print(newcol)
  #print(oldcol)
  
  if((nrow(is_name_in_pattern)>=1 & nrow(is_name_in_pattern)<=2 ) & ncol(is_name_in_pattern)==1){ #So if we have a 1x1 or 1x2 df
    detect_names<-detect_names %>%
      rename(!!newcol := !!oldcol) #We rename the jth column with the ith correct column name
  }else{ detect_names<-detect_names
  } 
              }
           }
names(indata)<-names(detect_names)
outdata<-as.data.frame(c(row_name,indata,total_APN))
}
#We now apply this function to all the data frames
options(error = NULL)
track_index<-0
df_list_xls<-sapply(df_list_xls,get_col_names,simplify=FALSE)
#debug<-df_list_xls[85], for instance, gives the 85th element of df_list_xls
track_index<-0
df_list_s2_xls<-sapply(df_list_s2_xls,get_col_names,simplify=FALSE)
track_index<-0
df_list_s3_xls<-sapply(df_list_s3_xls,get_col_names,simplify=FALSE)

#It is now safe to use bind_rows on all lists of data frames
df_AIF <- bind_rows(c(df_list_xls), .id = "file")
df_AIF_s2<-bind_rows (c(df_list_s2_xls), .id="file")
df_AIF_s3<-bind_rows (c(df_list_s3_xls), .id="file")

df_AIF<-rbind(df_AIF,df_AIF_s2,df_AIF_s3) #We concatenate all AIF table into one dataset
###We check all periods are in the df. It is true for the 1997-Sep. 2022 period. The only missing month
    #is September 2007. 
table(df_AIF$file)

rm(list=ls(pattern="*df_list"))
rm(list=ls(pattern="*n.cols"))
rm(list_xls,keep_s2_xls,keep_s3_xls,df_AIF_s2,df_AIF_s3,names_first_sheet,track_index)

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
  select(c(file,month,year,everything())) 

control<-df_AIF %>% 
  subset(!grepl("[0-9]",total)) %>% 
  subset(grepl("[0-9]",total_AN)) #We show there is no relevant information when the "total" variable is blank
head(control)
rm(control)

####Control: colummns have correct names-----
list_AIF<-c("concepto","tesoro_nac","recursos_afect","org_desc","ISS","Ex_cajas_prov","total_AN","PAMI_otros","total")
list_patterns<-c("TES","AFECT","DESC","INST","CAJAS","TOTAL","OTROS|EMPRESAS")
list_control<-list_AIF[list_AIF!="concepto" & list_AIF!="total"]
list_control_subset<-append(list_control,"file")

control<-df_AIF %>% 
  select(c(all_of(list_control_subset))) %>% 
  mutate(across(all_of(list_control),~gsub("\\d+",NA,.x)) #We delete all numbers
        ) 
control<-control[rowSums(is.na(control)) != ncol(control)-1,] #We delete all rows where only the file name remains


df_loop<-as.data.frame(cbind(list_control,list_patterns))
control<-control %>% 
  mutate(has_error=0)

for (i in 1:7){
  incorrect_patterns<-list_patterns[list_patterns!=df_loop[i,2]] #We check for all the wrong patterns for the column
  print(df_loop[i,2])
  incorrect_patterns<-paste(incorrect_patterns,collapse="|")
  print(incorrect_patterns)
  control<-control %>% 
    mutate(has_error=ifelse(grepl(incorrect_patterns,df_loop[i,1],ignore.case=TRUE),1, 
                         has_error)
          )
  print(df_loop[i,1])
}

errors<-control %>% 
  subset(has_error==1)
head(errors) #Shows files with errors in column names. 
rm(control,errors,incorrect_patterns,list_patterns,df_loop)


###Get numeric data -----
df_AIF<-df_AIF %>% 
  subset(grepl("[0-9]",total)) %>%  #We delete lines with no information (no numeric data on the total variable)
  mutate(across(all_of(list_AIF[list_AIF!="concepto"]),~as.double(.x) #The remaining lines are converted to numeric
  )) %>% 
  group_by(file) %>% 
  mutate(table_line=row_number()) %>% 
  ungroup() %>%   #Row number keeps the integrity of each table even when arranged
  select(c("file","year","month","table_line",everything()))

table_file_size<-df_AIF %>% 
  group_by(file) %>% 
  tally() %>% 
  ungroup() %>%  #The most recent monthly AIF often lacks financial sources and applications, beware. 
  rename(table_line=n)



##This gives us a concatenation of each monthly Savings-Investment-Funding account for Argentina, 
#for the wanted period. Here we are only interested in fiscal income for social security


#Concept names may change; run this to check how the concept you are interested in may be named. 
#Combine with row number for some concepts that may be repeated (such as "transferencias corrientes)

missing_concept<-df_AIF %>% 
  subset(is.na(concepto)) #Verify if the concepto column is missing
head(missing_concept)
df_AIF<-df_AIF %>% 
  subset(!is.na(concepto))

concept_names<- as.data.frame(df_AIF$concepto) %>% 
  distinct()
view(concept_names)

#Results----
#Here, we are only interested in ANSES fiscal income, used for pension mobiliy computation. 

df_ISS_fiscal_income<-df_AIF %>% 
  mutate(is_fiscal_income=ifelse(grepl("*INGRESOS TRIBUTARIOS",concepto), 1, 
                                 0)
  )%>% 
  subset(is_fiscal_income==1 ) %>% 
  mutate(month_num=as.numeric(month)) %>% 
  arrange(year,month) %>% 
  select(c(year,month,concepto,ISS))
#Here we check is_fiscal_income captures all rows that correspond to fiscal income, and only 
#those rows.
total_files<-nrow(as.data.frame(table(df_AIF$file)))
head(total_files)
table(df_ISS_fiscal_income$concepto) 

#We also want to see the surplus and operative deficit of public firms. 


df_public_firms_deficit<-df_AIF %>% 
  mutate(is_public_firm_deficit=ifelse(grepl("*DEFICIT OPER",concepto) | grepl("DÉFICIT OPER",concepto), 1, 
                                       0)
  )%>% 
  subset(is_public_firm_deficit==1 ) %>% 
  mutate(month_num=as.numeric(month)) %>% 
  arrange(year,month) %>% 
  select(c(year,month,PAMI_otros))%>% 
  rename(deficit=PAMI_otros)

df_public_firms_surplus<-df_AIF %>% 
  mutate(is_public_firm_surplus=ifelse(grepl("*SUPERAVIT OPER",concepto) | grepl("SUPERÁVIT OPER",concepto), 1, 
                                0)
        ) %>%  
  subset(is_public_firm_surplus==1  ) %>% 
  mutate(month_num=as.numeric(month)) %>% 
  arrange(year,month) %>% 
  select(c(year,month,PAMI_otros)) %>% 
  rename(surplus=PAMI_otros)

df_public_firms_result<-df_public_firms_surplus %>% 
  left_join(df_public_firms_deficit) %>% 
  mutate(public_firms_result=surplus-deficit)
view(df_public_firms_result)

###Export results----
setwd("../")

if(!file.exists("results_folder")) {
  dir.create("results_folder")
}

setwd("results_folder/")
getwd()


write.xlsx(df_AIF,"all_AIF_97_22.xlsx")

write.xlsx(df_public_firms_result,"public_firms_result_97_22.xlsx")

write.xlsx(df_ISS_fiscal_income,"ISS_fiscal_income_97_22.xlsx")


rm(df_public_firms_deficit,df_public_firms_surplus)
rm(total_files,control,list_AIF,table_file_size,concept_names,vector_ISS_fiscal_income,df_ISS_fiscal_income)


#On 4 GB Ram laptop, 7.6 minutes. 
#Cleanup -----
rm(output_name,sheet_name)
setwd("C:/Users/lcalcagno/Documents/iĂłn/MISSAR_private/R_files_for_MISSAR/")
unlink("download_folder",recursive=TRUE)
rm(list=ls())
