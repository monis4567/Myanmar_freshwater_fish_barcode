#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

########################################################################################
# R-code for analysing BLAST xml output and compare with 
# checklist of fishes and with PCR setups

########################################################################################
# >version
# _                           
# platform       x86_64-apple-darwin13.4.0   
# arch           x86_64                      
# os             darwin13.4.0                
# system         x86_64, darwin13.4.0        
# status                                     
# major          3                           
# minor          3.3                         
# year           2017                        
# month          03                          
# day            06                          
# svn rev        72310                       
# language       R                           
# version.string R version 3.3.3 (2017-03-06)
# nickname       Another Canoe  
########################################################################################
#remove everything in the working environment, without a warning!!
rm(list=ls())
########################################################################################

#define input directory
#wd00 <- "/Users/steenknudsen/Documents/Documents/NIVA_Ansaettelse_2021/Mathias_fish_Myanmar_barcode/R_analyse_af_seq"
#wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/Mathias_fish_Myanmar_barcode/R_analyse_af_seq"
#setwd(wd00)
#install read XML package
if(!require(XML)){
  install.packages("XML")
  library(XML)
}
library("XML")
library("methods")
library(xml2)
#define input file
inputxml01 <- "5HPXH1SG013-Alignment.xml"
inputxml01 <- "6S0VKT1D01R-Alignment.xml"
inputxml01 <- "B1WE022J01R-Alignment.xml"
inputxml01 <- "bilag02_blast_results.xml"
#inputxml01 <-"695W3V9Y013-Alignment.xml"
result <- xmlParse(file = inputxml01)
#define input file with PCRs
input02 <- "bilag03_list_ofPCRs_onMyanmarfish01.csv"
df_ep01 <- read.csv(input02)

#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
df_ep01$Tubeno_wZe <-stringr::str_pad(df_ep01$Tubeno, 2, pad = "0")
#paste together columns
df_ep01$PCRnoTb <- paste(df_ep01$PCRno,"_",df_ep01$Tubeno_wZe,sep="")

#define input file with species
input03 <- "bilag04_List_of_freshwater_fishes_from_Myanmar_2020okt.xls"
tibl_ep02 <- readxl::read_excel(input03)
df_ep02 <- as.data.frame(tibl_ep02)

##paste columns together
df_ep02$gns_spsnm <- paste(df_ep02$Genus,"_",df_ep02$Species,sep="")
#make a copy of the dataframe with a new name
df_FisBs <-df_ep02
#define input file with species
input04 <- "bilag05_lst_spc_Psomadakis_etal_FAO_guide_Myanmar.txt"
df_ep04 <- read.csv(input04,sep = "\t",header = F)
colnames(df_ep04) <-c("genus_species","pg_no")
#substitute to get genus name
df_ep04$genusnmn <- gsub("(^.*) .*$","\\1",df_ep04$genus_species)
df_ep04$specinmn <- gsub("(^.*) (.*$)","\\2",df_ep04$genus_species)
#paste columns together
df_ep04$gns_spsnm <- paste(df_ep04$genusnmn,"_",df_ep04$specinmn,sep="")
#make a copy of the dataframe with a new name
df_Pomad <- df_ep04

library(XML)
library(dplyr)

xmlfile<-inputxml01

xmltext <- readr::read_file(xmlfile)
xmltext <- gsub("CREATE_VIEW","",xmltext)
xmltext <- gsub("\\&quot;","",xmltext)
xmltext <- gsub("\\&gt;","",xmltext)
xmltext <- gsub("\\&lt;","",xmltext)
xmltext <- gsub("\\&apos;","",xmltext)

doc = xmlTreeParse(xmltext, useInternal=TRUE, asText=T)
ls<-xmlToList(doc)

iterations <- ls$BlastOutput_iterations
#
length(iterations[6]$Iteration$Iteration_hits)
length(iterations[7]$Iteration$Iteration_hits)
#make an empty data frame
df <- data.frame(iter_num=integer(),
                 query_ID=character(),
                 query_def=character(),
                 query_len=character(),
                 Hit_num=integer(),
                 Hit_id=character(),
                 Hit_def=character(),
                 Hit_len=integer(),
                 Hsp_align_len=character(),
                 Hsp_qseq=character(),
                 Hsp_hseq=character(),
                 Hsp_midline=character(),
                 
                 Hsp_bit_score=integer(),
                 Hsp_score=integer(),
                 Hsp_identity=integer(),
                 # if stringsAsFactorsis not set to F
                 #it will generate empty NA columns for non factor elements
                 stringsAsFactors = F
)
#i <- 7
for(i in 1:length(iterations)){
  # for(i in 1:1){
  #   print(i)
  # }
  #get the number of hits in the query
  niH <- length(iterations[i]$Iteration$Iteration_hits)
  #check if there are more than 2 hits
  if (!niH>2){print("not1")} else {
  iNum<-as.numeric(iterations[i]$Iteration$`Iteration_iter-num`)
  iQueryID<-iterations[i]$Iteration$`Iteration_query-ID`
  iQueryDef<-iterations[i]$Iteration$`Iteration_query-def`
  iQueryLen<-iterations[i]$Iteration$`Iteration_query-len`
  hits <- iterations[i]$Iteration$Iteration_hits
  #iterations[i]$Iteration$Iteration_hits$Hit
  #hits <- iterations[i]$Iteration$Iteration_hits$Hit$Hit_num
  for(j in 1:length(hits)){
    #j <- 2
    #for(j in 1:1){
    irow <-nrow(df)+1
    df[irow,] <- NA
    df$iter_num[irow] <- iNum
    df$query_ID[irow] <- iQueryID
    df$query_def[irow] <- iQueryDef
    df$query_len[irow] <- iQueryLen
    df$Hit_num[irow] <- as.numeric(hits[j]$Hit$Hit_num)
    df$Hit_id[irow] <- hits[j]$Hit$Hit_id
    df$Hit_def[irow] <- hits[j]$Hit$Hit_def
    df$Hit_len[irow] <- as.numeric(hits[j]$Hit$Hit_len)
    df$Hsp_align_len[irow] <- hits[j]$Hit$Hit_hsps$Hsp$`Hsp_align-len`
    df$Hsp_qseq[irow] <- hits[j]$Hit$Hit_hsps$Hsp$Hsp_qseq
    df$Hsp_hseq[irow] <- hits[j]$Hit$Hit_hsps$Hsp$Hsp_hseq
    df$Hsp_midline[irow] <- hits[j]$Hit$Hit_hsps$Hsp$Hsp_midline
    
    df$Hsp_score[irow] <- as.numeric(hits[j]$Hit$Hit_hsps$Hsp$Hsp_score)
    df$Hsp_identity[irow] <- as.numeric(hits[j]$Hit$Hit_hsps$Hsp$Hsp_identity)
    df$Hsp_bit_score[irow] <- as.numeric(hits[j]$Hit$Hit_hsps$Hsp$`Hsp_bit-score`) 
    }
  }
}

#copy the data frame
df_xml01 <- df
# split string by space and get first and second element
Hit_def2 <- data.frame(do.call('rbind', strsplit(as.character(df_xml01$Hit_def),' ',fixed=TRUE)))
df_xml01$Hit_genus <- as.character(Hit_def2$X1)
df_xml01$Hit_species <- as.character(Hit_def2$X2)
#paste columns together
df_xml01$Hit_genus_species <- paste(df_xml01$Hit_genus,df_xml01$Hit_species,sep="_")
#get accession number
Hit_id2 <- data.frame(do.call('rbind', strsplit(as.character(df_xml01$Hit_id),'|',fixed=TRUE)))
df_xml01$NCBIAccsno <- as.character(Hit_id2$X4)

#match between data frames
#see if there is a direct match with both genus and species in the Psomadakis_etal_FAO guide
df_xml01$Psomadakis_etal_FAO_pgno_gs <- df_Pomad$pg_no[match(df_xml01$Hit_genus_species, df_Pomad$gns_spsnm)]
#see if there is a direct match with genus in the Psomadakis_etal_FAO guide
df_xml01$Psomadakis_etal_FAO_pgno_g <- df_Pomad$pg_no[match(df_xml01$Hit_genus, df_Pomad$genusnmn)]
#see if there is a direct match with genus and species in FishBase list
df_xml01$FshBsMF_gs <-  df_FisBs$Marine_or_freshwater_M_F[match(df_xml01$Hit_genus_species, df_FisBs$gns_spsnm)]
#see if there is a direct match with genus in FishBase list
df_xml01$FshBsMF_g <-  df_FisBs$Marine_or_freshwater_M_F[match(df_xml01$Hit_genus, df_FisBs$Genus)]
#match to fish specimen number
df_xml01$FshSpcNo <- df_ep01$fishspecimennumber[match(df_xml01$query_def,df_ep01$PCRnoTb)]
# Sort by vector name [query_def] then [Hsp_bit_score]
df_xml02 <- df_xml01[
  with(df_xml01, order(query_def, Hsp_bit_score)),
  ]
#colnames(df_xml02)
#website on how to interpret blast results
#https://medium.com/computational-biology/how-to-interpret-blast-results-ee304216fd5
#define which column names you need for a list
keeps <- c("query_def",
           "Hsp_bit_score",
           "Hsp_score",
           "Hsp_identity",
           "Hit_genus",
           "Hit_species",
           "Hit_genus_species",
           "Psomadakis_etal_FAO_pgno_gs",
           "Psomadakis_etal_FAO_pgno_g",
           "FshBsMF_gs",
           "FshBsMF_g",
           "FshSpcNo",
           "NCBIAccsno")
#only keep selected columns from list
df_xml03 <- df_xml02[keeps]
# keep unique rows based on two columns # https://www.tutorialspoint.com/how-to-find-the-unique-rows-based-on-some-columns-in-r
df_xml04 <- df_xml03[row.names(unique(df_xml03[,c("query_def", "Hit_genus_species")])),]

#unique(df_xml04$query_def)
library(tableHTML)
#see it as an html table
# see : https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
#tableHTML(df_xml04)
# see it as a html table
df_xml04 %>%
  tableHTML 
#https://community.rstudio.com/t/how-to-select-top-n-highest-value-and-create-new-column-with-it/38914
#https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
#load the dplyr package
library(dplyr)
#use the dplyr to group by 'query_def' and then filter inside each group
# and find rows that are not NAs in 'FshBsMF_gs'
#Because the 'FshBsMF_gs' denotes if there is a direct hit with the species
# checklist, then this equals a valid hit with a species
tibl_dgs_Hits <- df_xml04 %>% 
  dplyr::group_by(query_def) %>%
  dplyr::filter(!is.na(FshBsMF_gs))
#make the tibble a data frame
df_dgs_Hits <-as.data.frame(tibl_dgs_Hits)
#identify which rows with 'query_def' are duplicated
# i.e. which 'query_def' that still needs a specific species hit
miss_query_def1 <- unique(df_dgs_Hits$query_def[duplicated(df_dgs_Hits$query_def)])
#Extract Rows from Data Frame According to Vector
#https://data-hacks.com/extract-rows-from-data-frame-according-vector-r
# get those rows that are not missing and ID
df_egs_Hits <- df_dgs_Hits[!df_dgs_Hits$query_def %in% miss_query_def1, ]
#append result back to data frame
df_xml04$BestMatch1 <- df_egs_Hits$Hit_genus_species[match(df_xml04$query_def,df_egs_Hits$query_def)]
#get the 'query_def' that are not missing an ID
not_missID1 <- unique(df_egs_Hits$query_def)
#exclude these already IDed
df_xml05 <- df_xml04[!df_xml04$query_def %in% not_missID1, ]
#among those not having been assigned a direct gs ID,
# try and see if there are multiple gs matches
# and among those then select the best Bit score match
tibl_xml_drctgen_Hits <- df_xml05 %>% 
  dplyr::group_by(query_def) %>%
  dplyr::filter(!is.na(FshBsMF_g)) %>%
  dplyr::filter(!is.na(FshBsMF_gs)) %>%
  #select among the highest Bit Score
  dplyr::filter(Hsp_bit_score == max(Hsp_bit_score))
#make the tibble a data frame
df_gsHBS_Hits <-as.data.frame(tibl_xml_drctgen_Hits)
#append result back to data frame
df_xml04$BestMatch2 <- df_gsHBS_Hits$Hit_genus_species[match(df_xml04$query_def,df_gsHBS_Hits$query_def)]

#identify which rows with 'query_def' are duplicated
# i.e. which 'query_def' that still needs a specific species hit
miss_query_def2 <- unique(df_gsHBS_Hits$query_def[duplicated(df_gsHBS_Hits$query_def)])

# get those rows that are not missing and ID
df_xml_exct_g_Hits <- df_gsHBS_Hits[!df_gsHBS_Hits$query_def %in% miss_query_def2, ]
#get the 'query_def' that are not missing an ID
not_missID2 <- unique(df_xml_exct_g_Hits$query_def)
#add up these two vectors, as there are IDs available now
not_missID3 <- c(not_missID1,not_missID2)
#exclude these already IDed
df_xml06 <- df_xml04[!df_xml04$query_def %in% not_missID3, ]
#among those not having been assigned a direct gs ID,
# try and see if a genus ID can be found
# and among those then select the best Bit score match
tibl_dg_Ht2 <- df_xml06 %>% 
  dplyr::group_by(query_def) %>%
  dplyr::filter(!is.na(FshBsMF_g)) %>%
  #select among the highest Bit Score
  dplyr::filter(Hsp_bit_score == max(Hsp_bit_score))
#make the tibble a data frame
df_dg_Ht2 <-as.data.frame(tibl_dg_Ht2)
#append result back to data frame
df_xml04$BestMatch3 <- df_dg_Ht2$Hit_genus[match(df_xml04$query_def,df_dg_Ht2$query_def)]
#identify which rows with 'query_def' are duplicated
# i.e. which 'query_def' that still needs a specific species hit
miss_query_def3 <- unique(df_dg_Ht2$query_def[duplicated(df_dg_Ht2$query_def)])
# get those rows that are not missing and ID
df_gHBS_Hits <- df_dg_Ht2[!df_dg_Ht2$query_def %in% miss_query_def3, ]
#get the 'query_def' that are not missing an ID
not_missID4 <- unique(df_gHBS_Hits$query_def)
#add up vectors
not_missID5 <- c(not_missID3,not_missID4)
#exclude these already IDed
df_xml07 <- df_xml04[!df_xml04$query_def %in% not_missID5, ]

#unique(df_xml07$query_def)
#get column number for column names
#https://stackoverflow.com/questions/9277363/get-the-column-number-in-r-given-the-column-name
BMc1 <- which( colnames(df_xml04)=="BestMatch1" )
BMc3 <- which( colnames(df_xml04)=="BestMatch3" )
#https://stackoverflow.com/questions/49882943/r-combine-columns-ignoring-nas
df_xml04$BestMatch4 <- apply(df_xml04[,BMc1:BMc3], 1, function(x) x[!is.na(x)][1])

#colnames(df_xml04)
#define which column names you need for a list
keeps <- c("query_def",
           #"Hit_genus",
           #"Hit_species",
           #"Hit_genus_species",
           "NCBIAccsno",
           "FshSpcNo",
           "BestMatch4")
#only keep selected columns from list
df_xml08 <- df_xml04[keeps]
#change column names
names(df_xml08)[names(df_xml08) == 'NCBIAccsno'] <- 'match_wNCBIAccsno'
names(df_xml08)[names(df_xml08) == 'BestMatch4'] <- 'Best_Seq_Match'
#subset with unique cases
#https://stackoverflow.com/questions/11369961/subset-with-unique-cases-based-on-multiple-columns
df_xml09 <- df_xml08[!duplicated(df_xml08$query_def),]
# see it as a html table
html_df_09 <- df_xml09 %>%
  tableHTML 

df_xml09$query_def
write_tableHTML(html_df_09,"bilag07_resulting_matches.html")
# get those rows that are still missing and ID
df_mssID <- df_ep01[!df_ep01$PCRnoTb %in% df_xml09$query_def, ]
#define which column names you need for a list
keeps <- c("Extraction",
           "PCRnoTb"
           )
#only keep selected columns from list
df_mssID <- df_mssID[keeps]
# see it as a html table
df_mssID %>%
  tableHTML 





