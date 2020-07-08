
library(haven)
library(tidyverse)
library(sjlabelled)
library(lubridate)
library(dplyr)
library(openxlsx)

load("ccs_11feb2020.Rdata") 

africos_via <- ccs_11feb2020 %>% 
     mutate(c_exam = case_when(c_exam==0 ~ "Normal", 
                               c_exam==1 ~ "Abnormal", 
                               c_exam==6 ~ "Not done"),
            c_exama = case_when(c_exama==0 ~ "No acetowhite lesion", 
                                c_exama==1 ~ "Acetowhite lesion"),
            c_examb = case_when(c_examb==0 ~ "No cryo", 
                                c_examb==1 ~ "Cryo"), 
            c_examz = case_when(c_examz==0 ~ "No other abnorm",
                                c_examz==1 ~ "Other abnormality"))

length(unique(africos_via$subjid)) # 2051 unique subjects 

referral <- africos_via %>% select(subjid, visitdt, visit, hivstat, age, examdt,
                                   c_exam, c_exama, c_examb, c_examz, oth10txt,
                                   ref, ref_whya, ref_whyb, ref_whyc, ref_whyz, ot11atxt,
                                   ref_nocon, ref_outa, ref_outb, ref_outc, 
                                   ref_outd, ref_oute, ref_outf, ref_outz) 
filename <- "G:/My Drive/UW/AFRICOS_VIA/MissingRef.xlsx"
Normal <- referral %>% filter(c_exam == "Normal" & ref == 1) %>% 
     rename(ref_other = ref_outz) %>%
     mutate(flag1 = ifelse(rowSums(select(., contains("ref_out"))) == 0, 1, 0), 
            flag2 = ifelse((ref_nocon == 1 | ref_other == 1 | flag1 == 1), 1, 0))%>% 
     filter(flag2 == 1)  

Abnormal <- referral %>% filter(c_exam == "Abnormal" & ref == 1) %>% 
     rename(ref_other = ref_outz) %>%
     mutate(flag1 = ifelse(rowSums(select(., contains("ref_out"))) == 0, 1, 0), 
            flag2 = ifelse((ref_nocon == 1 | ref_other == 1 | flag1 == 1), 1, 0)) %>%
     filter(flag2 == 1) 

Abnormal_noRef <- referral %>% 
     filter(c_exam == "Abnormal" & ref == 0 & c_examb != "Cryo") %>% 
     rename(ref_other = ref_outz) 

Missing_cExam <- referral %>% filter(is.na(c_exam)) %>%
     rename(ref_other = ref_outz) %>%
     mutate(flag1 =  ifelse(rowSums(select(., contains("ref_out"))) == 0, 1, 0), 
            flag2 = ifelse(rowSums(select(., contains("ref_why")))==0, 1, 0))

 

sheetnames <- c("Normal", "Abnormal", "Abnormal_noRef")

data_list <- list(Normal, Abnormal, Abnormal_noRef)

MissingRefs <- createWorkbook()
for (i in 1:length(sheetnames)) {
     addWorksheet(MissingRefs, sheetName = paste0(sheetnames[i]))
     x <- as.data.frame(data_list[i])
     writeData(MissingRefs, sheet=paste0(sheetnames[i]), x)
}

saveWorkbook(MissingRefs, filename, overwrite = T)
