
library(haven)
library(tidyverse)
library(sjlabelled)
library(lubridate)
library(dplyr)
library(openxlsx)
library(arsenal)
library(nnet)

load("ccs_11feb2020.Rdata") 
via <- ccs_11feb2020
attr(via$takearv, "label") <- "Currently taking ARV" 
attr(via$hivflag, "label") <- "HIV status at enrollement"
attr(via$hivstat, "label") <- "Current HIV status"
attr(via$diagdtn, 'label') <- "HIV diagnosed date"
attr(via$art_sdtn, "label") <- "Date started ART" 
attr(via$ccscrnd, "label") <- "Declined CC screening or had hysterectomy"
attr(via$examdt, "label") <- "Date of cervical exam"
attr(via$ab_vagin, "label") <- "Abnormal vaginal discharge"
attr(via$hm_b, "label") <- "Heavy menstrual bleeding"
attr(via$inter_b, "label") <- "Intermenstrual bleeding" 
attr(via$post_b, "label") <- "Post coital bleeding" 
attr(via$pain, "label") <- "Pain during sex" 
attr(via$lowpain, "label") <- "Low abdominal pain, unrelated to menstruation"
attr(via$exterg, "label") <- "External genitalia abnormality" 
attr(via$vagina, "label") <- "Vaginal wall abnormality"
attr(via$c_exam, "label") <- "Cervical exam"
attr(via$b_exam, "label") <- "Bimanual exam" 
attr(via$para, "label") <- "Number of births"
attr(via$artindt, "label") <- "CD4 at ART initiation test date"
attr(via$cd4artin, "label") <- "CD4 at ART initiation "
attr(via$vl, "label") <- "Viral load 1: undetectable, 10: <20 copies/mL, 17: <34 copies/mL, 20: <40copies/mL"
attr(via$famplan, "label") <- "Using contraception method"
attr(via$howfrkm, "label") <- "Distance from clinic"
attr(via$howfrest, "label") <- "Whether distance from clinic was estimated"
attr(via$hadsex, "label") <- "Ever had sex"
attr(via$sxptlnum, "label") <- "Number of lifetime partners" 
attr(via$sxptrnum, "label") <- "Number of regular partners in the past 6 months"
attr(via$sxactfrq, "label") <- "Number of sex acts in the last month"
attr(via$sxptcnum, "label") <- "Number of casual partners in the past 6 months"
attr(via$usecondr, "label") <- "Used condom at last sex with regular partner" 
attr(via$condrfrq, "label") <- "Freq of condom use with regular partner in last 6 months"
attr(via$usecondc, "label") <- "Use condom at last sex with casual partner"
attr(via$condcfrq, "label") <- "Freq of condom use with casual partner in last 6 months"
attr(via$hhincome, "label") <- "Weekly household income"
     
#length(unique(via$subjid)) # 2051 unique subjects 

via_enroll <- via %>% filter(visit == 1) %>%
     mutate(c_exam = case_when(c_exam==0 ~ "Normal", 
                               c_exam==1 ~ "Abnormal", 
                               c_exam==6 ~ "Not done"),
            c_exama = case_when(c_exama==0 ~ "No acetowhite lesion", 
                                c_exama==1 ~ "Acetowhite lesion"),
            c_examb = case_when(c_examb==0 ~ "No cryo", 
                                c_examb==1 ~ "Cryo"), 
            c_examz = case_when(c_examz==0 ~ "No other abnorm",
                                c_examz==1 ~ "Other abnormality"),
            lesions = as.factor(case_when(c_exam == "Normal" & c_exama == "No acetowhite lesion" ~ 1, #normal
                                c_exam == "Abnormal" & c_exama == "Acetowhite lesion" ~ 2, #abnormal, lesion 
                                c_exam == "Abnormal" & c_exama == "No acetowhite lesion" ~ 3, #abnormal, no lesion
                                c_exam == "Not done" | is.na(c_exam) ~ 4)), #not done
            exam_result = as.factor(ifelse(lesions != 4, lesions, NA)),
            exam_result2 = as.factor(ifelse(exam_result == 2, 1, 0)),
            exam_result3 = as.factor(ifelse(exam_result %in% c(2,3), 1, 0)),
            exam_done = as.numeric(ifelse(lesions != 4, 1, 0)),
            suppress_200 = ifelse(vl < 200, 1, 0),
            suppress_1000 = ifelse(vl < 1000, 1, 0), 
            vl_cat = as.factor(case_when(vl < 1000 ~ "<1000", 
                                         vl >= 1000 & vl < 10000 ~ "1000-9999",
                                         vl >= 10000 & vl < 50000 ~ "10000-49999", 
                                         vl >= 50000 ~ ">=50000")),
            country_curr = case_when(hcurrtyp == 1 ~ "Kenya", hcurrtyp == 2 ~ "Nigeria",
                                hcurrtyp == 3 ~ "Tanzania", hcurrtyp == 4 ~ "Uganda", 
                                TRUE ~ "Unk"), 
            country = as.factor(case_when(progid==2|progid== 3 ~ "Kenya", 
                                progid==1 ~ "Uganda",
                                progid==4 ~ "Tanzania",
                                progid==5 ~ "Nigeria")),
            timeToClinic_min = (howlongh * 60) + howlongm,
            sxptlife = ifelse(sxptlnum >=300, NA, sxptlnum),
            sxpt6mo = sxptcnum + sxptrnum, 
            condom6mo = pmin(condrfrq, condcfrq, na.rm=T), 
            condom6mo = as.factor(case_when(condom6mo == 0 ~ "Never", 
                                            condom6mo== 1 | condom6mo==2  ~ "Sometimes", 
                                            condom6mo==3 ~ "All the time", 
                                            TRUE ~ "No response")), 
            takearv = as.factor(takearv), 
            hiv = case_when(hivflag == 2 ~ "HIV-Negative", 
                            hivflag == 1 & takearv == 1 ~ "HIV-Positive, on ART",
                            hivflag == 1 & (takearv ==0 | is.na(takearv)) ~ "HIV-Positive, not on ART"),
            cd4nadir_cat = as.factor(case_when(cd4nadir < 200 ~ "<200", 
                                               cd4nadir >= 200 & cd4nadir < 500 ~ "200-499", 
                                               cd4nadir >= 500 ~ ">=500", 
                                               TRUE ~ "NA") ), 
            cd4artin = as.numeric(cd4artin),
            cd4artin_cat = as.factor(case_when(cd4artin < 200 ~ "<200", 
                                               cd4artin >= 200 & cd4artin < 500 ~ "200-499", 
                                               cd4artin >= 500 ~ ">=500",
                                               TRUE ~ "NA"))) 


tabcontrols <- tableby.control(test= TRUE, total = F, numeric.simplify = T, cat.simplify = F, 
                               numeric.test = "anova", numeric.stats = c("meansd", "medianq1q3"), 
                               cat.stats = "countpct", cat.test = "chisq", digits=1L)

hiv_pos <- via_enroll %>% filter(hivflag == 1)
labels(hiv_pos) <- c(age = "Age", country = "Country",
                  timeToClinic_min = "Travel time to clinic (mins)",
                  para = "Number of births",
                  sxptlife = "Number of sex partners (lifetime)",
                  sxpt6mo = "Number of sex partners (past 6 months)",
                  condom6mo = "Condom use (past 6 months)" ,
                  dur_hiv = "Year since HIV diagnosis" ,
                  takearv = "Currently taking ART",
                  dur_art = "Years since starting ART",
                  vl_cat = "Viral load",
                  cd4nadir_cat = "Nadir CD4+ cell count",
                  cd4artin_cat = "CD4+ cell count at ART initiation",
                  lesions = "Cervical exam results")

tab1 <- tableby(hiv ~ age + country + timeToClinic_min + para + sxptlife + sxpt6mo +
                      condom6mo + lesions, data=via_enroll, control = tabcontrols )

tab2 <- tableby(hiv ~ dur_hiv + vl_cat + cd4nadir_cat + cd4artin_cat, data=hiv_pos,
                control = tabcontrols)

write2(tab1, "table1",
       title = "Table 1. Descriptive statistics of AFRICOS participants at enrollemnt by HIV status.", 
       output_format = rmarkdown::word_document)
write2(tab2, "table1_pos",
       title = "Table 1. Descriptive statistics of AFRICOS participants at enrollemnt, HIV-positive only.", 
       output_format = rmarkdown::word_document)


##### logistic regressions #####

### cervical exam outcomes among all participants ###  
model_exam <- glm(exam_done ~ country, data = via_enroll)
coefs <- coef(model_exam)
or <- exp(coefs)

chi <- table(via_enroll$exam_result, via_enroll$country)
chisq.test(chi)

via_enroll$exam_result2 <- relevel(via_enroll$exam_result2, ref = 1)
model_result_unadj <- glm(exam_result2 ~ factor(hivflag) , data = via_enroll, 
                          family = "binomial")
unadj_orCI <- tidy(model_result_unadj, conf.int = T, conf.level = 0.95, exponentiate = T)
View(unadj_orCI)


via_enroll$hivflag <- relevel(as.factor(via_enroll$hivflag), ref = 2)
model_result <- glm(exam_result3 ~ as.factor(hivflag) + age + sxptlife ,
                    data = via_enroll, family = 'binomial')
orCI <- cbind(tidy(model_result, conf.int = F, conf.level = 0.95, exponentiate = T), 
              exp(confint_tidy(model_result, conf.level = 0.95, func = stats::confint.default)))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)
View(orCI)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab3 <- cbind(orCI$term, orCI$estimate, orCI$ci, orCI$p.value)

colnames(tab3) <- c("Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab3, file="table2_any_abnormality.csv")


### 3-level cervical exam outcomes among all participants ### 
via_enroll$exam_result <- relevel(via_enroll$exam_result, ref = 1)
model_result <- multinom(exam_result ~ hivflag + age + sxptlife ,
                    data = via_enroll)
orCI <- cbind(tidy(model_result, conf.int = T, conf.level = 0.95, exponentiate = T))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)
View(orCI)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab3 <- cbind(orCI$y.level, orCI$term, orCI$estimate, orCI$ci, orCI$p.value)

colnames(tab3) <- c("Outcome", "Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab3, file="table2_multinom.csv")


### cervical exam outcomes among HIV-positive only ### 
hiv_pos$cd4nadir_cat <- relevel(hiv_pos$cd4nadir_cat, ref = ">=500")
model_hiv <- glm(exam_result3 ~ hiv + age + para + timeToClinic_min +
                 dur_hiv + cd4nadir_cat, data = hiv_pos, family = 'binomial')
orCI_hiv <- cbind(tidy(model_hiv, conf.int = F, conf.level = 0.95, exponentiate = T), 
              exp(confint_tidy(model_hiv, conf.level = 0.95, func = stats::confint.default)))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)

orCI_hiv$ci <- paste(round(orCI_hiv$conf.low, 2), round(orCI_hiv$conf.high, 2), sep = "-")
tab4 <- cbind(orCI_hiv$y.level, orCI_hiv$term, orCI_hiv$estimate, orCI_hiv$ci, orCI_hiv$p.value)

colnames(tab4) <- c("Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab4, file="table3_any_abnormality.csv")


model_hiv <- multinom(exam_result ~ hiv + age + para + timeToClinic_min +
                      dur_hiv + cd4nadir_cat, data = hiv_pos, family = 'binomial')
orCI_hiv <- cbind(tidy(model_hiv, conf.int = T, conf.level = 0.95, exponentiate = T))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)

orCI_hiv$ci <- paste(round(orCI_hiv$conf.low, 2), round(orCI_hiv$conf.high, 2), sep = "-")
tab4 <- cbind(orCI_hiv$y.level, orCI_hiv$term, orCI_hiv$estimate, orCI_hiv$ci, orCI_hiv$p.value)

colnames(tab4) <- c('VIA result', "Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab4, file="table3_multinom.csv")
