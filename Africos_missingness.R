#####  Check for type of missingness  #####

### Subset of main AFRICOS databset to include Uganda and Kenya only.
### TZ and Nigeria excluded, as they had >70% missing on cervical exams. 

tabcontrols <- tableby.control(test= TRUE, total = F, numeric.simplify = T, cat.simplify = F, 
                               numeric.test = "anova", numeric.stats = c("meansd", "medianq1q3"), 
                               cat.stats = "countpct", cat.test = "chisq", digits=1L)

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

tab1_miss <- tableby(exam_done ~ hiv + age + country + timeToClinic_min + para + sxptlife + sxpt6mo +
                     condom6mo, data=via_enroll_KyUg, cat.stats = "countrowpct", control = tabcontrols )

write2(tab1_miss, "table1missingness_KyUg",
       title = "Table: Missingness type", 
       output_format = rmarkdown::word_document)

### Sensitivity analysis - worst/best case analysis ### 
via_enroll_KyUg_best <- via_enroll_KyUg %>% 
     mutate(exam_result_sens = as.factor(ifelse(lesions == 4, 1, exam_result)), #1=norm, 2=lesions, 3=ab no aceto
            exam_result2_sens = as.factor(ifelse(exam_result_sens == 2, 1, 0)), #1=lesions, 2=no lesions
            exam_result3_sens = as.factor(ifelse(exam_result_sens %in% c(2,3), 1, 0))) #1=any ab, 2=none
     
via_enroll_KyUg_worst <- via_enroll_KyUg %>% 
     mutate(exam_result_sens = as.factor(ifelse(lesions == 4, 2, exam_result)), #1=norm, 2=lesions, 3=ab no aceto
            exam_result2_sens = as.factor(ifelse(exam_result_sens == 2, 1, 0)), #1=lesions, 2=no lesions
            exam_result3_sens = as.factor(ifelse(exam_result_sens %in% c(2,3), 1, 0)))#1=any ab, 2=none


### Best case analysis ### 

## Acetowhite lesions vs no lesions 
#via_enroll_KyUg_best$exam_result2 <- relevel(via_enroll_KyUg_best$exam_result2, ref = 1)
via_enroll_KyUg_best$hiv <- relevel(as.factor(via_enroll_KyUg_best$hiv), ref = "HIV-Negative")
model_result <- glm(exam_result2_sens ~ as.factor(hiv) + age + sxptlife + country,
                    data = via_enroll_KyUg_best, family = 'binomial')
orCI <- cbind(tidy(model_result, conf.int = F, conf.level = 0.95, exponentiate = T), 
              exp(confint_tidy(model_result, conf.level = 0.95, func = stats::confint.default)))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab5 <- cbind(orCI$term, orCI$estimate, orCI$ci, orCI$p.value)
colnames(tab5) <- c("Variable", 'aOR', '95% CI', 'P-value')

write.csv(tab5, file="table4_acetowhitelesions_KyUg_best.csv")

### 3-level cervical exam outcomes among all participants ### 
via_enroll_KyUg_best$exam_result_sens <- relevel(via_enroll_KyUg_best$exam_result_sens, ref = 1)
model_result <- multinom(exam_result_sens ~ hivflag + age + sxptlife ,
                         data = via_enroll_KyUg_best)
orCI <- cbind(tidy(model_result, conf.int = T, conf.level = 0.95, exponentiate = T))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab6 <- cbind(orCI$y.level, orCI$term, orCI$estimate, orCI$ci, orCI$p.value)

colnames(tab6) <- c("Outcome", "Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab6, file="table2_multinom_KyUg_best.csv")

### Worst case analysis ### 

## Acetowhite lesions vs no lesions 
#via_enroll_KyUg_best$exam_result2 <- relevel(via_enroll_KyUg_best$exam_result2, ref = 1)
via_enroll_KyUg_worst$hiv <- relevel(as.factor(via_enroll_KyUg_worst$hiv), ref = "HIV-Negative")
model_result <- glm(exam_result2_sens ~ as.factor(hiv) + age + sxptlife + country,
                    data = via_enroll_KyUg_worst, family = 'binomial')
orCI <- cbind(tidy(model_result, conf.int = F, conf.level = 0.95, exponentiate = T), 
              exp(confint_tidy(model_result, conf.level = 0.95, func = stats::confint.default)))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab7 <- cbind(orCI$term, orCI$estimate, orCI$ci, orCI$p.value)
colnames(tab7) <- c("Variable", 'aOR', '95% CI', 'P-value')

write.csv(tab7, file="table4_acetowhitelesions_KyUg_worst.csv")

### 3-level cervical exam outcomes among all participants ### 
via_enroll_KyUg_worst$exam_result_sens <- relevel(via_enroll_KyUg_worst$exam_result_sens, ref = 1)
model_result <- multinom(exam_result_sens ~ hivflag + age + sxptlife ,
                         data = via_enroll_KyUg_worst)
orCI <- cbind(tidy(model_result, conf.int = T, conf.level = 0.95, exponentiate = T))
orCI <- orCI %>% mutate_if(is.numeric, round, 2)

orCI$ci <- paste(round(orCI$conf.low, 2), round(orCI$conf.high, 2), sep = "-")
tab8 <- cbind(orCI$y.level, orCI$term, orCI$estimate, orCI$ci, orCI$p.value)

colnames(tab8) <- c("Outcome", "Variable", 'aOR', '95% CI', 'P-value')
write.csv(tab8, file="table2_multinom_KyUg_worst.csv")
