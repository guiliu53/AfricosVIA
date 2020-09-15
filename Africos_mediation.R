##### Mediation analysis in HIV+ subset of AFRICOS Kenya and Uganda to estimate 
##### any mediation effect from viral load

library(mediation)

##### Potential outcomes method (Imai, 2010) ##### 
### Sequential ignorability assumption not satisfied ### 
hiv_pos_mediation <- hiv_pos %>% filter(!is.na(exam_result2)) %>%
     mutate(hiv_binary = ifelse(hiv == "HIV-Positive, not on ART", 0, 1))

model_direct <- glm(exam_result2 ~ hiv_binary + vl_num + age  + timeToClinic_min +
                         dur_hiv + cd4nadir_cat + country, data = hiv_pos_mediation, 
                    family = binomial('probit')) 
model_indirect <- lm(vl_num ~ hiv_binary + age  + timeToClinic_min +
                          dur_hiv + cd4nadir_cat + country, data = hiv_pos_mediation)

med.out <- mediate(model_indirect, model_direct, treat = "hiv_binary", mediator = 'vl_num', 
                   robustSE = T, sims = 1000)
summary(med.out)

# Total effect 
exp(med.out$tau.coef)
exp(med.out$tau.ci)

# ACME - indirect effect 
exp(med.out$d.avg)
exp(med.out$d.avg.ci)

# NDE - direct effect 
exp(med.out$z.avg)
exp(med.out$z.avg.ci)

##### Traditional method #####
### Check for interaction between exposure (HIV) and mediator (VL categories)
model_interact <- glm(exam_result2 ~ hiv_binary + vl_num + hiv_binary * vl_num, 
                      data = hiv_pos_mediation, family = 'binomial')
orCI_hiv <- cbind(tidy(model_interact, conf.int = F, conf.level = 0.95, exponentiate = T), 
                  exp(confint_tidy(model_interact, conf.level = 0.95, func = stats::confint.default)))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)
 # --- no interaction between HIV and mediator --- #

## Total effect ## 
model_hiv <- glm(exam_result2 ~ hiv_binary, data = hiv_pos_mediation, family = 'binomial')
orCI_hiv <- cbind(tidy(model_hiv, conf.int = F, conf.level = 0.95, exponentiate = T), 
                  exp(confint_tidy(model_hiv, conf.level = 0.95, func = stats::confint.default)))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)

## Direct effect ## 
model_direct <- glm(exam_result2 ~ hiv_binary + vl_num, data = hiv_pos_mediation, 
                    family = 'binomial')
orCI_hiv <- cbind(tidy(model_direct, conf.int = F, conf.level = 0.95, exponentiate = T), 
                  exp(confint_tidy(model_direct, conf.level = 0.95, func = stats::confint.default)))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)

## Indirect effect
 # difference in total - indirect beta coefficients 
diff <- (model_hiv$coefficients[2]) - (model_direct$coefficients[2])
View((diff))

 # product of direct effect coeff and coeff for vl ~ hiv
model_indirect <- lm(vl_num ~ hiv_binary, data = hiv_pos_mediation)
orCI_hiv <- cbind(tidy(model_indirect, conf.int = F, conf.level = 0.95, exponentiate = T), 
                  exp(confint_tidy(model_hiv, conf.level = 0.95, func = stats::confint.default)))
orCI_hiv <- orCI_hiv %>% mutate_if(is.numeric, round, 2)
View(orCI_hiv)
summary(model_indirect)

prod <- (model_indirect$coefficients[2]) * (model_direct$coefficients[3])
View((prod))
View(exp(prod)) # --> new OR, but method doesn't produce corrected CI 

#prorption mediated 
prop_med <- 1 - ((model_direct$coefficients[2])/(model_hiv$coefficients[2]))
View(prop_med)
