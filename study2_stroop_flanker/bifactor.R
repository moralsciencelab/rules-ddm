library(lavaan)
bifactor_data = read.csv('bifactor_analysis.csv')

bi_model <- 'flank =~ flanker1 + flanker2 + flanker3 + flanker4
                stroop =~ stroop1 + stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

bi_model_t <- 'flank =~ flanker1 + flanker2 + flanker3 + flanker4
                stroop =~ stroop2 + stroop3 + stroop4 
                rules =~ rules1 + rules2 + rules3 + rules4
                common =~ flanker1 + flanker2 + flanker3 + flanker4 + stroop1 + stroop2 + stroop3 + stroop4 + rules1 + rules2 + rules3 + rules4 
                '

### For non-decision time (remove stroop1)
bifactor_fit_t <- cfa(model = bi_model_t, data = subset(bifactor_data, parameter == 't'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_t, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

### Boundary separation
bifactor_fit_a <- cfa(model = bi_model, data = subset(bifactor_data, parameter == 'a'), 
                      std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_a, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## congruent drift rate
bifactor_fit_congruent <- cfa(model = bi_model, 
                              data = subset(bifactor_data, parameter == 'v' & conflict == 'congruent'), 
                              std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_congruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

bifactor_fit_incongruent <- cfa(model = bi_model, 
                                data = subset(bifactor_data, parameter == 'v' & conflict == 'incongruent'), 
                                std.lv = TRUE, orthogonal = TRUE)
summary(bifactor_fit_incongruent, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
