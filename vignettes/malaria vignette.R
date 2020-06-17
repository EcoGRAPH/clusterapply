rm(list=ls())
library(clusterapply)
library(ggplot2)

load("c:\\home\\Dropbox (Univ. of Oklahoma)\\work\\neko\\for_others\\20200615_bb_input\\20200615_bb_pfm_input.RData")

reg_eq <- formula("log(cases_epidemiar+1) ~ woreda_name +
                    s(doy, bs = 'cc', by=woreda_name, id = 1) +
                    s(numericdate, by = woreda_name, bs = 'tp', id = 2) +
                    s(lag, by = lst_day, bs = 'tp', id = 3) +
                    s(lag, by = ndwi6, bs = 'tp', id = 4) +
                    s(lag, by = totprec, bs = 'tp', id = 5)")
reg_eq_fallback <- formula("log(cases_epidemiar+1) ~ 1 +
                    s(doy, bs = 'cc', id = 1) +
                    s(numericdate, bs = 'tp', id = 2) +
                    s(lag, by = lst_day, bs = 'tp', id = 3) +
                    s(lag, by = ndwi6, bs = 'tp', id = 4) +
                    s(lag, by = totprec, bs = 'tp', id = 5)")
reg_eq_broken  <- formula("log(cases_epidemiar+1) ~ s(lag, id=1, bs='fz')")
fc_model_family <- gaussian()

# time regressions with no error
starttime <- proc.time()

regress <- clusterapply::batch_bam(data = epi_input_tp,
                                   bamargs = list("formula" = reg_eq,
                                                  "family" = fc_model_family,
                                                  "discrete" = TRUE),
                                   bamargs_fallback = list("formula" = reg_eq_fallback),
                                   over = "cluster_id")

epi_input_tp$preds <- clusterapply::predict.batch_bam(models=regress,
                                                 predictargs=list("discrete"=TRUE),
                                                 over="cluster_id",
                                                 newdata=epi_input_tp)

stoptime <- proc.time() - starttime
stoptime

# perform a function on the regressions
clusterapply::extractAIC.batch_bam(regress)

# plot obs vs. pred
ggplot(epi_input_tp) + geom_point(aes(x=log(cases_epidemiar+1), y=preds)) +
  geom_abline(slope=1, intercept=0, linetype=2, size=2, color="red") +
  coord_equal()

regress[[1]]

# what happens when you have an error but have a fallback formula?
regress <- clusterapply::batch_bam(data = epi_input_tp,
                                   bamargs = list("formula" = reg_eq_broken,
                                                  "family" = fc_model_family,
                                                  "discrete" = TRUE),
                                   bamargs_fallback = list("formula" = reg_eq_fallback),
                                   over = "cluster_id")

regress[[1]]
