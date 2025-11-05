get_register_data <- function(pop){
    if (FALSE){
        pop = tar_read(bsl_population)
    }
    setkey(pop,id,time)
    bsl <- pop[time == 0,.(id,gender,trt,bslkarnofsky = karnofsky)]
    bsl[,start_followup_date:= as.Date("2011-11-11")+runif(NROW(pop[time == 0]),min = -790,max = 1500)]
    pop = bsl[,.(id,start_followup_date)][pop,on = "id"]
    death_data <- pop[event == "death",.(id,date = start_followup_date+time*30.45)]
    PCP_data <- pop[event == "PCP/visit",.(id,date = start_followup_date+time*30.45)]
    censored_data <- pop[pop[,.I[.N],by = "id"]$V1][,.(id,date = start_followup_date+time*30.45,event = event)]
    censored_data <- censored_data[event != "death",.(id,date = date)]
    karnofsky <- pop[event == "visit",.(id,date = start_followup_date+time*30.45,value = changekarnofsky)]
    # dichotomize karnofsky
    karnofsky <- karnofsky[value < -1,.(id,date)]
    treatment_data <- list(bactrim = rbind(pop[event == "baseline"&trt == 1,.(id,date = start_followup_date+time*30.45)],
                                           pop[event == "visit"& ((trt == 1 & continue == 1)|(trt == 0 & crossover == 1)),.(id,date = start_followup_date+time*30.45)]),
                           AP = rbind(pop[event == "baseline"&trt == 0,.(id,date = start_followup_date+time*30.45)],
                                      pop[event == "visit"& ((trt == 0 & continue == 1)|(trt == 1 & crossover == 1)),.(id,date = start_followup_date+time*30.45)]))
    bsl[,trt := NULL][]
    list(baseline_data = bsl,
         timevar_data = c(list(PCP = PCP_data,karnofsky = karnofsky),treatment_data),
         death_data = death_data,
         censored_data = censored_data)
}
