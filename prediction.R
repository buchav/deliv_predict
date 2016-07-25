
source("distance_func.R")
library(foreach)
library(doParallel)

tresh<-0.95

#===============================================================================
target_schedule<-segment %>%  
        filter(PROGRAM_TYPE==target_show) %>%
        distinct(BROADCAST_DATE,DAY,HOUR_KEY) %>% 
        select(BROADCAST_DATE,DAY,HOUR_KEY,ID_SCHEDULE)  %>%
        mutate(ID_SCHEDULE_TEST=row_number())

test_schedule_availability<-target_schedule %>%
        inner_join(segment) %>%
        group_by(BROADCAST_DATE,DAY,HOUR_KEY,
                 USER,ID_USER,ID_SCHEDULE,ID_SCHEDULE_TEST) %>%
        summarise(AVAILABLE_MINUTES=sum(TOTAL_MIN))

#GENERATE TEST SCHEDULE=========================================================

#take original schedule 
test_schedule<-sparse_schedule[target_schedule$ID_SCHEDULE,]
rownames(test_schedule)<-target_schedule$ID_SCHEDULE_TEST

#find all AMC shows at test schedule and mark off
AMC_index<-grep("^AMC_",colnames(test_schedule))
test_schedule[,AMC_index]<-0

#modify test schedule so target show is on air
target_show_index<-grep("AMC_HALT AND CATCH FIRE",colnames(test_schedule))
test_schedule[,target_show_index]<-1

#COMPUTE PREDICTION FOR PERSON IN NEW SCHEDULE==================================

BP<-NULL
i<-1305

#transpose BM matrix for faster similarity distance calculation
system.time(tBM<-t(BM))

#register Do Parallel backend
registerDoParallel(cores=4)  

#create log socket 
log.socket <- make.socket(port=4000)

Log <- function(text, ...) {
        msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
        cat(msg)
        write.socket(log.socket, msg)
}

system.time(
#for  (i in 1:nrow(test_schedule_availability))
BP<-foreach(i= 1:nrow(test_schedule_availability),.combine = "rbind") %dopar%
        {
        Log("Processing block %d of %d", i, nrow(test_schedule_availability))             
                
        on_air<-test_schedule[test_schedule_availability$ID_SCHEDULE_TEST[i],]
        user_preference<-sparse_content_pref[test_schedule_availability$ID_USER[i],]

        hyp_schedule=on_air*user_preference
        
        #compute behaviour similarity index
        # system.time(SB<-apply(BM,1,tanimoto_dist,hyp_schedule))
        # system.time(SB<-tanimoto_dist_fast(BM,hyp_schedule))
        SB<-tanimoto_dist_fast_transpose(tBM,hyp_schedule)
        
        SB_good_index<-SB>tresh
        pred<-segment[SB_good_index,]
        pred$similarity<-SB[SB_good_index]
        
        #we need to filter out similar behaviour with content not presented in hyp schedule
        on_air_names<-names(on_air[on_air!=0])
        
        
        summary_view<-pred %>% 
                filter(CONTENT %in% on_air_names) %>%
                group_by(CONTENT) %>%
                summarise(N=n(),
                          SUM_MINUTES=sum(TOTAL_MIN),
                          MEAN_MINUTES=mean(TOTAL_MIN),
                          MEAN_SIMILARITY=mean(similarity),
                          MEAN_MINUTES_SHARE=mean(MINUTES_SHARE),
                          AVAILABILITY=test_schedule_availability$AVAILABLE_MINUTES[i],
                          PREDICTED_MINUTES=AVAILABILITY*MEAN_MINUTES_SHARE) %>%
                ungroup() %>%
                mutate(PROB_COUNT=N/sum(N),
                       PROB_MINUTES=SUM_MINUTES/sum(SUM_MINUTES),
                       PREDICTED_MINUTES=AVAILABILITY*MEAN_MINUTES_SHARE*PROB_MINUTES) %>%
                mutate(BROADCAST_DATE=test_schedule_availability$BROADCAST_DATE[i],
                       DAY=test_schedule_availability$DAY[i],
                       HOUR_KEY=test_schedule_availability$HOUR_KEY[i],
                       USER=test_schedule_availability$USER[i],
                       ID_USER=test_schedule_availability$ID_USER[i],
                       ID_SCHEDULE=test_schedule_availability$ID_SCHEDULE[i],
                       ID_SCHEDULE_TEST=test_schedule_availability$ID_SCHEDULE_TEST[i]
                       )
}
)

close.socket(log.socket)
write_rds(BP,"data/BP.rds")




