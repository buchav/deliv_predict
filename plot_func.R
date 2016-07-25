library(ggplot2)

availability_fig<-function(dfa,HH_ID=NULL,PP_ID=NULL,PERC=TRUE,title=""){
        if (!is.null(HH_ID) & 
            !is.null(PP_ID) &
            any(c("HOUSEHOLD_NUMBER","PERSON_ID") %in% names(dfa))){
                dfa<-dfa %>% filter(HOUSEHOLD_NUMBER==HH_ID,PERSON_ID==PP_ID)
                if (title=="") title<-paste("Availability person:",paste(HH_ID,PP_ID,sep = "_"))
        }
                
                fig<-ggplot(dfa,aes(DAY,HOUR_KEY))+
                        scale_fill_gradient(low = "white",high = "red")+
                        labs(title=title)+
                        scale_y_reverse()
                
                if (PERC){
                        fig<-fig+geom_tile(aes(fill = AVAILABILITY_INDEX),
                                           colour = "white")+
                                geom_text(aes(label=round(AVAILABILITY_INDEX,
                                                          digits = 3)))
                }else {
                        fig<-fig+geom_tile(aes(fill = MINUTES_AVAILABLE),
                                           colour = "white")+
                                geom_text(aes(label=MINUTES_AVAILABLE))
                }
                
                fig                
                
                
}