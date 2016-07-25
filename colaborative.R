library(readr)
library(dplyr)
library(tidyr)
library(Matrix)
source("plot_func.R")


#LOAD AND DATA PREPROCESSING====================================================
df<-read_delim("data/data_rawV2.csv",quote = "\'",delim=",")

target_show<-"HALT AND CATCH FIRE"

#select segment of persons who viewed target_show 
person_segment<-df %>% 
        filter(PROGRAM_TYPE==target_show,TOTAL_MIN>=6) %>% 
        distinct(HOUSEHOLD_NUMBER,PERSON_ID) %>%
        select(HOUSEHOLD_NUMBER,PERSON_ID) 

#get count of persons in segment
COUNT_PERSON_SEGMENT<-nrow(person_segment)

#Extract viewing behaviour of segment
segment<- person_segment %>% 
        inner_join(df) %>%
        mutate(CONTENT=as.factor(paste(NETWORK,PROGRAM_TYPE,sep="_"))) %>%
        mutate(USER=as.factor(paste(HOUSEHOLD_NUMBER,PERSON_ID,sep="_")))  %>%
        group_by(USER,BROADCAST_DATE,DAY,HOUR_KEY) %>%
        mutate(AVAILABILITY=sum(TOTAL_MIN),
               MINUTES_SHARE=TOTAL_MIN/AVAILABILITY) %>%
        ungroup()

rm(df,person_segment,COUNT_PERSON_SEGMENT)



#CREATE INDEX DICTIONARIES FOR SPARSE MATRIX CREATION===========================

#create content dictionary
content_names<-sort(unique(paste(segment$NETWORK,segment$PROGRAM_TYPE,sep="_")))
dict_content<-data.frame(ID_CONTENT=1:length(content_names),
                         CONTENT=content_names)
rm(content_names)

#create user dictionary
user_id<-sort(unique(paste(segment$HOUSEHOLD_NUMBER,segment$PERSON_ID,sep="_")))
dict_user<-data.frame(ID_USER=1:length(user_id),USER=user_id)
rm(user_id)

#create schedule dictionary
dict_schedule<-segment %>%
        distinct(BROADCAST_DATE,DAY,HOUR_KEY) %>%
        arrange(BROADCAST_DATE,DAY,HOUR_KEY) %>%
        select(BROADCAST_DATE,DAY,HOUR_KEY)

dict_schedule$ID_SCHEDULE<-1:nrow(dict_schedule)

#show_capacity minutes
show_capacity<-segment %>% 
        distinct(NETWORK,PROGRAM_TYPE,BROADCAST_DATE,DAY,HOUR_KEY) %>%
        group_by(NETWORK,PROGRAM_TYPE) %>%
        summarise(CAPACITY_MINUTES=n()*60)


#merge dictonaries with segment
segment<-segment %>% 
        inner_join(dict_content) %>%
        inner_join(dict_user) %>% 
        inner_join(dict_schedule)  %>%
        inner_join(show_capacity)

rm(show_capacity)

#USER PREFERENCE MODELING=======================================================

#create content preference dataframe
content_pref<-segment %>% 
        group_by(USER,ID_USER,CONTENT,ID_CONTENT,CAPACITY_MINUTES) %>%
        summarise(MINUTES=sum(TOTAL_MIN)) %>% 
        group_by(ID_USER) %>%
        mutate(TOTAL_MINUTES=sum(MINUTES),
               SHOW_VIEW_PERCENT=MINUTES/TOTAL_MINUTES,
               LIKE=MINUTES/CAPACITY_MINUTES) 

#pack it to sparse matrix
sparse_content_pref <- sparseMatrix(i = content_pref$ID_USER,
                         j = content_pref$ID_CONTENT,
                         x = content_pref$LIKE)

#add row and column names to sparse matrix
row.names(sparse_content_pref)<-dict_user$USER
colnames(sparse_content_pref)<-dict_content$CONTENT
#===============================================================================

#schedule data creation
schedule<-segment %>%
        distinct(ID_SCHEDULE,CONTENT) %>%
        select(BROADCAST_DATE,DAY,HOUR_KEY,NETWORK,PROGRAM_TYPE,ID_SCHEDULE,ID_CONTENT)


sparse_schedule <- sparseMatrix(i = schedule$ID_SCHEDULE,
                               j = schedule$ID_CONTENT,
                               x = 1)

colnames(sparse_schedule)<-dict_content$CONTENT
rownames(sparse_schedule)<-dict_schedule$ID_SCHEDULE
#==============================================================================

#BEHAVIOUR MODEL SPARSE MATRIX==================================================

BM<-sparse_schedule[segment$ID_SCHEDULE,]*
        sparse_content_pref[segment$ID_USER,]