#packages
library(dplyr) 
library(rio)
library(tidyverse)
library(corrr) 
library(sjlabelled)
library(parallel)
library(doParallel)
library(haven)


#c("dplyr", "corrr", "rio", "tidyverse", "sjlabelled"))
############################ Import data

DATAFORLOOP <- read_dta("Data/CSES/BIG3.dta") ### Tomas: still only the BIG3 here! -- that is not the final code I am qutie sure. on OSF it refers to this version: cluster script similarity variables 0711.R

############################ Select test sample
#
#DATAFORLOOP <- DATAFORLOOPORIG %>% 
#  group_by(country) %>%
#  do(sample_n(.,20)) %>% 
#  ungroup()
#
DATAFORLOOP %>% 
  group_by(country) %>% 
  summarise(n = n())

#
############################ get cores ready

# cores
n.cores <- parallel::detectCores() -1
n.cores <- 3

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()


############################ run parallel loop

#make sure that ids are all unique with duplicated()

  foreachlist <- foreach(cname = names(table(DATAFORLOOP$country)),
                      .packages= c("dplyr", "corrr", "rio", "tidyverse", "sjlabelled"))  %dopar% {
                        
                        dat_selectloop <- DATAFORLOOP[which(DATAFORLOOP$country == cname),]
                        
                        #if(length(table(duplicated(dat_selectloop$id)))>1)
                        #{
                        #  print("warning duplicate ids")
                        #}
                        
                        transposed_supp <- dat_selectloop %>% 
                          dplyr::select(bs_ideology,
                                        bs_health  ,
                                        bs_educ    ,
                                        bs_umeploy ,
                                        bs_defense ,
                                        bs_pension ,
                                        bs_busines ,
                                        bs_crime   ,
                                        bs_welfare ) %>% # put the column of the attitude/issue items here
                          t(.) %>% # transposes the whole thing and puts it on its side
                          `colnames<-`(dat_selectloop$id) %>% # makes the column names the participant ID (so use whatever participant ID is in your data)
                          as.data.frame()
                        
                        #this is where I calculate the structure similarity/difference
                        attitude_compare <- correlate(transposed_supp, use = "pairwise.complete.obs") %>% # calculates cors
                          #  shave() %>% # removes half the diag (to avoid repeats)
                          stretch(na.rm = FALSE) %>% # makes it a long data from with two columsn, one for X and one for Y
                          drop_na(r) %>% # remove missing values - this remove them after correlation i.e., removes diagonal self with self
                          dplyr::rename(agreement = r) %>% # calls the raw correlation "agreement"
                          dplyr::mutate(logic = abs(agreement)) %>% # BS structure/logic similarity by taking absolute value
                          dplyr::select(x, y, agreement, logic) # selects key columns. X and Y will have the participant IDs
                        
                        #this is where I calculate teh content similarity/difference
                        allsimdata <- as.data.frame(attitude_compare)
                        supp <- t(transposed_supp)
                        allsimdata$content.sim <- rowMeans(abs(supp[allsimdata[,1], ] - supp[allsimdata[,2], ] ),  na.rm = TRUE)
                        allsimdata$country <- cname

                       allsimdata
                      } 
  
  
  foreachlistdf <- bind_rows(foreachlist, .id = NULL)
  
  write.dta(foreachlistdf,"Data/CSES/big3.dta")




# stop cluster
parallel::stopCluster(cl = my.cluster)


############## original loop
#
#looplist1 <- list()
#
#countnamesvec <- names(table(DATAFORLOOP$country))
#
## for(i in length(table(dat_select$country)))
#for (i in 1:length(table(DATAFORLOOP$country))) {
#  print(i)
#  dat_selectloop <- DATAFORLOOP %>% dplyr::filter(country == countnamesvec[i])
#  
#  transposed_supp <- dat_selectloop %>% 
#    dplyr::select(bs_ideology,
#                  bs_health  ,
#                  bs_educ    ,
#                  bs_umeploy ,
#                  bs_defense ,
#                  bs_pension ,
#                  bs_busines ,
#                  bs_crime   ,
#                  bs_welfare ) %>% # put the column of the attitude/issue items here
#    t(.) %>% # transposes the whole thing and puts it on its side
#    `colnames<-`(dat_selectloop$id) %>% # makes the column names the participant ID (so use whatever participant ID is in your data)
#    as.data.frame()
#  
#  attitude_compare <- correlate(transposed_supp) %>% # calculates cors
#    #  shave() %>% # removes half the diag (to avoid repeats)
#    stretch(na.rm = FALSE) %>% # makes it a long data from with two columsn, one for X and one for Y
#    drop_na(r) %>% # remove missing values
#    dplyr::rename(agreement = r) %>% # calls the raw correlation "agreement"
#    dplyr::mutate(logic = abs(agreement)) %>% # BS structure/logic similarity by taking absolute value
#    dplyr::select(x, y, agreement, logic) # selects key columns. X and Y will have the participant IDs
#  
#  
#  allsimdata <- as.data.frame(attitude_compare)
#  allsimdata$content.sim <- colMeans(abs(transposed_supp[ , allsimdata[,1] ] - transposed_supp[ , allsimdata[,2] ] ),  na.rm = TRUE)
#  allsimdata <- cbind(country= get_labels(DATAFORLOOP$country)[i], allsimdata)
#  
#  
#  looplist1[[i]] <- allsimdata
#  names(looplist1)[i] <- get_labels(DATAFORLOOP$country)[i]
#  
#}
