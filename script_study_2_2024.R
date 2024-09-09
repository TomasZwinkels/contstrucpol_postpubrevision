##################################install and load pacakges

# List of required packages
packages <- c("haven", "dplyr", "rio", "tidyverse", "corrr", "sjlabelled", 
              "Rfast", "lme4", "lmerTest", "cowplot", "foreign", "emmeans","stargazer")

# Function to install a package if it is missing
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

# Function to load a package
load_package <- function(p) {
  library(p, character.only = TRUE)
}

# Install all missing packages
lapply(packages, install_if_missing)

# Load all required packages
lapply(packages, load_package)

###################################import data

# raw data
	# version 4 - 2011-2016 -- the file that was origionally used
	CSES4 <- read_dta("Data/cses4.dta")

	# the integrated dataset, which we where suggested to use by David Young
	CSES <- read_dta("Data/cses_imd.dta")	

#The constructed similarity measures
logic_cont <- read_dta("Data/CSES/logic_cont_final.dta")

# prepared mlm data with similarity variables merged in
mlm.dat.fin <- readRDS("Data/CSES/mlm.dat.fin0711.Rda")

#################################### 
#table(duplicated(CSESIMD$IMD1005))
#table(duplicated(CSES4$D1005))
#
#################################### select data
CSES4_SELECT <- CSES4    %>%  select(D1005, D1006_UN, D1006_NAM, D3014, D3001_1, D3001_2, D3001_3, D3001_4, D3001_5, D3001_6, D3001_7, D3001_8, 
                                     D3011_A, D3011_B, D3011_C, D3011_D, D3011_E, D3011_F, D3011_G, D3011_H, D3011_I, D5201_A, D5201_B, D5201_C, D5201_D, D5201_E, D5201_F, D5201_G, D5201_H, D5201_I, 
                                     D3008_LH_PL, D3018_4, D3018_3,
                                     D2002, D2001_Y, D2003, D2020, D2030, D1028)  
CSES4_CLEAN <- CSES4_SELECT  %>% 
                                mutate(id       = D1005,
                                        country_num = as.numeric(D1006_UN),
                                        country = D1006_NAM,
                                        ideology= ifelse(D3014<11, D3014, NA),
                                        health  = ifelse(D3001_1<6, D3001_1, NA),
                                        educ    = ifelse(D3001_2<6, D3001_2, NA),
                                        umeploy = ifelse(D3001_3<6, D3001_3, NA),
                                        defense = ifelse(D3001_4<6, D3001_4, NA),
                                        pension = ifelse(D3001_5<6, D3001_5, NA),
                                        busines = ifelse(D3001_6<6, D3001_6, NA),
                                        crime   = ifelse(D3001_7<6, D3001_7, NA),
                                        welfare = ifelse(D3001_8<6, D3001_8, NA),
                                        polar_1 = as.numeric(ifelse(D3011_A<11, D3011_A, NA)),
                                        polar_2 = as.numeric(ifelse(D3011_B<11, D3011_B, NA)),
                                        polar_3 = as.numeric(ifelse(D3011_C<11, D3011_C, NA)),
                                        polar_4 = as.numeric(ifelse(D3011_D<11, D3011_D, NA)),
                                        polar_5 = as.numeric(ifelse(D3011_E<11, D3011_E, NA)),
                                        polar_6 = as.numeric(ifelse(D3011_F<11, D3011_F, NA)),
                                        polar_7 = as.numeric(ifelse(D3011_G<11, D3011_G, NA)),
                                        polar_8 = as.numeric(ifelse(D3011_H<11, D3011_H, NA)),
                                        polar_9 = as.numeric(ifelse(D3011_I<11, D3011_I, NA)),
                                        partya  = ifelse(D5201_A==9999,NA,D5201_A),
                                        partyb  = ifelse(D5201_B==9999,NA,D5201_B),
                                        partyc  = ifelse(D5201_C==9999,NA,D5201_C),
                                        partyd  = ifelse(D5201_D==9999,NA,D5201_D),
                                        partye  = ifelse(D5201_E==9999,NA,D5201_E),
                                        partyf  = ifelse(D5201_F==9999,NA,D5201_F),
                                        partyg  = ifelse(D5201_G==9999,NA,D5201_G),
                                        partyh  = ifelse(D5201_H==9999,NA,D5201_H),
                                        partyi  = ifelse(D5201_I==9999,NA,D5201_I),
                                        lowhouse=D3008_LH_PL,
                                        party   = as.numeric(D3018_3),
                                       closest  = D3018_4,
                                       gender   = ifelse(D2002==1,-1,                                       # makes controls as per pre-reg
                                                               ifelse(D2002 ==2,1,NA)),
                                       age      = ifelse(D1028 == 9999 | D2001_Y >= 9997, NA, D1028- D2001_Y),
                                       edu      = D2003,
                                       edu_c    = D2003 - median(D2003, na.rm=TRUE),           
                                       inc      = D2020,
                                       inc_c    = D2020 - mean(D2020, na.rm = TRUE),
                                       eth      = D2030        
                                       ) %>%
                                   dplyr::select(-D1005:-D3018_3) %>%
                                   dplyr::mutate(age_c = age-mean(age, na.rm = TRUE),
                                                 bs_ideology=  (ideology)/10,
                                                 bs_health  = (health  -1)/4,
                                                 bs_educ    = (educ    -1)/4,
                                                 bs_umeploy = (umeploy -1)/4,
                                                 bs_defense = 1-(defense -1)/4,
                                                 bs_pension = (pension -1)/4,
                                                 bs_busines = 1-(busines -1)/4,
                                                 bs_crime   = 1-(crime   -1)/4,
                                                 bs_welfare = (welfare -1)/4,
                                                 na_bs      = is.na(bs_ideology)+
                                                              is.na(bs_health )+
                                                              is.na(bs_educ   )+
                                                              is.na(bs_umeploy)+
                                                              is.na(bs_defense)+
                                                              is.na(bs_pension)+
                                                              is.na(bs_busines)+
                                                              is.na(bs_crime  )+
                                                              is.na(bs_welfare)
                                                 )

######### SAMPLE selection

# As per pre-reg remove pps for whom n>= 2 beleif system items are absent

# check missingness
#                      table(CSES4_CLEAN$na_bs)
#
#                      CSES4_CLEAN %>% 
#                        group_by(country) %>% 
#                        summarise(n = n())
#                      
#                      CSES4_CLEAN %>% 
#                        group_by(country) %>% 
#                        summarise_each(funs(sum(is.na(.))))
#
#
##filter if party is not measured in liking (i.e., party must be 1-9, if belief system items do not have more than 2 missing, AND if there are at least 20 party supporters for each group)
CSES4_SAMPLE_P1 <- CSES4_CLEAN %>% filter(party < 10 & na_bs<2) # should be 2 or more i.e.,  <3 ### TOMAS: should me more like if below letter L? 
#
CSES4_SAMPLE <- CSES4_SAMPLE_P1 %>% 
  group_by(country, party) %>% 
  filter(n() >20) %>% 
  ungroup()

                 # get number of cases per country 
                   CSES4_SAMPLE %>% 
                     group_by(country) %>% 
                     summarise(n = n())
                   
				   # get number of cases per country starting with smallest cases per country
                   CSES4_SAMPLE %>% 
                      count(country) %>% 
                     arrange(n)
        
                    
#make dataset containing smallest 25 countries
          #        SMALLEST25 <- CSES4_SAMPLE %>% group_by(country) %>% filter(n() < 1000)
          #         SMALLEST25 %>% 
          #           count(country) %>%
          #           arrange(n)
   
                   
#write.dta(CSES4_SAMPLE,"Data/Main survey/CSES4_SAMPLE.dta")


					# check missingness
                      CSES4_SAMPLE %>% 
                        group_by(country,party) %>% 
                        summarise(n = n())
                      
                      table(CSES4_SAMPLE$country, CSES4_SAMPLE$party)  
                      table(CSES4_SAMPLE$na_bs)
                      

## note that for some variables party_a differs across rows, e.g., for greece, party a is 47 for some people and 1592 for others.
#x <-       c(unique(CSES4_CLEAN$party1),
#             unique(CSES4_CLEAN$party2),
#             unique(CSES4_CLEAN$party3),
#             unique(CSES4_CLEAN$party4),
#             unique(CSES4_CLEAN$party5),
#             unique(CSES4_CLEAN$party6),
#             unique(CSES4_CLEAN$party7),
#             unique(CSES4_CLEAN$party8),
#             unique(CSES4_CLEAN$party9))
#    


############################################ Make BS similarity

# GO TO SCRIPT "cluster script final 1508"
                      
################################## loop

## parallel::detectCores()              
#              
#looplist1 <- list()
#
#countnamesvec <- names(table(CSES4_SAMPLE$country))
#
## for(i in length(table(dat_select$country)))
#for (i in 1:length(table(CSES4_SAMPLE$country))) {
#  print(i)
#  dat_selectloop <- CSES4_SAMPLE %>% dplyr::filter(country == countnamesvec[i])
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
#                      supp <- t(transposed_supp)
#                      allsimdata$content.sim <- rowMeans(abs(supp[allsimdata[,1], ] - supp[allsimdata[,2], ] ),  na.rm = TRUE)
#                      allsimdata <- cbind(country= get_labels(CSES4_SAMPLE$country)[i], allsimdata)
#  
#  
#  looplist1[[i]] <- allsimdata
#  names(looplist1)[i] <- get_labels(CSES4_SAMPLE$country)[i]
#
#  }
#
#newdf <- bind_rows(looplist1, .id = NULL)
#
#write.csv(newdf,"Data/Main survey/allcountriesparties selected.csv", row.names = FALSE)

###### See cluster script with foreach loop
#              
             
## now organize output by party supported.
#### ALRIGHT, SO THIS IS THE BIT OF CODE THAT NEEDS TO BE ADJUSTED.

#head(newdf)
#head(BIG3)
#newdf <- BIG3 ## ask Felicity about this ##

head(newdf)
allsimdata1 <- merge(x = newdf,        y = CSES4_SAMPLE[ , c("id", "party")], by.x = "x", by.y = "id",all.x=TRUE)
head(allsimdata1)

allsimdata2 <- merge(x = allsimdata1,  y = CSES4_SAMPLE[ , c("id", "party")], by.x = "y", by.y = "id",all.x=TRUE)

# filter to selct only those who support a party in aff_pol1-4

finalsimdata1 <- allsimdata2 %>% 
  mutate(match         = paste(party.x,party.y,sep="_"),
         ingroup       = ifelse(party.x == party.y, "ingroup","outgroup")
         
  )

finalsimdata <- finalsimdata1 %>% 
  mutate(target = ifelse(party.y==1,"partya",
                          ifelse(party.y==2,"partyb",
                                 ifelse(party.y==3,"partyc",
                                        ifelse(party.y==4,"partyd",
                                               ifelse(party.y==5,"partye",
                                                      ifelse(party.y==6,"partyf",
                                                             ifelse(party.y==7,"partyg",
                                                                    ifelse(party.y==8,"partyh",
                                                                           ifelse(party.y==9,"partyi",999))))))))))

# now  make a variable that takes the pps mean over ingroup matches and outgroup matches

logic <-   aggregate(logic       ~x+target, mean, data = finalsimdata, na.action = na.omit)
content <- aggregate(content_sim ~x+target, mean, data = finalsimdata, na.action = na.omit)

logic_cont <- merge(logic, content, by = c('x','target'))
logic_cont <- logic_cont %>% rename(  id = x,
                                      contentdif = content_sim
) 
##write.csv(logic_cont,"Data/Main survey/logic_cont.csv", row.names = FALSE)
#logic_contbig3 <- logic_cont
#logic_cont <- rbind(logic_contsmall, logic_contmid, logic_contbig3)

#write.dta(logic_cont,"Data/CSES/logic_contall.dta")

#logic_contsmall <- logic_cont
#logic_cont <- rbind(logic_contsmall, logic_contmid)
                      
logic_cont <- rename(logic_cont, partytarget = target)
                      
logic_cont <- logic_cont %>%
                  mutate(target = ifelse(partytarget == "partya", "polar_1",
                                        ifelse(partytarget == "partyb", "polar_2", 
                                               ifelse(partytarget == "partyc","polar_3", 
                                                      ifelse(partytarget == "partyd","polar_4", 
                                                             ifelse(partytarget == "partye","polar_5", 
                                                                    ifelse(partytarget == "partyf","polar_6", 
                                                                           ifelse(partytarget == "partyg","polar_7", 
                                                                                  ifelse(partytarget == "partyh","polar_8", 
                                                                                         ifelse(partytarget == "partyi","polar_9", NA))))))))))

#write.dta(logic_cont,"Data/CSES/logic_cont_final.dta")
############################ Make MLM dataset

mlm.dat <- pivot_longer(CSES4_SAMPLE, polar_1:polar_9,  names_to = "target", values_to = "aff.pol")
mlm.dat <- mlm.dat %>% mutate(targetorig = target)
mlm.dat <- as.data.frame(mlm.dat)

#add content and structure variables in
mlm.dat <- merge(mlm.dat, logic_cont, by = c('id','target'), all.x = TRUE)

#make final variables
mlm.dat.fin <- mlm.dat%>% 
                        mutate(affpoldummy = ifelse((party==1 & target == "polar_1")|(party==2 & target == "polar_2")|(party==3 & target == "polar_3")|(party==4 & target == "polar_4")|(party==5 & target == "polar_5")|(party==6 & target == "polar_6")|(party==7 & target == "polar_7")|(party==8 & target == "polar_8")|(party==9 & target == "polar_9"), "ingroup", 
                                                    ifelse((party==1 & target != "polar_1")|(party==2 & target != "polar_2")|(party==3 & target != "polar_3")|(party==4 & target != "polar_4")|(party==5 & target != "polar_5")|(party==6 & target != "polar_6")|(party==7 & target != "polar_7")|(party==8 & target != "polar_8")|(party==9 & target != "polar_9"), "outgroup", NA)),
                               outgroupdummy = ifelse(affpoldummy == "ingroup",0,
                                                      ifelse(affpoldummy == "outgroup",1,NA)),
                               content     = 1-contentdif,
                               logic_c     = mlm.dat$logic-mean(mlm.dat$logic,na.rm=TRUE),
                               ingroup     = as.factor(affpoldummy),
                               partynum    = paste0(country,party),
                               countrydummy= as.factor(country),
                               aff.pol.num = as.numeric(aff.pol)
                           
                        )                      
                               
mlm.dat.fin <- mlm.dat.fin%>% 
  mutate(content_c= mlm.dat.fin$content-mean(mlm.dat.fin$content,na.rm=TRUE),
         partydummy  = as.factor(partynum),
         logic_q     = logic_c^2,
         content_q     = (mlm.dat.fin$content-mean(mlm.dat.fin$content,na.rm=TRUE))^2)

#saveRDS(mlm.dat.fin, file = "Data/CSES/mlm.dat.fin0711.Rda")

################################### descritpives

#cor set

cordat <- mlm.dat.fin %>% 
          dplyr::select(aff.pol, logic_c, content_c, outgroupdummy)
cor(cordat, use = "pairwise.complete.obs")


cor(mlm.dat.fin$logic, mlm.dat.fin$content, use = "pairwise.complete.obs")
cor.test(mlm.dat.fin$logic, mlm.dat.fin$content, method = "pearson")

# get subsamples
subsamp.in <- subset(mlm.dat.fin, mlm.dat.fin$affpoldummy == "ingroup")
subsamp.ot <- subset(mlm.dat.fin, mlm.dat.fin$affpoldummy == "outgroup")
subsamp.t1 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_1"))
subsamp.t2 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_2"))
subsamp.t3 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_3"))
subsamp.t4 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_4"))
subsamp.t5 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_5"))
subsamp.t6 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_6"))
subsamp.t7 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_7"))
subsamp.t8 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_8"))
subsamp.t9 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "polar_9"))

# correlations for structure/logic
cor.test(subsamp.in$aff.pol, subsamp.in$logic, method = "pearson")
cor.test(subsamp.ot$aff.pol, subsamp.ot$logic, method = "pearson")

mean(c(
  cor.test(subsamp.t1$aff.pol, subsamp.t1$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t2$aff.pol, subsamp.t2$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t3$aff.pol, subsamp.t3$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t4$aff.pol, subsamp.t4$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t5$aff.pol, subsamp.t5$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t6$aff.pol, subsamp.t6$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t7$aff.pol, subsamp.t7$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t8$aff.pol, subsamp.t8$logic, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t9$aff.pol, subsamp.t9$logic, method = "pearson")$estimate[[1]]))

#correlations for content
cor.test(subsamp.in$aff.pol, subsamp.in$content, method = "pearson")
cor.test(subsamp.ot$aff.pol, subsamp.ot$content, method = "pearson")


mean(c(
  cor.test(subsamp.t1$aff.pol, subsamp.t1$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t2$aff.pol, subsamp.t2$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t3$aff.pol, subsamp.t3$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t4$aff.pol, subsamp.t4$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t5$aff.pol, subsamp.t5$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t6$aff.pol, subsamp.t6$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t7$aff.pol, subsamp.t7$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t8$aff.pol, subsamp.t8$content, method = "pearson")$estimate[[1]],
  cor.test(subsamp.t9$aff.pol, subsamp.t9$content, method = "pearson")$estimate[[1]]))


# histogram logic
p2 <- mlm.dat %>%
  ggplot( aes(x=logic, fill=affpoldummy)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

# histogram content
q2 <- mlm.dat %>%
  ggplot( aes(x=content, fill=affpoldummy)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")


# volin plot

ggplot(mlm.dat, 
       aes(x = logic, 
           y = aff.pol, 
           color = target)) +
  geom_violin(trim=TRUE)+
  theme_cowplot(font_size = 16) +
  facet_grid(cols = vars(target), rows = vars(affpoldummy))

################################### # are people more similar to thier ingroup than outgroup?
testc <- lmer(content ~  
                outgroupdummy+
                countrydummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat.fin)

tests <- lmer(logic ~  
                outgroupdummy+
                countrydummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat.fin)

summary(testc)
summary(tests)

effectsize::standardize_parameters(testc)
effectsize::standardize_parameters(tests)

################################### H1 test
emtyh1 <- lmer(aff.pol ~ 1 + (1 | id), data=mlm.dat.fin)
compute_icc(emtyh1)

h1test <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat.fin)


h1test.st1 <- lmer(aff.pol ~  logic_c + 
                     outgroupdummy+
                     logic_c*outgroupdummy + 
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

h1test.st2 <- lmer(aff.pol ~  + content_c+
                     outgroupdummy+
                     content_c*outgroupdummy +
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

summary(h1test)
summary(h1test.st1)
summary(h1test.st2)

#standardize

ef1 <- effectsize::standardize_parameters(h1test, method = "refit")
ef2 <- effectsize::standardize_parameters(h1test.st1, method = "refit")
ef3 <- effectsize::standardize_parameters(h1test.st2, method = "refit")

##### test VIP

car::vif(h1test)

#####  test 
test(emtrends(h1test, ~ outgroupdummy, var="logic_c"))
test(emtrends(h1test, ~ outgroupdummy, var="content_c"))

#####  power
powers <- powerSim(fit = h1test, test = fixed(xname= "logic_c", method = "t"))
powerc <- powerSim(fit = h1test, test = fixed(xname= "content_c", method = "t"))

### compare betas

linearHypothesis(h1test, "content_c - logic_c= 0")
linearHypothesis(h1test, "content_c:outgroupdummy - logic_c:outgroupdummy = 0")

####################### table

library(stargazer)
detach(package:lmerTest)

stargazer(h1test, h1test.st1, h1test.st2, title="Table 2. Multilevel regression model predicting group liking, reporting unstandardized beta (95% confidence interval), and t-value (Study 2)",   
          dep.var.labels= "Group liking",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)", "Outgroup dummy", "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"),
          column.labels = c("Model 2: Full", "Model 2a: Structure", "Model 2b: Content"), 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          model.numbers = FALSE,
          intercept.bottom = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 study2 2110.htm", report=("vcst*"),ci = TRUE, star.cutoffs = c(0.001, 0.0001, 0.00001))


################################################### Supplementary materials

# add controls

h1test.c <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                  gender + edu_c + age_c + inc_c +
                  logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 (1 | id), data=mlm.dat.fin)


h1test.st1.c <- lmer(aff.pol ~  logic_c + 
                     outgroupdummy+
                       gender + edu_c + age_c + inc_c +
                       logic_c*outgroupdummy + 
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

h1test.st2.c <- lmer(aff.pol ~  + content_c+
                     outgroupdummy+
                       gender + edu_c + age_c + inc_c +
                       content_c*outgroupdummy +
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

# table

stargazer(h1test.c, h1test.st1.c, h1test.st2.c, title="Table 2. Multilevel regression model predicting group liking from political identity, belief system content and structure similarity, reporting unstandardized beta (95% confidence interval), and t-value (Study 2)",   
          dep.var.labels= "Group liking",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)", "Outgroup dummy", 
              "Gender", "Education", "Age", "Income",
              "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"),
          column.labels = c("Model 2: Full", "Model 2a: Structure", "Model 2b: Content"), 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          model.numbers = FALSE,
          intercept.bottom = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 study2 controls 2410.htm", report=("vcst*"),ci = TRUE, star.cutoffs = c(0.001, 0.0001, 0.00001))



#add quadratic term

h1test.q <- lmer(aff.pol ~  logic_c + content_c+ logic_q+ content_q+
                 outgroupdummy+
                   logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                (1 | id), data=mlm.dat.fin)

h1test.q.c <- lmer(aff.pol ~  logic_c + content_c+ logic_q+ content_q+
                   outgroupdummy+
                   gender + edu_c + age_c + inc_c +
                   logic_c*outgroupdummy + content_c*outgroupdummy + 
                   countrydummy+ partydummy+
                   #                edu_c + age_c + inc_c +ethnicdummy+
                   (1 | id), data=mlm.dat.fin)

stargazer(h1test.q, h1test.q.c, title="Table 2. Multilevel regression model predicting group liking from political identity, belief system content and structure similarity and quadratic similarity effects, reporting unstandardized beta (95% confidence interval), and t-value (Study 2)",   
          dep.var.labels= "Group liking",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)", "Outgroup dummy", 
              "BS structure similarity (quadratic)", "BS content similarity (quadratic)",
              "Gender", "Education", "Age", "Income",
              "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"),
          column.labels = c("Model 2", "Model 2: Controls"), 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          model.numbers = FALSE,
          intercept.bottom = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 study2 quadratic 2110.htm", report=("vcst*"),ci = TRUE, star.cutoffs = c(0.001, 0.0001, 0.00001))

# Check results in Europe

eunames <- c("Austria", "Bulgaria", "Czech Republic", "Finland", "France", "Germany", "Great Britain", "Greece",
             "Iceland", "Latvia", "Montenegro", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Sweden",
             "Switzerland") 

mlm.dat.eu <- mlm.dat.fin %>% 
  filter(country %in% eunames) 
  
  mlm.dat.eu %>% 
  group_by(country) %>% 
  summarise(n = n())

# rerum main models  
  h1test.eu <- lmer(aff.pol ~  logic_c + content_c+ 
                   outgroupdummy+
                   logic_c*outgroupdummy + content_c*outgroupdummy + 
                   countrydummy+ partydummy+
                   #                edu_c + age_c + inc_c +ethnicdummy+
                   (1 | id), data=mlm.dat.eu)
  
  
  h1test.st1.eu <- lmer(aff.pol ~  logic_c + 
                       outgroupdummy+
                       logic_c*outgroupdummy + 
                       countrydummy+ partydummy+
                       (1 | id), data=mlm.dat.eu
  )
  
  h1test.st2.eu <- lmer(aff.pol ~  + content_c+
                       outgroupdummy+
                       content_c*outgroupdummy +
                       countrydummy+ partydummy+
                       (1 | id), data=mlm.dat.eu
  )
  
  test(emtrends(h1test.eu, ~ outgroupdummy, var="logic_c"))
  test(emtrends(h1test.eu, ~ outgroupdummy, var="content_c"))
  
  ### table
  
  stargazer(h1test.eu, h1test.st1.eu, h1test.st2.eu, title="Table 1. Multilevel regression model predicting group liking from political identity, belief system content and structure similarity in European countries only, reporting unstandardized beta (95% confidence interval), and t-value (Study 2)",   
            dep.var.labels= "Group liking",
            covariate.labels = 
              c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)", "Outgroup dummy", "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"),
            column.labels = c("Model 2: Full", "Model 2a: Structure", "Model 2b: Content"), 
            omit = c("partydummy" , "countrydummy"),
            omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
            model.numbers = FALSE,
            intercept.bottom = FALSE,
            digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 study2 EU only 2410.htm", report=("vcst*"),ci = TRUE, star.cutoffs = c(0.001, 0.0001, 0.00001))
  

# check content by step
h1test.st2.1 <- lmer(aff.pol ~  + content_c+
                      countrydummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

h1test.st2.2 <- lmer(aff.pol ~  + content_c+
                       outgroupdummy+
                       countrydummy+ partydummy+
                       (1 | id), data=mlm.dat.fin
)

h1test.st2.3 <- lmer(aff.pol ~  + content_c+logic_c + 
                       outgroupdummy+
                       countrydummy+ partydummy+
                       (1 | id), data=mlm.dat.fin
)
h1test.st2.4 <- lmer(aff.pol ~  + content_c+logic_c + 
                       outgroupdummy+
                       countrydummy+ partydummy+
                       logic_c*outgroupdummy + 
                       (1 | id), data=mlm.dat.fin
)

h1test.st2.21 <- lmer(aff.pol ~  + content_c+
                                        countrydummy+ partydummy+
                       (1 | id), data=mlm.dat.fin
)

summary(h1test.st2.1)
summary(h1test.st2.2)
summary(h1test.st2.3)
summary(h1test.st2.4)

summary(emtyh1)
summary(h1test)
summary(h1test.st1)
summary(h1test.st2)


#standardize coefficient




#######################  simple slopes

# make simple slopes at mean lower and upper quartiles
ss1 <- interactions::sim_slopes(model=h1test  , pred=outgroupdummy, modx=logic_c     , modx.values = c(summary(mlm.dat.fin$logic_c)  [[2]]     , summary(mlm.dat.fin$logic_c) [[4]]     , summary(mlm.dat.fin$logic_c)   [[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper
ss2 <- interactions::sim_slopes(model=h1test  , pred=outgroupdummy, modx=content_c   , modx.values = c(summary(mlm.dat.fin$content_c)[[2]]     , summary(mlm.dat.fin$content_c)[[4]]     , summary(mlm.dat.fin$content_c)[[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper
#ss3 <- interactions::sim_slopes(model=h1test  , pred=outgroupdummy, modx=logic_c.q   , modx.values = c(summary(mlm.dat$logic_c.q)  [[2]]   , summary(mlm.dat$logic_c.q) [[4]]     , summary(mlm.dat$logic_c)   [[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper

# plot
ssplotdata <- tibble(models = rep(c("Model 2: Structure", "Model 2: Content"), times = c(3,3)),
                     coeff  = factor(rep(c("Lower quartile", "Mean", "Upper quartile"), 2)),
                     Estimate =c(ss1$slopes[[2]], ss2$slopes[[2]]),
                     lo95 = c(ss1$slopes[[4]], ss2$slopes[[4]]),
                     hi95 = c(ss1$slopes[[5]], ss2$slopes[[5]])
)

level_order <- c("Upper quartile", "Mean", "Lower quartile")

s1model1 <- ggplot(ssplotdata, aes(x = factor(coeff, level = level_order), y = Estimate)) +
  geom_pointrange(stat = "identity", aes(ymin = lo95, ymax = hi95), size = 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_y_continuous(limits = c(-5,0), breaks = c(-5,-4,-3,-2,-1, 0)) +
  facet_grid(~models) + 
  theme_cowplot(font_size = 16) +
  labs(y = "Estimate of ingroup - outgroup difference") +
  theme(axis.title.y = element_blank()) +
  coord_flip()


#tiff("simpleslopesmodel1 study 2 2110.tif", width = 750, height = 250)
s1model1
#dev.off()

