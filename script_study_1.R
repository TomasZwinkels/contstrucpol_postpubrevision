################################### packages

# install.packages("psych")

library(haven)
library(rio)
library(tidyverse)
library(corrr) 
library(sjlabelled)
library(Rfast)
library(lme4)
library(lmerTest)
library(cowplot)
library(pysch)

###################################import data

#raw data
data <- read_dta('Data/Main survey/UNDPOLAR data_full_V9.dta')
data <- as.data.frame(data)

#similarity data
#newdf <- read.csv("Data/Main survey/allcountriesparties selected.csv")
#logic_cont <- read.csv("Data/Main survey/logic_cont.csv")

#mlm data ready to go
#mlm.dat <- readRDS("Data/Main survey/mlm.dat.s1new.Rda")

################################### Data overview
#str(data, list.len=ncol(data))

################################### check unique id

data %>%
  group_by(Country) %>%
  summarise(unique = table(duplicated(id)))

################################### clean data

# recoding all belief system items so they range from 0 to 1

dat_clean <- data       %>% mutate(country             = Country,
                                   econ_attitudes_1r   = egalitarianism_right/10,            		#recodes data to be between 0 and 1: zi = (xi – min(x)) / (max(x) – min(x)) i.e., x-1/(10-1)
                                   econ_attitudes_2r   = econ_nationalization_right/10,            	#recodes data to be between 0 and 1: zi = (xi – min(x)) / (max(x) – min(x))
                                   econ_attitudes_3r   = gov_resp_right/10,            				#recodes data to be between 0 and 1: zi = (xi – min(x)) / (max(x) – min(x))
                                   cult_attitudes_1r   = (immigration_right-1)/4,             		#recodes data to be between 0 and 1
                                   cult_attitudes_2r   = (lgbt_rights_right-1)/4,
                                   cult_attitudes_3r   = (gender_equality_right-1)/4,             	#recodes data to be between 0 and 1
                                   newpol_attitudes_1r = euroscepticism_right/10,          			#recodes data to be between 0 and 1
                                   newpol_attitudes_2r   = (climate_change_right-1)/4,           	#recodes data to be between 0 and 1
                                   newpol_attitudes_3r   = (polcorrectness_right -1)/4,         	#recodes data to be between 0 and 1
                                   ideology              = lrposition/10,
                                   group = ifelse(party_pref>1, party_pref, NA),					# Tomas: Q: what is 'party_pref' here?, A: Political party preference, 1 is I do not feel close, all others are valid values.
                                   genderdummy = ifelse(gender==1,-1,                              	# makes controls as per pre-reg
                                                        ifelse(gender ==2,1,NA)),                  	# makes controls as per pre-reg
                                   ethnicdummy= ifelse(ethnicity_rec==0,-1,ethnicity_rec),        	# makes controls as per pre-reg
                                   edu_c = highest_diploma - median(highest_diploma, na.rm=TRUE),  	# makes controls as per pre-reg
                                   age_c = age    - mean(age, na.rm = TRUE),                   		# makes controls as per pre-reg
                                   inc_c = hhincome - mean(hhincome, na.rm = TRUE),
                                   partyid_c = party_id - mean(party_id, na.rm = TRUE))        		# makes controls as per pre-reg


# some inspections

	summary(dat_clean$econ_attitudes_1r)
	hist(dat_clean$econ_attitudes_1r)
	
	labels(dat_clean$party_pref)


################################### select sample

options(dplyr.print_max = 150)

dat_clean    %>% dplyr::group_by(party_pref) %>%  summarise(n = n())
dat_clean    %>% dplyr::group_by(country) %>%  summarise(n = n())

# remove poland from sample
dat_poloandout <- dat_clean    %>% 
  dplyr::filter(country <9)

nrow(dat_poloandout) # full sample in manuscript
dat_poloandout    %>% dplyr::group_by(party_pref) %>%  summarise(n = n()) # gives smple who support no party & missing
nrow(dat_poloandout)-nrow(dat_select)-8-2925 # pps who supported a party not one of the 4 measured

# As per pre-reg remove pps for whom n>= 2 beleif system items are absent
table(rowSums(is.na(dat_clean[,c("ideology", "econ_attitudes_1r", "econ_attitudes_2r", "econ_attitudes_3r", "cult_attitudes_1r", "cult_attitudes_2r", "cult_attitudes_3r", "newpol_attitudes_1r", "newpol_attitudes_2r", "newpol_attitudes_3r")])))
#shows there is no missingess on the attiude variables so noo need to select


# note select people with 1 of the 4 possible party preferences that were also meausred as the targets of the aff_polarization items, polish parties not included as poland measurements are different 
dat_select <- dat_clean    %>% 
                          dplyr::filter(party_pref ==100|party_pref ==18|party_pref ==52|party_pref ==38|party_pref ==14|party_pref ==57|party_pref ==7|party_pref ==70|party_pref ==82|
                                        party_pref ==99 |party_pref ==19|party_pref ==50|party_pref ==36|party_pref ==13|party_pref ==58|party_pref ==5|party_pref ==65|party_pref ==83|
                                        party_pref ==108|party_pref ==23|party_pref ==54|party_pref ==46|party_pref ==15|party_pref ==56|party_pref ==6|party_pref ==69|party_pref ==85|
                                                         party_pref ==25|party_pref ==51|party_pref ==35|                party_pref ==62|party_pref ==8|party_pref ==67|party_pref ==84)     %>%             
                                  dplyr::select(id, country,  
                                  econ_attitudes_1r, econ_attitudes_2r, econ_attitudes_3r,
                                  cult_attitudes_1r, cult_attitudes_2r, cult_attitudes_3r, 
                                  newpol_attitudes_1r, newpol_attitudes_2r, newpol_attitudes_3r,
                                  ideology,
                                  aff_polarization_1, aff_polarization_2, aff_polarization_3, aff_polarization_4,
                                  party_pref, group, party_id, 
                                  gender, age, hhincome, highest_diploma, genderdummy, age_c, edu_c, inc_c, ethnicdummy)  

# Tomas, OK, so I think this should be equivalent to something like.. Is it possible to get this from the data? - seems not, seems like is from codebook, but the result can be checked

	# in Belgium for example, we should have 8 (it turns out 7) parties.
		# Belgium (French): Le Parti Socialiste supporters = 14
		# Belgium (Dutch): Vooruit supporters = 7
		
		# Belgium (French): Le Mouvement Réformateur supporters = 13
		# Belgium (Dutch): Open Vlaamse Liberalen en Democraten = 5
		
		# Belgium (French): Le parti du Travail de Belgique supporters = 15
		# Belgium (Dutch): Partij van de Arbeid van België supporters = 6

		# Belgium (French): Chez Nous supporters = not measured?
		# Belgium (Dutch): Vlaams Belang supporters = 8
		
		# are these indeed the ones?
		table(dat_select[which(dat_select$country == 1),]$group) # 7 parties included, all correct
	
	# in Denmark
		# Denmark: Socialdemokratiet supporters = 18
		# Denmark: Venstre supporters - 19
		# Denmark: Enhedslisten supporters - 23
		# Denmark: Nye Borgerlige supporters - 25
		table(dat_select[which(dat_select$country == 2),]$group) # 4 parties included, all correct
		
	# in France
		# France: Le Parti Socialiste supporters - 38
		# France: Les Républicains supporters - 36
		# France: La France Insoumise supporters - 46
		# France: Le Rassemblement National - 35
		table(dat_select[which(dat_select$country == 3),]$group) # 4 parties included, all correct

	# in Greece
		# Greece: το ΚΙΝ.ΑΛ supporters - 52
		# Greece: τη Νέα Δημοκρατία supporters - 50
		# Greece: την Ελληνική Λύση supporters - 54
		# Greece: τον ΣΥ.ΡΙΖ.Α. supporters - 51
		table(dat_select[which(dat_select$country == 4),]$group) # 4 parties included, all correct
		
	# in Hungary
		# Fidesz supporters - 57
		# Jobbik supporters - 58
		# Demokratikus Koalíció supporters - 56
		# Párbeszéd Magyarországért supporters - 62
		table(dat_select[which(dat_select$country == 5),]$group) # 4 parties included, all correct
	
	# in the Netherlands
		# Partij van de Arbeid supporters = 70 # is included
		# Volkspartij voor Vrijheid en Democratie = 65 # is included
		# Socialistische Partij supporters = 69 # NOT included?! -- hmm, this really is what it says in the codebook #Tomas 
		# Partij voor de Vrijheid supporters = 67 # NOT included?! -- hmm, this really is what it says in the codebook #Tomas
		table(dat_select[which(dat_select$country == 6),]$group) # NEW: matches the codebook I have now # OLD 4 parties included, but two of them seem incorrect? 68 (CDA) and 66 (D66)
		
	# in Spain
		# Spain: Partido Socialista Obrero Español - not listed?! - yes, they are but under NL - 82
		# Spain: Partido Popular supporters - not listed?!  - yes, they are but under NL - 83
		# Spain: Unidas Podemos supporters - 85
		# Spain: VOX supporters - 84
		table(dat_select[which(dat_select$country == 7),]$group) # looks good
		
	# in the UK
		# Labour Party supporters - 100
		# Conservative party supporters - 99
		# SNP (Scottish National Party) supporters - 108
		# UKIP (UK Independence Party) - 102 # UKIP not on purposes? - probably not enough party supporters?
		table(dat_select[which(dat_select$country == 8),]$group) # UKIP is missing here?! Why?
		
		
## Tomas: are their any pre-registered exclusions like in study 2 that need to be taken into account


################################ sample descriptives
summary(dat_select$age, na.rm=TRUE)
sd(dat_select$age, na.rm=TRUE)

#ethnicity
table(dat_select$ethnicity_rec)

#main variables summary library(pysch) # does not run, package issue, will check later.
describe(dat_select[,c("ideology", "econ_attitudes_1r", "econ_attitudes_2r", "econ_attitudes_3r", "cult_attitudes_1r", "cult_attitudes_2r", "cult_attitudes_3r", "newpol_attitudes_1r", "newpol_attitudes_2r", "newpol_attitudes_3r",
                      "aff_polarization_1", "aff_polarization_2", "aff_polarization_3", "aff_polarization_4")])

################################# Beleif system similarity

################################## loop
looplist1 <- list()

for (i in 1:length(table(dat_select$country))) {
  print(i)
  dat_selectloop <- dat_select %>% dplyr::filter(country == i)
  
  transposed_supp <- dat_selectloop %>% 
    dplyr::select(ideology, econ_attitudes_1r, econ_attitudes_2r, econ_attitudes_3r, cult_attitudes_1r, cult_attitudes_2r, cult_attitudes_3r, newpol_attitudes_1r, newpol_attitudes_2r, newpol_attitudes_3r) %>% # put the column of the attitude/issue items here
    t(.) %>% # transposes the whole thing and puts it on its side
    `colnames<-`(dat_selectloop$id) %>% # makes the column names the participant ID (so use whatever participant ID is in your data)
    as.data.frame()
  
  attitude_compare <- correlate(transposed_supp) %>% # calculates cors
    #  shave() %>% # removes half the diag (to avoid repeats)
    stretch(na.rm = FALSE) %>% # makes it a long data from with two columsn, one for X and one for Y
    drop_na(r) %>% # remove missing values
    dplyr::rename(agreement = r) %>% # calls the raw correlation "agreement"
    dplyr::mutate(logic = abs(agreement)) %>% # BS structure/logic similarity by taking absolute value
    dplyr::select(x, y, agreement, logic) # selects key columns. X and Y will have the participant IDs
  
  
  allsimdata <- as.data.frame(attitude_compare)
  allsimdata$content.sim <- colMeans(abs(transposed_supp[ , allsimdata[,1] ] - transposed_supp[ , allsimdata[,2] ] ),  na.rm = TRUE)
  allsimdata <- cbind(country= get_labels(dat_select$country)[i], allsimdata)
  
  
  looplist1[[i]] <- allsimdata
  names(looplist1)[i] <- get_labels(dat_select$country)[i]

  }
	
newdf <- bind_rows(looplist1, .id = NULL)

# OK, so what do we want to see here?

	head(newdf)
	
	# senseable correlation values
	hist(newdf$agreement) # can be negative
	hist(newdf$logic) # cannot be negative
	
	# missingness not clearly patterned per country - result = no missingness at all.
	table(newdf$country)
	table(newdf$country,is.na(newdf$agreement))
	table(newdf$country,is.na(newdf$logic))

#write.csv(newdf,"Data/Main survey/allcountriesparties selected.csv", row.names = FALSE)

# now organize output by party supported -- Tomas: Q: what is 'group' here? A: that is party_pref

head(dat_select)

allsimdata1 <- merge(x = newdf,  y = dat_select[ , c("id", "group")], by.x = "x", by.y = "id"  ,all.x=TRUE)
head(allsimdata1)

allsimdata2 <- merge(x = allsimdata1,  y = dat_select[ , c("id", "group")], by.x = "y", by.y = "id",all.x=TRUE)
head(allsimdata2)

# filter to selct only those who support a party in aff_pol1-4

finalsimdata1 <- allsimdata2 %>% 
                        mutate(match         = paste(group.x,group.y,sep="_"),
                               ingroup       = ifelse(group.x == group.y, "ingroup","outgroup")
                                                   
                               )
							   
head(finalsimdata1)

# lets have a look at all combos with grepl('14$', finalsimdata1$match)

table(finalsimdata1[which(grepl('14$', finalsimdata1$match)),]$match) # answer is yes, there are cross langauge matches.. why?!

#mathcing party supporters pairings with aff_pol items so that only those who end with one of the options measured; select pps who pol_pref is the same as the aff-pol target e.g., aff_pol1 in UK = 100, so we select all that end with 100
#BUT for belgium I do precise matches otherwsie french and belgium dutch pairings are offered? Tomas: Q: is this true?,  let check above. A: yes this is true, the reason is that everybody in each country is compared with everybody, 
# so also across the languages in BE, this while for party 14 for example, only the comparison with 13 and 15 is relevant - so that needs to be - as it is below - specified.
# group.y is ones own party, group.y is the party of the other. We are here just specifying where to look! - when you get aff_pol_1 you should be in first row, I checked all these now manually, and expect for the same two Dutch parties. These are all as they should be.

finalsimdata <- finalsimdata1 %>% 
                          mutate( target = ifelse(grepl('100$', finalsimdata1$match)|grepl('18$', finalsimdata1$match)|grepl('52$', finalsimdata1$match)|grepl('38$', finalsimdata1$match)|grepl('^14_14$', finalsimdata1$match)|grepl('^13_14$', finalsimdata1$match)|grepl('^15_14$', finalsimdata1$match)|grepl('57$', finalsimdata1$match)|grepl('^7_7$', finalsimdata1$match)|grepl('^5_7$', finalsimdata1$match)|grepl('^6_7$', finalsimdata1$match)|grepl('^8_7$', finalsimdata1$match)|grepl('70$', finalsimdata1$match)|grepl('82$', finalsimdata1$match),"aff_polarization_1",
                                                  ifelse(grepl('99$', finalsimdata1$match)|grepl('19$', finalsimdata1$match)|grepl('50$', finalsimdata1$match)|grepl('36$', finalsimdata1$match)|grepl('^13_13$', finalsimdata1$match)|grepl('^14_13$', finalsimdata1$match)|grepl('^15_13$', finalsimdata1$match)|grepl('58$', finalsimdata1$match)|grepl('^5_5$', finalsimdata1$match)|grepl('^7_5$', finalsimdata1$match)|grepl('^6_5$', finalsimdata1$match)|grepl('^8_5$', finalsimdata1$match)|grepl('65$', finalsimdata1$match)|grepl('83$', finalsimdata1$match),"aff_polarization_2",
                                                         ifelse(grepl('108$', finalsimdata1$match)|grepl('23$', finalsimdata1$match)|grepl('54$', finalsimdata1$match)|grepl('46$', finalsimdata1$match)|grepl('^15_15$', finalsimdata1$match)|grepl('^14_15$', finalsimdata1$match)|grepl('^13_15$', finalsimdata1$match)|grepl('56$', finalsimdata1$match)|grepl('^6_6$', finalsimdata1$match)|grepl('^7_6$', finalsimdata1$match)|grepl('^5_6$', finalsimdata1$match)|grepl('^8_6$', finalsimdata1$match)|grepl('67$', finalsimdata1$match)|grepl('85$', finalsimdata1$match),"aff_polarization_3",
                                                                ifelse(grepl('25$', finalsimdata1$match)|grepl('51$', finalsimdata1$match)|grepl('35$', finalsimdata1$match)|grepl('62$', finalsimdata1$match)|grepl('^8_8$', finalsimdata1$match)|grepl('^7_8$', finalsimdata1$match)|grepl('^5_8$', finalsimdata1$match)|grepl('^6_8$', finalsimdata1$match)|grepl('67$', finalsimdata1$match)|grepl('84$', finalsimdata1$match),"aff_polarization_4",NA))))
                                  
                          )

# now  make a variable that takes the pps mean over ingroup matches and outgroup matches

logic <- aggregate(logic       ~x+target, mean, data = finalsimdata, na.action = na.omit)

content <- aggregate(content.sim ~x+target, mean, data = finalsimdata, na.action = na.omit)
head(content)

table(is.na(content$content.sim)) # OK, so we still have them all here.

logic_cont <- merge(logic, content, by = c('x','target'))
logic_cont <- logic_cont %>% rename(  id = x,
                                      contentdif = content.sim )

head(logic_cont)
table(is.na(logic_cont$contentdif)) # OK, and here
                                   ) 
#write.csv(logic_cont,"Data/Main survey/logic_cont.csv", row.names = FALSE)
#
#logic_conttest <- merge(logic_cont, dat_select[,c("id", "country")], by.x = "id", by.y = "id",  all.x=TRUE)
#table(logic_conttest$country, logic_conttest$target)
#
## histogram logic
#p <- logic %>%
#  ggplot( aes(x=logic, fill=target)) +
#  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
# # scale_fill_manual(values=c("#69b3a2", "#404080")) +
#  labs(fill="")
#
## histogram content
#q <- content %>%
#  ggplot( aes(x=content.sim, fill=target)) +
#  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
##  scale_fill_manual(values=c("#69b3a2", "#404080")) +
#  labs(fill="")
#
################################### convert data to long format for MLM
mlm.datv1 <- pivot_longer(dat_select, aff_polarization_1:aff_polarization_4,  names_to = "target", values_to = "aff.pol") # so this is where we get the long data, one row for each version of aff_polarization_1-4
mlm.datv1 <- mlm.datv1 %>% mutate(targetorig = target)
mlm.datv1 <- as.data.frame(mlm.datv1)

head(mlm.datv1) # long dataframe, one row per aff_polarization_1-4 value, for all but the belief system vars.
head(logic_cont) # data frame that contains for each row the content and logic value with the specific party that in that country got this postion on the aff_polarization_1-4 position.

#add content and structure variables in
mlm.dat <- merge(mlm.datv1, logic_cont, by = c('id','target'), all.x = TRUE) # Tomas: Q: does this merge make sense to me? A: Yes, it does, see row 318 and 319 for an explanation.

head(mlm.dat)
table(is.na(mlm.dat$contentdif)) # OK, here we lost 806 cases, look below for an explanation why.

mlm.contmiss <- mlm.dat[which(is.na(mlm.dat$content)),]
head(mlm.contmiss)

# lets see, is there a value for all IDS in logic_cont?

length(unique(logic_cont$id))
length(unique(mlm.datv1$id)) # OK, so there are 7 more people here, but maybe also different ones?

# one way
logic_cont[which(!(logic_cont$id %in% mlm.datv1$id)),] # OK, so all in logic_cont are in mlm.datv1

# the other way
mlm.datv1[which(!(mlm.datv1$id %in% logic_cont$id)),] # but these cases are not in logic_cont - what all these cases have in common is that everbody scores exactly 
													  # the same on all items. I think it is fair enough that these people are excluded.
													  # we can consider mentioning that explicitly, but I think the specififcation of the variable construction implies this. 


as.data.frame(mlm.dat)[0:20,]

# Is assume we we get the same missingness on structure?

table(is.na(mlm.dat$logic)) # yes, indeed

#mutate recodea content similarity so that greater similarity is larger values, and same = 1, most different = 0
# .c variables center

# below, UKIP is mentioned again, but I think they are out of the data already because of size!
# OK, so I don't understand for 100% yet that it makes sense how ingroup is constructed here.. 

mlm.dat <- mlm.dat %>% mutate(affpoldummy   =ifelse(                                target == "aff_polarization_1"&(group ==100|group ==18|group ==52|group ==38|group ==14|group ==57|group ==7|group ==70|group ==82),"ingroup",
                                                    ifelse(                         target == "aff_polarization_2"&(group ==99| group ==19|group ==50|group ==36|group ==13|group ==58|group ==5|group ==65|group ==83),"ingroup", 
                                                                ifelse(             target == "aff_polarization_3"&(group ==108|group ==23|group ==54|group ==46|group ==15|group ==56|group ==6|group ==69|group ==85),"ingroup",
                                                                             ifelse(target == "aff_polarization_4"&(group ==102|group ==25|group ==51|group ==35|           group ==62|group ==8|group ==67|group ==84),"ingroup","outgroup")))),
                              outgroupdummy.num = ifelse(affpoldummy == "ingroup",0,
                                                    ifelse(affpoldummy == "outgroup",1,NA)),
                              content     = 1-contentdif,
                              logic_c   = mlm.dat$logic-mean(mlm.dat$logic,na.rm=TRUE),
                              logic_c_q = logic_c*logic_c,
                              partyid_c= mlm.dat$party_id-mean(mlm.dat$party_id,na.rm=TRUE),
                              ingroup     = as.factor(affpoldummy),
                              partydummy  = as.factor(party_pref),
                              countrydummy= as.factor(country),
                              populist   = ifelse((target == "aff_polarization_3" |target == "aff_polarization_4"), "populist", "mainstream")
                              )



mlm.dat <- mlm.dat %>% mutate(content_c= mlm.dat$content-mean(mlm.dat$content,na.rm=TRUE),
                              popdummy    = ifelse(populist == "mainstream",0,
                                                   ifelse(populist == "populist",1,NA)),
                              logic_q     = logic_c^2,
                              content_q= (mlm.dat$content-mean(mlm.dat$content,na.rm=TRUE))^2,
                              outgroupdummy = as.factor(outgroupdummy.num),
                              aff.pol.num = as.numeric(aff.pol))

#saveRDS(mlm.dat, file = "Data/Main survey/mlm.dat.s1new.Rda")
#mlm.datloadtest <- readRDS("Data/Main survey/mlm.dat.s1new.Rda")

## and some instructions on the final data

hist(mlm.dat$content)
table(is.na(mlm.dat$content)) # Q: what are these 806 cases? A: see linme 313-338, these are cases where the subjects answered the exact same value to all belief system items.



mean(mlm.dat$content)
hist(mlm.dat$content_c)

hist(mlm.dat$logic)
hist(mlm.dat$logic_c)




################################### descritpives

#cor set

cordat <- mlm.dat %>% 
  dplyr::select(aff_pol, logic_c, content_c, outgroupdummy)
cor(cordat, use = "pairwise.complete.obs")

cor(mlm.dat$logic, mlm.dat$content, use = "pairwise.complete.obs")
cor.test(mlm.dat$logic, mlm.dat$content, method = "pearson")

# get subsamples
subsamp.in <- subset(mlm.dat, mlm.dat$affpoldummy == "ingroup")
subsamp.ot <- subset(mlm.dat, mlm.dat$affpoldummy == "outgroup")
subsamp.t1 <- subset(mlm.dat, (mlm.dat$affpoldummy == "outgroup" & mlm.dat$target == "aff_polarization_1"))
subsamp.t2 <- subset(mlm.dat, (mlm.dat$affpoldummy == "outgroup" & mlm.dat$target == "aff_polarization_2"))
subsamp.t3 <- subset(mlm.dat, (mlm.dat$affpoldummy == "outgroup" & mlm.dat$target == "aff_polarization_3"))
subsamp.t4 <- subset(mlm.dat, (mlm.dat$affpoldummy == "outgroup" & mlm.dat$target == "aff_polarization_4"))

# correlations for structure/logic
cor.test(subsamp.in$aff.pol, subsamp.in$logic, method = "pearson")
cor.test(subsamp.ot$aff.pol, subsamp.ot$logic, method = "pearson")

mean(c(
      cor.test(subsamp.t1$aff.pol, subsamp.t1$logic, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t2$aff.pol, subsamp.t2$logic, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t3$aff.pol, subsamp.t3$logic, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t4$aff.pol, subsamp.t4$logic, method = "pearson")$estimate[[1]]))

#correlations for content
cor.test(subsamp.in$aff.pol, subsamp.in$content, method = "pearson")
cor.test(subsamp.ot$aff.pol, subsamp.ot$content, method = "pearson")


mean(c(
      cor.test(subsamp.t1$aff.pol, subsamp.t1$content, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t2$aff.pol, subsamp.t2$content, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t3$aff.pol, subsamp.t3$content, method = "pearson")$estimate[[1]],
      cor.test(subsamp.t4$aff.pol, subsamp.t4$content, method = "pearson")$estimate[[1]]))
     

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

# scatter plot
ggplot(mlm.dat, 
       aes(x = logic, 
           y = aff.pol)) +
  geom_point(size = 0.5)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_cowplot(font_size = 16) +
  facet_grid(cols = vars(target), rows = vars(affpoldummy))

ggplot(mlm.dat, 
       aes(x = content, 
           y = aff.pol)) +
  geom_point(size = 0.5)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  theme_cowplot(font_size = 16) +
  facet_grid(cols = vars(target), rows = vars(affpoldummy))

################################### MLM functions

# Function to compute the intraclass correlation coefficient (ICC)
compute_icc <- function(lmer_object){
  var_dat <- lmer_object %>% VarCorr %>% as.data.frame
  icc <- var_dat$vcov[1]/(var_dat$vcov[1]+var_dat$vcov[2])
  return(icc)
}

################################### # are people more similar to thier ingroup than outgroup?
testc <- lmer(content ~  
                outgroupdummy+
                countrydummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat)

tests <- lmer(logic ~  
                outgroupdummy+
                countrydummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat)

summary(testc)
summary(tests)

effectsize::standardize_parameters(testc)
effectsize::standardize_parameters(tests)

###################################################################### Main analyses
################################### H1 test
emtyh1 <- lmer(aff.pol ~ 1 + (1 | id), data=mlm.dat)
compute_icc(emtyh1)

# Main models
h1test <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)
# Main models
h1test <- lmer(aff_pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)

mlm.dat.testimport

#step 1
h1test.st1 <- lmer(aff.pol ~  logic_c + 
                     outgroupdummy+
                     logic_c*outgroupdummy + 
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat
)

#step 2
h1test.st2 <- lmer(aff.pol ~  + content_c+
                     outgroupdummy+
                     content_c*outgroupdummy +
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat
)


summary(emtyh1)
summary(h1test)
summary(h1test.st1)
summary(h1test.st2)

anova(h1test,h1testq)

#standardize

effectsize::standardize_parameters(h1test, method = "refit")
effectsize::standardize_parameters(h1test.st1, method = "refit")
effectsize::standardize_parameters(h1test.st2, method = "refit")

##### test VIF

car::vif(h1test)
h1testm <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
     #            logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)
car::vif(h1testm)
car::vif(h1test.st2)


#_use the Kenward-Roger or Satterthwaite approximations

##### simple slopes draft for outgroup/ingroup slopes
library(emmeans)
# test
summary(mlm.dat$content_c)
sd(mlm.dat$content_c, na.rm = TRUE)
hist(mlm.dat$logic_c)

summary(mlm.dat$logic_c)
sd(mlm.dat$logic_c, na.rm = TRUE)
hist(mlm.dat$logic_c)


test(emtrends(h1test, ~ outgroupdummy, var="logic_c"))
test(emtrends(h1test, ~ outgroupdummy, var="content_c"))

emtrends(h1test, pairwise ~ outgroupdummy, var="logic_c", pbkrtest.limit = 15802)
emtrends(h1test, pairwise ~ outgroupdummy, var="content_c")

#plot
emmip(h1test, outgroupdummy~logic_c, CIs = TRUE, at = list(logic_c = c(summary(mlm.dat$logic_c)[[1]], 0, summary(mlm.dat$logic_c)[[6]]))
) +
  theme_cowplot(font_size = 16) + xlab('Belief system structure similarity') + labs(color='Group', text = "D") +
  scale_color_manual(labels = c("Ingroup", "Outgroup"), values = c("blue", "red"))  

emmip(h1test, outgroupdummy~content_c, CIs = TRUE, at = list(content_c = c(summary(mlm.dat$content_c)[[1]], 0, summary(mlm.dat$content_c)[[6]]))
) +
  theme_cowplot(font_size = 16) + xlab('Content similarity') + labs(color='Group', text = "D") +
  scale_color_manual(labels = c("Ingroup", "Outgroup"), values = c("blue", "red")) 

#  simple slopes looking at affective polarization
# Main models
h1testv1 <- lmer(aff.pol.num ~  logic_c + content_c+ 
                 outgroupdummy.num+
                 logic_c*outgroupdummy.num + content_c*outgroupdummy.num + 
                 countrydummy+ partydummy+
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)
# make simple slopes at mean lower and upper quartiles
ss1 <- interactions::sim_slopes(model=h1testv1  , pred=outgroupdummy.num, modx=logic_c     , modx.values = c(summary(mlm.dat$logic_c)  [[2]]     , summary(mlm.dat$logic_c) [[4]]     , summary(mlm.dat$logic_c)   [[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper
ss2 <- interactions::sim_slopes(model=h1testv1  , pred=outgroupdummy.num, modx=content_c   , modx.values = c(summary(mlm.dat$content_c)[[2]]     , summary(mlm.dat$content_c)[[4]]     , summary(mlm.dat$content_c)[[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper
#ss3 <- interactions::sim_slopes(model=h1test  , pred=outgroupdummy, modx=logic_c.q   , modx.values = c(summary(mlm.dat$logic_c.q)  [[2]]   , summary(mlm.dat$logic_c.q) [[4]]     , summary(mlm.dat$logic_c)   [[5]])     , confint = TRUE) # modx. at lower quartile, mean and upper

# plot
ssplotdata <- tibble(models = rep(c("Model 1: Structure", "Model 1: Content"), times = c(3,3)),
                     coeff  = factor(rep(c("Lower quartile", "Mean", "Upper quartile"), 2)),
                     Estimate =c(ss1$slopes[[2]], ss2$slopes[[2]]),
                     lo95 = c(ss1$slopes[[4]], ss2$slopes[[4]]),
                     hi95 = c(ss1$slopes[[5]], ss2$slopes[[5]])
)

level_order <- c("Upper quartile", "Mean", "Lower quartile")

s1model1 <- ggplot(ssplotdata, aes(x = factor(coeff, level = level_order), y = Estimate)) +
  geom_pointrange(stat = "identity", aes(ymin = lo95, ymax = hi95), size = 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_y_continuous(limits = c(-60,-30), breaks = c(-60, -50, -40, -30, -20)) +
  facet_grid(~models) + 
  theme_cowplot(font_size = 16) +
  labs(x = "Estimate") +
  theme(axis.title.y = element_blank()) +
  coord_flip()


#tiff("simpleslopesmodel1 2010 new.tif", width = 750, height = 250)
s1model1
#dev.off()

interact_plot(h1testv1, pred = , modx = logic_c )

######################################################################  Appendices

################################### Controls

h1test.cont <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                   genderdummy+ age_c + edu_c + inc_c +ethnicdummy+
                    countrydummy+ partydummy+
                 (1 | id), data=mlm.dat)


h1test.st1.cont <- lmer(aff.pol ~  logic_c + 
                     outgroupdummy+
                     logic_c*outgroupdummy + 
                       genderdummy+ age_c + edu_c + inc_c +ethnicdummy+
                       countrydummy+ partydummy+
                                          (1 | id), data=mlm.dat
)

h1test.st2.cont <- lmer(aff.pol ~  + content_c+
                     outgroupdummy+
                     content_c*outgroupdummy +
                       genderdummy+ age_c + edu_c + inc_c +ethnicdummy+
                     countrydummy+ partydummy+
                     (1 | id), data=mlm.dat
)


stargazer(h1test.cont, h1test.st1.cont, h1test.st2.cont, title="Table 1. Multilevel regression model predicting group liking from political identity, and belief system content and structure similarity, including controls (Study 1)",   
          dep.var.labels= "Group liking",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)", "Outgroup dummy",
              "Gender", "Age", "Education", "Income", "Ethnicity",
              "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"
              ),
          column.labels = c("Model 1: Full", "Model 1a: Structure", "Model 1b: Content"), 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          model.numbers = FALSE,
          intercept.bottom = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 suppl models controls mlm 2010.htm", report=("vc*st"),ci = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

################################### Quadratic effects


h1testq <- lmer(aff.pol ~  logic_c + content_c+ 
                  outgroupdummy+
#                  genderdummy+ age_c + edu_c + inc_c +ethnicdummy+
                  logic_c*outgroupdummy + content_c*outgroupdummy + 
                  countrydummy+ partydummy+
  logic_q+content_q+
                  (1 | id), data=mlm.dat)

h1testq.cont <- lmer(aff.pol ~  logic_c + content_c+ 
                  outgroupdummy+
                  genderdummy+ age_c + edu_c + inc_c +ethnicdummy+
                  logic_c*outgroupdummy + content_c*outgroupdummy + 
                  countrydummy+ partydummy+
                    logic_q+content_q+
                  (1 | id), data=mlm.dat)

stargazer(h1testq, h1testq.cont, title="Table 1. Multilevel regression model predicting group liking from political identity, and belief system content and structure similarity, including a quadratic effect and controls (Study 1)",   
          dep.var.labels= "Group liking",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity (ingroup)", "BS content similarity (ingroup)",  "Outgroup dummy",
              "Gender", "Age", "Education", "Income", "Ethnicity",
              "BS structure similarity (quadratic)", "BS content similarity (quadratic)",
              "Structure*outgroup (outgroup)", "Content*outgroup (outgroup)"
            ),
          column.labels = c("Model 1", "Model 1: Controls"), 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          model.numbers = FALSE,
          intercept.bottom = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", out="H1 suppl models quadratic mlm 2010.htm", report=("vc*st"),ci = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

################################### Identification analyses

ggplot(subsamp.in, 
       aes(x = as.factor(target), 
           y = party_id, 
           color = target)) +
  geom_violin(trim=TRUE)+
  theme_cowplot(font_size = 16) +
  facet_grid(cols = vars(country))


h3test <- lm(partyid_c ~ logic_c + content_c + 
               countrydummy+ partydummy,
              data = subset(mlm.dat, mlm.dat$affpoldummy == "ingroup"))
h3test.s1 <- lm(party_id ~ logic_c + 
               countrydummy+ partydummy,
             data = subset(mlm.dat, mlm.dat$affpoldummy == "ingroup"))
h3test.s2 <- lm(party_id ~ content_c + 
               countrydummy+ partydummy,
             data = subset(mlm.dat, mlm.dat$affpoldummy == "ingroup"))

h3test.3 <- lm(aff.pol ~ content_c + logic_c + 
                  countrydummy+ partydummy,
                data = subset(mlm.dat, mlm.dat$affpoldummy == "ingroup"))
h3test.4 <- lm(aff.pol ~ content_c + logic_c + partyid_c+ 
                 countrydummy+ partydummy,
               data = subset(mlm.dat, mlm.dat$affpoldummy == "ingroup"))

summary(h3test)
summary(h3test.s1)
summary(h3test.s2)
summary(h3test.4)

results <- mediation::mediate(h3test, h3test.4, treat='content_c', mediator='partyid_c', boot=FALSE)


####################### table

# table
#detach(package:lmerTest)
stargazer(h3test, h3test.s1, h3test.s2, title="Table 1. Multiple regression model predicting ingroup identification from beleif system content and structure similarity among ingroup members",   
          dep.var.labels= "Group (dis)like",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity", "BS content similarity"),
          column.labels = c("Model 3", "Model 3: Step 1", "Model 3: Step 2"), model.numbers = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          intercept.bottom = FALSE,
          out="H3 models mlm study 1 2010.htm", report=("vc*st"),ci = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


# sig tests
h3test <- lmer(party_id ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                 popdummy +
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)

h4test.1 <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                   partyid_c+ 
                                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                 popdummy +
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)
h4test <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 partyid_c+ 
                 logic_c*outgroupdummy + content_c*outgroupdummy + party_id*outgroupdummy + 
                 countrydummy+ partydummy+
                 #                 popdummy +
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat)

summary(h3test)
summary(h4test)

stargazer(h1test,  h4test.1, h4test, title="Table 2. Multilevel regression model predicting group liking, including political identity, belief system content and structure similairty and political identification (Study 1)",   
          dep.var.labels= "Group (dis)like",
          covariate.labels = 
            c("(Intercept)", "BS structure similarity", "BS content similarity", "Outgroup dummy", "Party identification",
              "BS structure similarity * outgroup", "BS content similarity * outgroup", "Party identification * outgroup"),
          column.labels = c("Model 1", "Model 2.1", "Model 2.2", "Model 2: Step 2"), model.numbers = FALSE,
          digits = 2, align=TRUE, single.row=TRUE, type="text", 
          omit = c("partydummy" , "countrydummy"),
          omit.labels = c("Party fixed effects?" , "Country fixed effects?"),
          intercept.bottom = FALSE,
          out="H4 models mlm study 1 2010.htm", report=("vc*st"),ci = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


