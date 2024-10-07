################################### install and load pacakges

	# List of required packages
		packages <- c("haven", "dplyr", "rio", "tidyverse", "corrr", "sjlabelled", 
					  "Rfast", "lme4", "lmerTest", "cowplot", "foreign", "emmeans","stargazer","parallel","doParallel","sqldf")

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

################################### making sure that we really are working in a clean environment

	# Clear all objects in the environment
	rm(list = ls())

	# Prevent .RData from being loaded automatically
	if (file.exists(".RData")) {
	  unlink(".RData")
	}

################################### import data

# raw data
	# version 4 - 2011-2016 -- the file that was origionally used
	CSES4 <- read_dta("Data/cses4.dta")
	head(CSES4)
	
	# the integrated dataset, which we where suggested to use by David Young
	CSES_IMD <- read_dta("Data/cses_imd.dta")	
	head(CSES_IMD)
	
	nrow(CSES_IMD)
	CSES_IMD_4 <- CSES_IMD[which(CSES_IMD$IMD1008_MOD_4 == 1),]
	nrow(CSES_IMD_4)
	names(CSES_IMD_4)
	
	# I think this is the respondent id
	head(CSES_IMD_4$IMD1005)
	length(CSES_IMD_4$IMD1005) == length(unique(CSES_IMD_4$IMD1005)) # should return TRUE, so not unique..
	
#################################### select data
CSES4_SELECT <- CSES4    %>%  select(D1005, D1006_UN, D1006_NAM,D1004,D1008, D3014, D3001_1, D3001_2, D3001_3, D3001_4, D3001_5, D3001_6, D3001_7, D3001_8, 
                                     D3011_A, D3011_B, D3011_C, D3011_D, D3011_E, D3011_F, D3011_G, D3011_H, D3011_I, D5201_A, D5201_B, D5201_C, D5201_D, D5201_E, D5201_F, D5201_G, D5201_H, D5201_I, 
                                     D3008_LH_PL, D3018_4, D3018_3,
                                     D2002, D2001_Y, D2003, D2020, D2030, D1028)  
CSES4_CLEAN <- CSES4_SELECT  %>% 
                                mutate(id       = D1005,
                                        country_num = as.numeric(D1006_UN),
                                        country = D1006_NAM,
										country_election = D1004,
										election_year = D1008,
                                        ideology= ifelse(D3014<11, D3014, NA),
                                        health  = ifelse(D3001_1<6, D3001_1, NA),
                                        educ    = ifelse(D3001_2<6, D3001_2, NA),
                                        umeploy = ifelse(D3001_3<6, D3001_3, NA),
                                        defense = ifelse(D3001_4<6, D3001_4, NA),
                                        pension = ifelse(D3001_5<6, D3001_5, NA),
                                        busines = ifelse(D3001_6<6, D3001_6, NA),
                                        crime   = ifelse(D3001_7<6, D3001_7, NA),
                                        welfare = ifelse(D3001_8<6, D3001_8, NA),
                                #        polar_1 = as.numeric(ifelse(D3011_A<11, D3011_A, NA)), # we drop these here, because we would like to take them from IMD
                                #        polar_2 = as.numeric(ifelse(D3011_B<11, D3011_B, NA)),
                                #        polar_3 = as.numeric(ifelse(D3011_C<11, D3011_C, NA)),
                                #        polar_4 = as.numeric(ifelse(D3011_D<11, D3011_D, NA)),
                                #        polar_5 = as.numeric(ifelse(D3011_E<11, D3011_E, NA)),
                                #        polar_6 = as.numeric(ifelse(D3011_F<11, D3011_F, NA)),
                                #        polar_7 = as.numeric(ifelse(D3011_G<11, D3011_G, NA)),
                                #        polar_8 = as.numeric(ifelse(D3011_H<11, D3011_H, NA)),
                                #        polar_9 = as.numeric(ifelse(D3011_I<11, D3011_I, NA)),
                                #        partya  = ifelse(D5201_A==9999,NA,D5201_A),
                                #        partyb  = ifelse(D5201_B==9999,NA,D5201_B),
                                #        partyc  = ifelse(D5201_C==9999,NA,D5201_C),
                                #        partyd  = ifelse(D5201_D==9999,NA,D5201_D),
                                #        partye  = ifelse(D5201_E==9999,NA,D5201_E),
                                #        partyf  = ifelse(D5201_F==9999,NA,D5201_F),
                                #        partyg  = ifelse(D5201_G==9999,NA,D5201_G),
                                #        partyh  = ifelse(D5201_H==9999,NA,D5201_H),
                                #        partyi  = ifelse(D5201_I==9999,NA,D5201_I),
                                        lowhouse=D3008_LH_PL,
                                        party   = ifelse(as.numeric(D3018_3)>88,NA,as.numeric(D3018_3)), # this missingness recoding on this used to be somewhere else I think, now doing it here
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
							
								
### here, I use the participant ID to merge in whatever info I need from CSES_IMD_4

	# lets first make CSES_IMD_4 make a bit more sense
		
		nrow(CSES_IMD_4)
		head(CSES_IMD_4)
		
		table(CSES_IMD_4$IMD5000_A)
		names()
		
	CSES_IMD_4_CLEAN <- CSES_IMD_4  %>% 
										  mutate(
											id = IMD1005,
											closestpartyuniqueid = IMD3005_3,
											numid_party_a = IMD5000_A,
											numid_party_b = IMD5000_B,
											numid_party_c = IMD5000_C,
											numid_party_d = IMD5000_D,
											numid_party_e = IMD5000_E,
											numid_party_f = IMD5000_F,
											numid_party_g = IMD5000_G,
											numid_party_h = IMD5000_H,
											numid_party_i = IMD5000_I,
											affpol_party_A = IMD3008_A,
											affpol_party_B = IMD3008_B,
											affpol_party_C = IMD3008_C,
											affpol_party_D = IMD3008_D,
											affpol_party_E = IMD3008_E,
											affpol_party_F = IMD3008_F,
											affpol_party_G = IMD3008_G,
											affpol_party_H = IMD3008_H,
											affpol_party_I = IMD3008_I,
											country_election_imd = IMD1004
										  ) %>%
										  select(
											id,
											closestpartyuniqueid,
											numid_party_a,
											numid_party_b,
											numid_party_c,
											numid_party_d,
											numid_party_e,
											numid_party_f,
											numid_party_g,
											numid_party_h,
											numid_party_i,
											affpol_party_A,
											affpol_party_B,
											affpol_party_C,
											affpol_party_D,
											affpol_party_E,
											affpol_party_F,
											affpol_party_G,
											affpol_party_H,
											affpol_party_I,
											country_election_imd
										  )

		nrow(CSES_IMD_4_CLEAN)
		head(CSES_IMD_4_CLEAN)

#### now merge in what I need from the IMD data
	
		nrow(CSES4_CLEAN)
		CSES4_CLEAN <- CSES4_CLEAN %>%
							  left_join(CSES_IMD_4_CLEAN, by = "id") 
		nrow(CSES4_CLEAN)
		head(CSES4_CLEAN)

#### code CSU in Germany as CDU # decided not to, from an identity point of view, these are really not the same parties.
#
#	CSES4_CLEAN$closestpartyuniqueid[which(CSES4_CLEAN$closestpartyuniqueid == 2760001)] <- 2760002
#	CSES4_CLEAN$closestpartyuniqueid[which(CSES4_CLEAN$closestpartyuniqueid == 2760003)] <- 2760002
#	
#	CSES4_CLEAN$numid_party_a[which(CSES4_CLEAN$numid_party_a == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_a[which(CSES4_CLEAN$numid_party_a == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_b[which(CSES4_CLEAN$numid_party_b == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_b[which(CSES4_CLEAN$numid_party_b == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_c[which(CSES4_CLEAN$numid_party_c == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_c[which(CSES4_CLEAN$numid_party_c == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_d[which(CSES4_CLEAN$numid_party_d == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_d[which(CSES4_CLEAN$numid_party_d == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_e[which(CSES4_CLEAN$numid_party_e == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_e[which(CSES4_CLEAN$numid_party_e == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_f[which(CSES4_CLEAN$numid_party_f == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_f[which(CSES4_CLEAN$numid_party_f == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_g[which(CSES4_CLEAN$numid_party_g == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_g[which(CSES4_CLEAN$numid_party_g == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_h[which(CSES4_CLEAN$numid_party_h == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_h[which(CSES4_CLEAN$numid_party_h == 2760003)] <- 2760002
#
#	CSES4_CLEAN$numid_party_i[which(CSES4_CLEAN$numid_party_i == 2760001)] <- 2760002
#	CSES4_CLEAN$numid_party_i[which(CSES4_CLEAN$numid_party_i == 2760003)] <- 2760002

	# was this successfull? (should return all zero's)
		for (party in letters[1:9]) {
		  party_column <- paste0("numid_party_", party)
		  
		  # Check for 2760001
		  rows_with_2760001 <- nrow(CSES4_CLEAN[which(CSES4_CLEAN[[party_column]] == 2760001),])
		  cat(paste0("Rows with 2760001 in ", party_column, ": ", rows_with_2760001, "\n"))
		  
		  # Check for 2760003
		  rows_with_2760003 <- nrow(CSES4_CLEAN[which(CSES4_CLEAN[[party_column]] == 2760003),])
		  cat(paste0("Rows with 2760003 in ", party_column, ": ", rows_with_2760003, "\n"))
		}

		
#### deal with missingness, make sure it has the internal R code

	# before
	summary(CSES4_CLEAN)
	table(CSES4_CLEAN$closestpartyuniqueid)

	# set the missingness
		CSES4_CLEAN <- CSES4_CLEAN %>%
		  mutate(
			# closest Party
			closestpartyuniqueid = ifelse(closestpartyuniqueid > 9000000, NA,closestpartyuniqueid),
			# Replace values > 90 with NA for affpol_party_* variables
			across(
			  starts_with("affpol_party_"),
			  ~ ifelse(. > 90, NA, .)
			),
			# Replace values > 9000000 with NA for numid_party_* variables
			across(
			  starts_with("numid_party_"),
			  ~ ifelse(. > 9000000, NA, .)
			)
		  )

	# after
	summary(CSES4_CLEAN)
	table(is.na(CSES4_CLEAN$closestpartyuniqueid))
	
	# and preregistered kick out criteria
	
		# not more than time items missing in the belief system
		
		# there need to be at least twenty people that support this party

#### make a country/election level variable (used to be 'country' but we want the simularity measures e.t.c. at the country/election level, not the country level -
	#> noting that some countries, e.g. candada ran the survey twice as part of wave 4 as there where two elections)

	table(paste(CSES4_CLEAN$election_year,CSES4_CLEAN$country,sep="_"))
	table(is.na(CSES4_CLEAN$election_year))
	table(is.na(CSES4_CLEAN$country))
	
	# /\ these two tables should be the same \/ looks like they are
	table(CSES4_CLEAN$country_election)
	
	table(CSES4_CLEAN$country_election_imd)

## OK, so as a first step, lets have a look at the unique identifier of the party I feel closest to (taken from the IMD codebook as IMD3005_3
	
	# old closest parties VS NOW
		
		# old
		table(CSES4_CLEAN$party)
		table(is.na(CSES4_CLEAN$party))
		
		# new
		CSES4_CLEAN$closestpartyuniqueid # this is the unique indentifier of the party I am closest to
		table(CSES4_CLEAN$closestpartyuniqueid)
		table(is.na(CSES4_CLEAN$closestpartyuniqueid)) # note that these NA scores are not the same, but this is due to the 44 swiss cases that John also identified that do not get a letter score anyways
		

	
## NOW, lets inspect who party A id for all the repondents, this should be the same within all the countries -- yes, this is the pattern one would expect. 
		table(CSES4_CLEAN$numid_party_a, CSES4_CLEAN$country)
		table(CSES4_CLEAN$numid_party_b, CSES4_CLEAN$country)
			
## the like/dislike variables for party A e.t.c. are here "IMD3008_A - IMD3008_I #(do we need to think about the last three being optional?)

	# lets do some inspections
		summary(CSES4_CLEAN$affpol_party_A)
		hist(CSES4_CLEAN$affpol_party_A)
		
		summary(CSES4_CLEAN$affpol_party_B)
		hist(CSES4_CLEAN$affpol_party_B)
		
		summary(CSES4_CLEAN$affpol_party_C)
		hist(CSES4_CLEAN$affpol_party_C)
		
		summary(CSES4_CLEAN$affpol_party_D)
		hist(CSES4_CLEAN$affpol_party_D)
		
		summary(CSES4_CLEAN$affpol_party_E)
		hist(CSES4_CLEAN$affpol_party_E)
		
		summary(CSES4_CLEAN$affpol_party_F)
		hist(CSES4_CLEAN$affpol_party_F)
		
		summary(CSES4_CLEAN$affpol_party_G)
		hist(CSES4_CLEAN$affpol_party_G)
		
		summary(CSES4_CLEAN$affpol_party_H)
		hist(CSES4_CLEAN$affpol_party_H)
		
		summary(CSES4_CLEAN$affpol_party_I)
		hist(CSES4_CLEAN$affpol_party_I)

#################################### ANALYTICAL SAMPLE selection

# As per pre-reg remove pps for whom n>= 2 beleif system items are absent

	## filter if belief system items do not have more than 2 missing
	
		nrow(CSES4_CLEAN)
		
			# what the pre-registration says
			# CSES4_SAMPLE_P1 <- CSES4_CLEAN %>% filter(!(na_bs > 2)) # should be 2 or more i.e.,  # used to say: CSES4_SAMPLE_P1 <- CSES4_CLEAN %>% filter(party < 10 & na_bs < 2)
		
			# what David did
			CSES4_SAMPLE_P1 <- CSES4_CLEAN %>% filter(!(na_bs > 1))
		
		nrow(CSES4_SAMPLE_P1)
	
	## drop all the cases where we do not know the closestpartyuniqueid

		table(is.na(CSES4_SAMPLE_P1$closestpartyuniqueid))

		CSES4_SAMPLE_P1A <- CSES4_SAMPLE_P1 %>%
			filter(!is.na(closestpartyuniqueid))

		nrow(CSES4_SAMPLE_P1A)
	
	## and if there are at least 20 party supporters for each group in this 
		
		length(table(CSES4_SAMPLE_P1A$country_election))
		length(table(CSES4_SAMPLE_P1A$closestpartyuniqueid))
		
			CSES4_SAMPLE_P2 <- CSES4_SAMPLE_P1A %>% 
			  group_by(country_election, closestpartyuniqueid) %>% 
			  filter(n() >20) %>% 
			  ungroup()

			nrow(CSES4_SAMPLE_P2)

### start of bug hunting!	
	
		# how many parties with a number above 9
			as.numeric(cbind(names(table(CSES4_SAMPLE_P1A$party)),table(CSES4_SAMPLE_P1A$party)))
			
	
	
		# lets pick a country where we know the filters are off
		
			table(CSES4_SAMPLE_P1A$country_election)
			ROU_2012 <- CSES4_SAMPLE_P1A[which(CSES4_SAMPLE_P1A$country_election == "ROU_2012"),]
			nrow(ROU_2012)
			
			table(ROU_2012$closestpartyuniqueid,ROU_2012$party)
	
		
		
		table(CSES4_SAMPLE_P2$country_election)
		
		table(is.na(CSES4_SAMPLE_P2$closestpartyuniqueid)) 
		
		summary(CSES4_SAMPLE_P2)
		
		table(CSES4_SAMPLE_P2$country_election) - table(CSES4_SAMPLE_P1$country_election)
		
		table(CSES4_SAMPLE_P2$country_election) - table(CSES4_SAMPLE_P1A$country_election)
		
		as.data.frame(CSES4_SAMPLE_P2[0:20,])
		
		## same thing, but how David does it
		
			# maybe it goes wrong at his letter coding?
			
			# Now convert the party closeness codes into letter codes
				party.conversion <- CSES4_SAMPLE_P1A %>% 
				  select(country_election_imd, contains("numid_party",ignore.case=TRUE)) %>% 
				  distinct %>%
				  pivot_longer(-country_election_imd, names_prefix = "numid_party_") %>%
				  filter(!is.na(value), value < 9999980) %>%
				  mutate(value = paste(country_election_imd, value, sep = "_"), .keep = "unused") %>%
				  select(value, name) %>%
				  deframe

				CSES4_SAMPLE_P1$party_asletter <- recode(paste(CSES4_SAMPLE_P1$country_election, CSES4_SAMPLE_P1$party, sep = "_"), !!!party.conversion) %>% ifelse(. %in% LETTERS, ., NA)
		
				table(CSES4_SAMPLE_P1$party_asletter)
		
		
		cut2 <- CSES4_SAMPLE_P1 %>%
		  group_by(country_election, party) %>%
		  mutate(n = n()) %>%
		  group_by(country_election) %>%
		  mutate(party = ifelse(n <= 20, NA, party)) %>%                
		  filter(!is.na(party)) %>% 
		  ungroup
		  
		nrow(cut2) # is the same as above, so it is not the script or the party that is being used... it must be before this..
		as.data.frame(cut2[0:20,])

### end of bug hunting!			
	
	## also, for your party, affective polarization needs to be measured somewhere
	
		nrow(CSES4_SAMPLE_P2)
		
		table(CSES4_SAMPLE_P2$closestpartyuniqueid)
		table(CSES4_SAMPLE_P2$numid_party_a)
		
			# get a variable that tells me if it was measured somewhere
				resvec <- vector()
				pb = txtProgressBar(min = 0, max = nrow(CSES4_SAMPLE_P2), initial = 0) 
				for(i in 1:nrow(CSES4_SAMPLE_P2))
				{
					# get a vector with all party ids available for me
					allpartyidsavail <- c(
										  CSES4_SAMPLE_P2$numid_party_a[i],
										  CSES4_SAMPLE_P2$numid_party_b[i],
										  CSES4_SAMPLE_P2$numid_party_c[i],
										  CSES4_SAMPLE_P2$numid_party_d[i],
										  CSES4_SAMPLE_P2$numid_party_e[i],
										  CSES4_SAMPLE_P2$numid_party_f[i],
										  CSES4_SAMPLE_P2$numid_party_g[i],
										  CSES4_SAMPLE_P2$numid_party_h[i],
										  CSES4_SAMPLE_P2$numid_party_i[i]
										)
					
					resvec[i] <- CSES4_SAMPLE_P2$closestpartyuniqueid[i] %in% allpartyidsavail
					setTxtProgressBar(pb,i)
				}
				close(pb)
				
				CSES4_SAMPLE_P2$ismypartymeasured <- resvec
			
				table(CSES4_SAMPLE_P2$ismypartymeasured)
				
			# do the reduction
				CSES4_SAMPLE <- CSES4_SAMPLE_P2[which(CSES4_SAMPLE_P2$ismypartymeasured),]
		
		nrow(CSES4_SAMPLE)
		
	# some inspections
                # get number of cases per country_election 
                   CSES4_SAMPLE %>% 
                     group_by(country_election) %>% 
                     summarise(n = n())
                   
				# get number of cases per country_election starting with smallest cases per country_election
                   CSES4_SAMPLE %>% 
                      count(country_election) %>% 
                     arrange(n)
             
				# check missingness
                      CSES4_SAMPLE %>% 
                        group_by(country_election,closestpartyuniqueid) %>% 
                        summarise(n = n())
                      
                      table(CSES4_SAMPLE$country_election, CSES4_SAMPLE$closestpartyuniqueid)  
                      table(CSES4_SAMPLE$na_bs)
					  nrow(CSES4_SAMPLE)
    
	TIXCHECKDAT <- CSES4_SAMPLE[which(!is.na(CSES4_SAMPLE$closestpartyuniqueid)),]
	nrow(TIXCHECKDAT)

### sample check against David	

	nrow(CSES4_SAMPLE)
	as.data.frame(CSES4_SAMPLE)[0:20,]
	
	table(is.na(CSES4_SAMPLE$closestpartyuniqueid))
	
	SAMCHECK <- CSES4_SAMPLE[which(!is.na(CSES4_SAMPLE$closestpartyuniqueid)),]
	nrow(SAMCHECK) # felicity gets 30.053 cases here. -- Felicity now has 30.191 cases here.
	
############################################ Make BS similarity
             
################################## clustering script

######### get cores etc ready

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

######### run the loop, fast version with transposed matrix (getting the diadic data)      

	DATAFORLOOP <- CSES4_SAMPLE

	foreachlist <- foreach(cname = names(table(DATAFORLOOP$country_election)),
                      .packages= c("dplyr", "corrr", "rio", "tidyverse", "sjlabelled"))  %dopar% {
                        
                        dat_selectloop <- DATAFORLOOP[which(DATAFORLOOP$country_election == cname),]
                        
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
                        allsimdata$country_election <- cname

                       allsimdata
                      } 
  
  
	newdf <- bind_rows(foreachlist, .id = NULL)

	# bunch of inspections from Tomas

		# check newdf for country_election completeness
		table(names(table(newdf$country_election)) == names(table(CSES4_SAMPLE$country_election))) # should return TRUE only

		# check if the basis structure of newdf is what one would expect it to be
			head(newdf)
			nrow(newdf)
			nrow(newdf[which(newdf$x == "036020130001000794"),]) # should be the sample size of australia
			table(CSES4_SAMPLE$country_election) # gets close, I guess there was probably some missingness on some variables. -- 
			table(CSES4_SAMPLE$country_election,CSES4_SAMPLE$na_bs)
	
################################## NEW final steps of the data buildup

######### buildup an 'empty' dataframe with the structure we know we need (so, 9 rows per participant - one for each party)

	## UOA: relation with parties within participants (9 per participant, one for each party affective polarisation was measured for)
	# Reshape the data from wide to long format, keeping only some core columns we need to for matching later
		CSES4_LONG <- CSES4_SAMPLE %>%
		  select(id, country_election, closestpartyuniqueid, starts_with("numid_party_")) %>%  # Keep only necessary columns
		  pivot_longer(
			cols = starts_with("numid_party_"),  # Select columns to pivot (numid_party_a to numid_party_i)
			names_to = "numid_party",            # New column for party identifiers (a-i)
			values_to = "numid_value"            # New column for the values of numid_party
		  )

	# View the reshaped data
		CSES4_LONG[0:20,]
		
	# show me the greek data # we checked this against Felicity' checks, this looks good.
		table(CSES4_LONG$country_election)
		GR <- CSES4_LONG[which(CSES4_LONG$country_election == "GRC_2012" | CSES4_LONG$country_election == "GRC_2015"),]
		nrow(GR)
		table(GR$numid_party,GR$numid_value,GR$country_election)
		GR[0:20,]

######### get party into the diadic data from newdf, using the old code but using closestpartyuniqueid instead of party, from the cluster script we also have country_election
	
	## UOA: diadic between all participants
	# get the party (closestpartyuniqueid) from CSES4_SAMPLE for the 'sending' participant (x)
		allsimdata1 <- merge(x = newdf,        y = CSES4_SAMPLE[ , c("id", "closestpartyuniqueid")], by.x = "x", by.y = "id",all.x=TRUE) # done now -> # Here, you probably want to merge in the unique id of the party?
		nrow(allsimdata1)
		head(allsimdata1)
		tail(allsimdata1)
		#table(allsimdata1$closestpartyuniqueid)
		#table(allsimdata1$country_election)

	#  get the party (closestpartyuniqueid) from CSES4_SAMPLE for the 'receiving' participant (y)
		allsimdata2 <- merge(x = allsimdata1,  y = CSES4_SAMPLE[ , c("id", "closestpartyuniqueid")], by.x = "y", by.y = "id",all.x=TRUE) # done now -> # also unique id here?
		nrow(allsimdata2)
		head(allsimdata2)
		
		# making sure closestpartyuniqueid.y here is indeed what I think it is # answer, yes it is, closestpartyuniqueid.y is the party of the other
		as.data.frame(CSES4_SAMPLE[which(CSES4_SAMPLE$id == "036020130007949553"),])
		as.data.frame(CSES4[which(CSES4$D1005 == "036020130007949553"),])
		
		tail(allsimdata2)
		#table(allsimdata2$closestpartyuniqueid.x)
		#table(allsimdata2$closestpartyuniqueid.y)
		#table(allsimdata2$country_election) 

######### aggregate diadic data to the party and country_election level

	## here the diadic data is tranformed to parties within participant level data
		# new level of analysis: from diadic to partipant = all parties in country-election -- and this is the final unit of analysis!
			# note: needs to target (now: closestpartyuniqueid.y, so the party of the other participants) as well as country_election (we need to make sure that elections are seperated)

		# create two dataframes, one for logic and one for content
			logic <-   aggregate(logic       ~x+closestpartyuniqueid.y+country_election, mean, data = allsimdata2, na.action = na.omit) # lets add country election here!
			head(logic)
			
			nrow(logic) # does the size of this reduction make sense?
			table(logic$closestpartyuniqueid.y)
			
			# still 9 observations per participant?
				logic[which(logic$x == "036020130001000794"),] # only 4, but that might because of NA's?
				table(allsimdata2[which(allsimdata2$x == "036020130001000794"),]$closestpartyuniqueid.y) # indeed 
			
			content <- aggregate(content.sim ~x+closestpartyuniqueid.y+country_election, mean, data = allsimdata2, na.action = na.omit)
			head(content)
		
		# combine them
			LOCO <- merge(logic, content, by = c('x','closestpartyuniqueid.y','country_election'))
			LOCO <- LOCO %>% rename(  id = x,
												  contentdif = content.sim )
			head(LOCO)
        
		# again, relabbeling for later
			LOCO <- rename(LOCO, partytarget = closestpartyuniqueid.y)
			head(LOCO)
			
######### merge the participant specific partyid and country_election level values into our new long data, to get the data into the format we need for our analysis

		# sqldf version
	#	FINDAT <- sqldf("SELECT CSES4_LONG.*, LOCO.logic, LOCO.contentdif
	#			   FROM CSES4_LONG
	#			   LEFT JOIN LOCO ON (
	#				   CSES4_LONG.id = LOCO.id AND
	#				   CSES4_LONG.numid_value = LOCO.partytarget AND
	#				   CSES4_LONG.country_election = LOCO.country_election
	#			   )")
		
		# dplyer version
		FINDAT <- CSES4_LONG %>%
					  left_join(LOCO, by = c(
						"id" = "id",
						"numid_value" = "partytarget",
						"country_election" = "country_election"
					  )) %>%
					  select(everything(), logic, contentdif)

		FINDAT[0:20,]
		
		table(is.na(FINDAT$logic))
		table(is.na(FINDAT$contentdif))
		
		# are there ids in FINDAT that are not in CSES4_SAMPLE?
			
			# IDs in FINDAT$id that are not in CSES4_SAMPLE$id
			ids_in_FINDAT_not_in_CSES4 <- setdiff(FINDAT$id, CSES4_SAMPLE$id)
			ids_in_FINDAT_not_in_CSES4 # should be empty

			# IDs in CSES4_SAMPLE$id that are not in FINDAT$id
			ids_in_CSES4_not_in_FINDAT <- setdiff(CSES4_SAMPLE$id, FINDAT$id)
			ids_in_CSES4_not_in_FINDAT # should be empty		
		
#### now  make a variable that tell me if a row is ingroup or outgroup (was called affpoldummy before in Felicity' code)

	FINDAT$affpoldummy <- ifelse(FINDAT$closestpartyuniqueid == FINDAT$numid_value,"ingroup","outgroup")
	table(FINDAT$affpoldummy)
	table(is.na(FINDAT$affpoldummy))
	
	# every person should only have one ingroup
		table(FINDAT$affpoldummy) # which is not the case...
		nrow(FINDAT)
	
	# are the ids unique?
		filtered_data <- FINDAT %>%
		  filter(affpoldummy == "ingroup") %>%
		  group_by(id) %>%
		  filter(n() > 1) %>%
		  ungroup()

		# View the filtered results
		filtered_data
		
		table(filtered_data$country_election)
	

############################ Make MLM dataset	
#### merge this whole new shabang with CSES4_SAMPLE

	## before we merge however, lets drop the 'wide' variables (numid_party_a until numid_party_i AND affpol_party_A-affpol_party_I), the latter does need to be merged in, but we will do that in a moment
	CSES4_SAMPLE_FOCUSED <- CSES4_SAMPLE %>%
							select(-starts_with("numid_party"), -starts_with("affpol_party"))
	as.data.frame(CSES4_SAMPLE_FOCUSED[0:20,])
							
	# sqldf version
	#	mlm.dat <- sqldf("SELECT FINDAT.numid_party, FINDAT.numid_value, FINDAT.logic, FINDAT.contentdif, FINDAT.affpoldummy, CSES4_SAMPLE.*
	#			   FROM FINDAT
	#			   LEFT JOIN CSES4_SAMPLE ON (
	#				   FINDAT.id = CSES4_SAMPLE.id
	#			   )")

	# dplyr version
		mlm.dat <- FINDAT %>%
						  left_join(CSES4_SAMPLE_FOCUSED, by = "id") %>%
						  select(numid_party, numid_value, logic, contentdif, affpoldummy, everything())
	
		nrow(mlm.dat) 
	
	# Extract the rightmost letter from numid_party using substr and put it in a new variable and capitalise it using the function 'to upper'
		mlm.dat$partyletter <- toupper(substr(mlm.dat$numid_party, nchar(mlm.dat$numid_party), nchar(mlm.dat$numid_party)))
		
	
	# For reporting purposes in the memo, how many cases here where actually affected?
		
		# number of cases affected
		
			# per letter (not sure this makes sense)
			table(mlm.dat$partyletter,mlm.dat$party)
		
			# in one number / percentage
			sum(mlm.dat$partyletter != mlm.dat$party, na.rm = TRUE) / nrow(mlm.dat)
		
		# and how much of the ingroup/outgroup labels does this actually affected
			
			mlm.dat$OLDincorrectinoutlabel <- ifelse(mlm.dat$party == mlm.dat$numid_value,"ingroup","outgroup")
		
	
### now merge in the affective polarisation bits
		
	# for this, first we need to affective polarisation values also in the long format
		# Reshape the data from wide to long format, keeping only some core columns we need to for matching later
		CSES4_LONG_AFFPOL <- CSES4_SAMPLE %>%
		  select(id, country_election, closestpartyuniqueid, starts_with("affpol_party_")) %>%  # Keep only necessary columns
		  pivot_longer(
			cols = starts_with("affpol_party_"),  # Select columns to pivot (numid_party_a to numid_party_i)
			names_to = "affpol_party",            # New column for party identifiers (a-i)
			values_to = "affpol_value"            # New column for the values of numid_party
		  )
		nrow(CSES4_LONG_AFFPOL)
		
		# Extract the rightmost letter from affpol_party using substr and put it in a new variable
		CSES4_LONG_AFFPOL$partyletter <- substr(CSES4_LONG_AFFPOL$affpol_party, nchar(CSES4_LONG_AFFPOL$affpol_party), nchar(CSES4_LONG_AFFPOL$affpol_party))
		
		CSES4_LONG_AFFPOL[0:20,]
		
		# manual check on the top cases
		as.data.frame(CSES4_SAMPLE[which(CSES4_SAMPLE$id == "036020130001000794"),])
		
	# now merge this into mlm.dat (would probably also be possible with a simple rbind, as both should be the same length, but this is safer and less vurnerable to errors 
		# in the data)
		
		nrow(mlm.dat)
		
			mlm.dat <- mlm.dat %>%
					  left_join(CSES4_LONG_AFFPOL, by = c(
						"id" = "id",
						"partyletter" = "partyletter",
						"country_election.x" = "country_election"
					  )) %>%
					  select(everything(), affpol_value)

		nrow(mlm.dat)

### make final variables
	nrow(mlm.dat)
	mlm.dat.fin <- mlm.dat%>% 
							mutate(outgroupdummy = ifelse(affpoldummy == "ingroup",0,
														  ifelse(affpoldummy == "outgroup",1,NA)),
								   content     	= 1-contentdif,
								   logic_c     	= mlm.dat$logic-mean(mlm.dat$logic,na.rm=TRUE),
								   ingroup     	= as.factor(affpoldummy),
								   partynum    	= paste0(closestpartyuniqueid.x), #partynum    	= paste0(closestpartyuniqueid.x),
								   countryelectiondummy	= as.factor(country_election.x),
								   aff.pol 		= affpol_value,
								   aff.pol.num 	= as.numeric(aff.pol),
								   target = partyletter
							   
							)                      
	nrow(mlm.dat.fin)
	as.data.frame(mlm.dat[0:20,])
	
	mlm.dat.fin <- mlm.dat.fin%>% 
	  mutate(content_c= mlm.dat.fin$content-mean(mlm.dat.fin$content,na.rm=TRUE),
			 partydummy  = as.factor(partynum),
			 logic_q     = logic_c^2,
			 content_q     = (mlm.dat.fin$content-mean(mlm.dat.fin$content,na.rm=TRUE))^2)

## how many unique participants in this final data.

	# John is dropping: anyone with NA on relevant variables:
	SAMPLECHECKLONG <- mlm.dat.fin[which(!(	is.na(mlm.dat.fin$content_c)|
										is.na(mlm.dat.fin$logic_c)|
										is.na(mlm.dat.fin$ingroup)|
										is.na(mlm.dat.fin$aff.pol))),]
	nrow(SAMPLECHECKLONG)
	length(unique(SAMPLECHECKLONG$id))
	

################################### descriptives

## inspection of the key variables

	summary(mlm.dat.fin$affpoldummy)
	table(mlm.dat.fin$affpoldummy)
	table(is.na(mlm.dat.fin$affpoldummy))
	
	summary(mlm.dat.fin$outgroupdummy)
	table(mlm.dat.fin$outgroupdummy)
	table(is.na(mlm.dat.fin$outgroupdummy))
	
	summary(mlm.dat.fin$content)
	table(is.na(mlm.dat.fin$content))
	
	summary(mlm.dat.fin$aff.pol)
	hist(mlm.dat.fin$aff.pol)


#cor set

cordat <- mlm.dat.fin %>% 
          dplyr::select(aff.pol, logic_c, content_c, outgroupdummy)
cor(cordat, use = "pairwise.complete.obs")

cor(mlm.dat.fin$logic, mlm.dat.fin$content, use = "pairwise.complete.obs")
cor.test(mlm.dat.fin$logic, mlm.dat.fin$content, method = "pearson")

# get subsamples
table(mlm.dat.fin$affpoldummy)

subsamp.in <- subset(mlm.dat.fin, mlm.dat.fin$affpoldummy == "ingroup")
subsamp.ot <- subset(mlm.dat.fin, mlm.dat.fin$affpoldummy == "outgroup")
subsamp.t1 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "A")) # this now contains the party letters, used to say: & mlm.dat.fin$target == "polar_1"))
subsamp.t2 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "B"))
subsamp.t3 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "C"))
subsamp.t4 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "D"))
subsamp.t5 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "E"))
subsamp.t6 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "F"))
subsamp.t7 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "G"))
subsamp.t8 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "H"))
subsamp.t9 <- subset(mlm.dat.fin, (mlm.dat.fin$affpoldummy == "outgroup" & mlm.dat.fin$target == "I"))

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
nrow(subsamp.in)

cor.test(subsamp.ot$aff.pol, subsamp.ot$content, method = "pearson")
nrow(subsamp.ot)

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
p2 <- mlm.dat.fin %>%
  ggplot( aes(x=logic, fill=affpoldummy)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")
p2

# histogram content
q2 <- mlm.dat.fin %>%
  ggplot( aes(x=content, fill=affpoldummy)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

q2

# volin plot

ggplot(mlm.dat.fin, 
       aes(x = logic, 
           y = aff.pol, 
           color = target)) +
  geom_violin(trim=TRUE)+
  theme_cowplot(font_size = 16) +
  facet_grid(cols = vars(target), rows = vars(affpoldummy))

################################### # are people more similar to thier ingroup than outgroup?
testc <- lmer(content ~  
                outgroupdummy+
                countryelectiondummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat.fin)

tests <- lmer(logic ~  
                outgroupdummy+
                countryelectiondummy+ partydummy+
                #                edu_c + age_c + inc_c +ethnicdummy+
                (1 | id), data=mlm.dat.fin)

summary(testc)
summary(tests)

effectsize::standardize_parameters(testc)
effectsize::standardize_parameters(tests)

################################### H1 test

emtyh1 <- lmer(aff.pol ~ 1 + (1 | id), data=mlm.dat.fin)
compute_icc(emtyh1)

head(mlm.dat.fin)

h1test <- lmer(aff.pol ~  logic_c + content_c+ 
                 outgroupdummy+
                 logic_c*outgroupdummy + content_c*outgroupdummy + 
                 countryelectiondummy+ partydummy+
                 #                edu_c + age_c + inc_c +ethnicdummy+
                 (1 | id), data=mlm.dat.fin)

summary(h1test) 

# Set the current timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Export the model
h1test_tomas <- h1test
save(h1test_tomas, file = paste0("h1test_tomas_", timestamp, ".RData"))

# Export the model data
h1test_tomas_model_data <- model.frame(h1test_tomas)
nrow(h1test_tomas_model_data)
h1test_tomas_model_data[0:20,]
write.csv(h1test_tomas_model_data, paste0("h1test_tomas_model_data_", timestamp, ".csv"), row.names = FALSE)


				 
table(mlm.dat.fin$country) # OK, so here we have all the countries again...  how?!


h1test.st1 <- lmer(aff.pol ~  logic_c + 
                     outgroupdummy+
                     logic_c*outgroupdummy + 
                     countryelectiondummy+ partydummy+
                     (1 | id), data=mlm.dat.fin
)

h1test.st2 <- lmer(aff.pol ~  + content_c+
                     outgroupdummy+
                     content_c*outgroupdummy +
                     countryelectiondummy+ partydummy+
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

