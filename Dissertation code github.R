#Final code

#Open packages
library(tidyverse) 
library(partynat) #package made by Juraj Medzihorsky @jmedzihorsky on github
library(fixest) #package for fixed effects estimation

#Open Lok Dhaba dataset
general_election <- read.csv("~/All_States_GE.csv") #FILE PATH HERE

#Group results by year, party and constituency
general_election <- general_election %>%
  group_by(Year, Party, State_Name, Constituency_Name) %>%
  summarise(Votes = sum(Votes)) #ge_1 has 50052 obs

#Madras and Mysore are given their up-to-date names so they have the same fixed effects
general_election$State_Name[general_election$State_Name == "Madras"] <- "Tamil_Nadu"
general_election$State_Name[general_election$State_Name == "Mysore"] <- "Karnataka"


#Select only election years
general_election <- general_election[general_election$Year %in% c(1962, 1967, 1971, 1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019),]

#Remove 0 vote candidates - there were a few zero vote entries into teh 
general_election <- general_election[general_election$Votes !=0,]

#Add a unique identifier for each constituency (some states share constituencies)
general_election$State_Constituency <- paste0(general_election$State_Name, general_election$Constituency_Name)
#Make sure no repeats are included in a given year
general_election <- general_election %>%
  arrange(Year, State_Name, Constituency_Name, State_Constituency,  desc(row_number())) %>%
  distinct(Year, State_Name, Constituency_Name, State_Constituency, .keep_all = TRUE)

#Name unnamed rows
general_election$Party <- ifelse(general_election$Party == "", "No_name", general_election$Party)

general_election <- na.omit(general_election) #50013 obs

#Make function to calculate party nationalisation scores separately for each year

election_years <- c(1962, 1967, 1971,1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019)

PNS_function <- function(df, election_years) {
  result_list <- list()
  
  for (year in election_years) {
    year_df <- df[df$Year == year, ]
    
    # Create a contingency table  
    votes_table <- year_df %>%
      pivot_wider(names_from = Party, values_from = Votes, values_fill = list(Votes = 0))
    
    votes_df <- as.data.frame(votes_table)
    
    votes_df <- na.omit(votes_df)
    
    row.names(votes_df) <- votes_df$State_Constituency
    votes_df <- votes_df %>%
      select(-Year,-State_Constituency, -State_Name, -Constituency_Name)
    votes_df[is.na(votes_df)] <- 0
    
    
    # Convert the table to a matrix
    matrix <- as.matrix(votes_df)
    matrix[is.na(matrix)] <- 0
    matrix <- matrix[, colSums(matrix != 0) > 0]
    
    # Calculate PNS using partynat
    Medhizorsky_PNS <- partynat(matrix, statistic = "PNS", weight_choice = TRUE, weight_territory = FALSE)
    
    PNS_score <- as.data.frame(Medhizorsky_PNS$choices)
    
    result_list[[as.character(year)]] <- PNS_score
    
  }
  return(result_list)
  
}

#Run function
PNS_scores <- PNS_function(general_election, election_years)

PNS_scores <- do.call(rbind, PNS_scores)

#Make variable from rownames (e.g. "2019.BJP")
PNS_scores$Year_Party <- rownames(PNS_scores)

PNS_scores <- separate(PNS_scores, col=Year_Party, into = c("Year", "Party"), sep = "\\.")


#Returning to the main dataset, calculate the proportion of votes won by each party in each constituency
general_election <- general_election %>%
  group_by(Year, Party, State_Name, Constituency_Name, State_Constituency) %>%
  summarise(Votes = sum(Votes)) %>%
  ungroup()

general_election <- general_election %>%
  group_by(Year, State_Name, Constituency_Name, State_Constituency) %>%
  mutate(Prop_votes = (Votes/sum(Votes))) %>%
  ungroup()

#For the

#Merge the party nationalisation scores with the general election dataset
general_election_PNS <- merge(general_election, PNS_scores, by.x = c("Year", "Party"), by.y = c("Year", "Party"), all = T)

#Give Independents a 0 PNS score - Independents were getting high PNS because
#they were misclassified as belonging to the same party
general_election_PNS$est[general_election_PNS$Party == "IND"] <- 0

#Calculate nationalised voting score (dependent variable)
general_election_A_PNS <- general_election_PNS
general_election_A_PNS$Nationalised_voting_score <- general_election_A_PNS$est * general_election_A_PNS$Prop_votes

general_election_A_PNS <- general_election_A_PNS %>%
  group_by(Year, State_Name, State_Constituency, Constituency_Name) %>%
  summarise(Nationalised_voting_score = sum(Nationalised_voting_score)) %>%
  ungroup()

general_election_A_PNS <- general_election_A_PNS %>%
  select(Year, State_Name, State_Constituency, Nationalised_voting_score)

#For the alternative dependent variables
general_election_B_PNS <- general_election_PNS
general_election_B_PNS$Party_Type <-ifelse(general_election_B_PNS$est > 0.5, "National", "Subnational")

general_election_B_PNS <- general_election_B_PNS %>%
  group_by(Party_Type, State_Constituency, Year, State_Name) %>%
  summarise(Prop_votes = sum(Prop_votes)) %>%
  ungroup()

general_election_B_PNS_NAT <- general_election_B_PNS[general_election_B_PNS$Party_Type == "National",]
general_election_B_PNS_SUBNAT <- general_election_B_PNS[general_election_B_PNS$Party_Type == "Subnational",]

#Add independent variables

#All 'State_Name' are modified so Karnataka is Mysore and Madras is renamed Tamil_Nadu

##Simultaneity variable
dates <- read.csv("~/Election dates.csv") #FILE PATH HERE
dates$State_Name<-ifelse(dates$State_Name == "Mysore", "Karnataka",
                         ifelse(dates$State_Name == "Madras", "Tamil_Nadu", dates$State_Name))

general_election_A_PNS <- merge(general_election_A_PNS, dates, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS <- merge(general_election_B_PNS, dates, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_NAT <- merge(general_election_B_PNS_NAT, dates, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_SUBNAT <- merge(general_election_B_PNS_SUBNAT, dates, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))

##Per capita income
GSDP_per_cap <- read.csv("~/GSDP Index.csv") #FILE PATH HERE
GSDP_per_cap <- GSDP_per_cap %>%
  select("State_Name", "Year", "Per.capita.income")
GSDP_per_cap$State_Name <- ifelse(GSDP_per_cap$State_Name == "Mysore", "Karnataka",
                                  ifelse(GSDP_per_cap$State_Name == "Madras", "Tamil_Nadu", GSDP_per_cap$State_Name))

general_election_A_PNS <- merge(general_election_A_PNS, GSDP_per_cap, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS <- merge(general_election_B_PNS, GSDP_per_cap, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_NAT <- merge(general_election_B_PNS_NAT, GSDP_per_cap, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_SUBNAT <- merge(general_election_B_PNS_SUBNAT, GSDP_per_cap, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))

##Literacy rate
literacy <- read.csv("~/Statewise literacy (2).csv") #FILE PATH HERE
literacy <- literacy[literacy$Year %in%  c(1962, 1967, 1971, 1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019),]
literacy$State_Name <- ifelse(literacy$State_Name == "Mysore", "Karnataka",
                              ifelse(literacy$State_Name == "Madras", "Tamil_Nadu", literacy$State_Name))

general_election_A_PNS <- merge(general_election_A_PNS, literacy, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS <- merge(general_election_B_PNS, literacy, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_NAT <- merge(general_election_B_PNS_NAT, literacy, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_SUBNAT <- merge(general_election_B_PNS_SUBNAT, literacy, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))

##Fiscal centralisation
fiscal <- read.csv("~/Fiscal centralisation 1980-2018.csv") #FILE PATH HERE
fiscal <- fiscal[fiscal$Year %in%  c(1962, 1967, 1971, 1977, 1980, 1984, 1989, 1991, 1996, 1998, 1999, 2004, 2009, 2014, 2019),]
fiscal$State_Name <- ifelse(fiscal$State_Name == "Mysore", "Karnataka",
                              ifelse(fiscal$State_Name == "Madras", "Tamil_Nadu", fiscal$State_Name))

general_election_A_PNS <- merge(general_election_A_PNS, fiscal, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS <- merge(general_election_B_PNS, fiscal, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_NAT <- merge(general_election_B_PNS_NAT, fiscal, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))
general_election_B_PNS_SUBNAT <- merge(general_election_B_PNS_SUBNAT, fiscal, by.x = c("State_Name", "Year"), by.y = c("State_Name", "Year"))

#Nationalised voting
feols(Nationalised_voting_score ~ Simultaneous + Literacy + Per.capita.income | State_Name + Year, data  = general_election_A_PNS, cluster = ~State_Name + ~Year)
#Nationalised voting, state-specific time trends
feols(Nationalised_voting_score ~ Simultaneous | State_Name^Year, data  = general_election_A_PNS)

#On national parties 
feols(Prop_votes ~ Simultaneous + Literacy + Per.capita.income | Year + State_Name, data  = general_election_B_PNS_NAT, cluster = ~State_Name + ~Year)
#National parties, state-specific time trends
feols(Prop_votes ~ Simultaneous | Year + Year^State_Name, data  = general_election_B_PNS_NAT, cluster = ~State_Name + ~Year) 

#On state parties
feols(Prop_votes ~ Simultaneous + Literacy + Per.capita.income | Year + State_Name, data  = general_election_B_PNS_SUBNAT, cluster = ~State_Name + ~Year)
#State parties, state-specific time trends
state_specific<- feols(Prop_votes ~ Simultaneous | Year + Year^State_Name, data  = general_election_B_PNS_SUBNAT, cluster = ~State_Name + ~Year)

#Run model after 1980 with fiscal centralisation
general_election_A_PNS_1980 <- general_election_A_PNS[general_election_A_PNS$Year > 1977,]
general_election_B_PNS_NAT_1980 <- general_election_B_PNS_NAT[general_election_B_PNS_NAT$Year >1977,]
general_election_B_PNS_SUBNAT_1980 <- general_election_B_PNS_SUBNAT[general_election_B_PNS_SUBNAT$Year >1977,]
feols(Nationalised_voting_score ~ Simultaneous + Fiscal.centralisation + Literacy + Per.capita.income| Year + State_Name, data  = general_election_A_PNS_1980, cluster = ~State_Name + ~Year)
feols(Prop_votes ~ Simultaneous + Fiscal.centralisation + Literacy + Per.capita.income| Year + State_Name, data  = general_election_B_PNS_NAT_1980, cluster = ~State_Name + ~Year)
feols(Prop_votes ~ Simultaneous + Fiscal.centralisation + Literacy + Per.capita.income| Year + State_Name, data  = general_election_B_PNS_SUBNAT_1980, cluster = ~State_Name + ~Year)

#Make a lags and leads model
general_election_A_PNS_lag_lead <- general_election_A_PNS %>%
  group_by(State_Constituency) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(one_lag = dplyr::lag(Simultaneous, 1, order_by = Year)) %>%
  mutate(two_lag = dplyr::lag(Simultaneous, 2, order_by = Year)) %>%
  mutate(three_lag = dplyr::lag(Simultaneous,3, order_by = Year)) %>%
  mutate(four_lag = dplyr::lag(Simultaneous,4, order_by = Year))

general_election_A_PNS_lag_lead <- general_election_A_PNS_lag_lead %>%
  group_by(State_Constituency) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(one_lead = dplyr::lead(Simultaneous, 1, order_by = Year)) %>%
  mutate(two_lead = dplyr::lead(Simultaneous, 2, order_by = Year)) %>%
  mutate(three_lead = dplyr::lead(Simultaneous,3, order_by = Year)) %>%
  mutate(four_lead = dplyr::lead(Simultaneous,4, order_by = Year))

#Three term lags and leads
lags_and_leads <- feols(Nationalised_voting_score ~  three_lag + two_lag + one_lag + Simultaneous + one_lead + two_lead + three_lead |Year + State_Name, data = general_election_A_PNS_lag_lead, cluster = ~State_Name + ~Year)
Term <- seq(from = -3, to = 3, by = 1)

plot(x = c(-3, 3), y = c(-0.06, 0.04),
     type = "n",
     ylab = "Coefficient",
     xlab = "General elections before and after treatment")
points(Term, lags_and_leads$coefficients)
arrows(Term, lags_and_leads$coefficients - lags_and_leads$se, Term, lags_and_leads$coefficients + lags_and_leads$se, angle = 90, code = 3, length = 0.05)

#Model with lags
feols(Nationalised_voting_score ~ Simultaneous + Literacy + Per.capita.income + one_lag + two_lag| State_Name + Year, data  = general_election_A_PNS_lag_lead, cluster = ~State_Name + ~Year)



