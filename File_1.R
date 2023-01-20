######################################################
###1.  PLOT 50 DAY MORTALITY RATES FOR HEAT ATTACK###
#####################################################

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

##############################################
###2. FINDING THE BEST HOSPITAL IN A STATE###
#############################################

best <- function(state,outcome){
  ##read the outcome data
  o <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  ##create data frame with only data we are interested in 
  rates <- as.data.frame(cbind(o[,2],
                               o[,7],
                               o[,11],
                               o[,17],
                               o[,23],
                               stringsAsFactors = FALSE))
  
  ##rename columns 
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  ## check that state and outcome are valid 
  if(!state %in% rates[,"State"]){
    stop("Invalid State")
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid Outcome")
  }
  
  ##Create data frame with only data for given state 
  Hopsital_Rates <- rates[(rates[,"State"] == state),]
  
  ##Convert rates in outcome column to numeric
  Hopsital_Rates[,outcome] <- as.numeric(Hopsital_Rates[,outcome])
  
  ## Only keep non na values in outcome column
  Hopsital_Rates <- Hopsital_Rates[!is.na(Hopsital_Rates[,outcome]),]
  
  ## Order by outcome column rate
  Hopsital_Rates <- Hopsital_Rates[order(Hopsital_Rates[,outcome]),]
  
  ## Get names of hospital with lowest rate
  Hopsital_Names <- Hopsital_Rates[Hopsital_Rates[,outcome]== min(Hopsital_Rates[,outcome]),1]
  
  ## Sort by hospital name if tie 
  sort(Hopsital_Names)[1]
  
}

best("TX", "heart attack")
best("TX", "heart failure")

#################################################
###3. Ranking hospitals by outcome in a state###
################################################

rankhospital <- function(state, outcome, num = "best"){
  #read the outcome data
  o <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  ##create data frame with only data we are interested in 
  rates <- as.data.frame(cbind(o[,2],
                               o[,7],
                               o[,11],
                               o[,17],
                               o[,23],
                               stringsAsFactors = FALSE))
  ##rename columns 
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  ## check that state and outcome are valid 
  if(!state %in% rates[,"State"]){
    stop("Invalid State")
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid Outcome")
  }
  
  ##Create data frame with only data for given state 
  Hopsital_Rates <- rates[(rates[,"State"] == state),]
  
  ##Convert rates in outcome column to numeric
  Hopsital_Rates[,outcome] <- as.numeric(Hopsital_Rates[,outcome])
  
  ## Only keep non na values in outcome column
  Hopsital_Rates <- Hopsital_Rates[!is.na(Hopsital_Rates[,outcome]),]
  
  ##convert num argument to valid rank
  if(num =="best"){
    num <-1
  }
  if(num =="worst"){
    num <- nrow(Hopsital_Rates)
  }
  
  ## Order by outcome column rate & hopsital name 
  Hopsital_Rates <- Hopsital_Rates[order(Hopsital_Rates[, outcome], Hopsital_Rates[, "Hospital"]), ]
  
  ## Get names of hospital at given ranking 
  Hopsital_Rates[num,1]
}

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")



#################################################
###4. Ranking hospitals in all states###########
################################################

rankall <- function(outcome, num = "best") {
  #read the outcome data
  o <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = TRUE)
  
  ##create data frame with only data we are interested in 
  rates <- as.data.frame(cbind(o[,2],
                               o[,7],
                               o[,11],
                               o[,17],
                               o[,23],
                               stringsAsFactors = FALSE))
  ##rename columns 
  colnames(rates) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  
  ## check that outcome is valid 
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("Invalid Outcome")
  }
  
  ##Return hopsital in each state that matches the ranking for the given outcome. Then add this hopsital to hrank data frame 
  hrank <-data.frame()
  
  for(state in sort(unique(rates[,"State"]))){
    ##Create data frame with only data for given state 
    Hopsital_Rates <- rates[(rates[,"State"] == state),]
    
    ##Convert rates in outcome column to numeric
    Hopsital_Rates[,outcome] <- as.numeric(Hopsital_Rates[,outcome])
    
    ## Only keep non na values in outcome column
    Hopsital_Rates <- Hopsital_Rates[!is.na(Hopsital_Rates[,outcome]),]
  
  
  ## convert num argument to valid rank 
    if(num =="best"){
      rnum <-1
    } else if(num =="worst"){
      rnum <-nrow(Hopsital_Rates)
    }
    else {rnum = num}
    
    ## Order by outcome column rate & hopsital name 
    Hopsital_Rates <- Hopsital_Rates[order(Hopsital_Rates[, outcome], Hopsital_Rates[, "Hospital"]), ]
    
    ## Get names of hospital at given ranking 
    hName <- Hopsital_Rates[rnum,1]
   
    hrank <- rbind(hrank, data.frame(hopsital = hName, state = state))
  }
  hrank
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)