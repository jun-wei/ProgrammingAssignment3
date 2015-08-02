best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        # read file into vector
        readfile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # subset the outcome vector into data with the data required
        
        
        colnames(readfile)[11] <- "heart attack"
        colnames(readfile)[17] <- "heart failure"
        colnames(readfile)[23] <- "pneumonia"
        
        data <- readfile[c(2,7,11,17,23)]
        # data2 <- data[order('heart attack','heart failure','pneumonia')]
        
        #lowercase the state and do check layer here
        #state <- tolower(state)
        
        if (!state %in% data$State)
        {
                stop("invalid state")
        }
        
        if ((!outcome %in% names(data)[3])&(!outcome %in% names(data)[4])&(!outcome %in% names(data)[5]))
        {
                stop("invalid outcome")
        }
        Subset_data <- data[data$State == state,]
        #remove NA
        bad <- is.na(Subset_data)
        desired_data <- Subset_data[!bad, ]
        #remove Not Available data
        desired_data2 <- desired_data[which(desired_data[,outcome]!="Not Available"),]
        #subset specific outcome and find min value
        Subset_data3 <- desired_data2[c('Hospital.Name','State',outcome)]
        Subset_data4 <- which.min(as.double(Subset_data3[,outcome]))
                
        head(Subset_data3[Subset_data4,"Hospital.Name"])
}