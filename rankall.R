rankall <- function(outcome, num=1) {
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
        
       
        if ((!outcome %in% names(data)[3])&(!outcome %in% names(data)[4])&(!outcome %in% names(data)[5]))
        {
                stop("invalid outcome")
        }
        #Subset_data <- data[data$State == state,]
        #remove NA
        bad <- is.na(data)
        desired_data <- data[!bad, ]
        #remove Not Available data
        desired_data2 <- desired_data[which(desired_data[,outcome]!="Not Available"),]
        #subset specific outcome and find rank
        Subset_data3 <- desired_data2[c('Hospital.Name','State',outcome)]
        #sorted
        Sorted_Subset_data3 <- Subset_data3[order(as.numeric(Subset_data3[[outcome]]),Subset_data3[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
        # never include outcome numeric result because it is sorted, right now is just to get ranking from every state
        Split_States <- split(Sorted_Subset_data3[,c("Hospital.Name")], Sorted_Subset_data3$State)
        
        rankhospital <- function(x, num)
                {       
                        # if rank is more than row, NA is returned automatically
                        if(num == "best"){
                        x[1]
                        }
                        if(num == "worst"){ 
                        tail(x,1)
                        }
                        else
                        x[num]

                }
        rank_in_States <- lapply(Split_States, rankhospital, num)
        finaldata <- data.frame(hospital = unlist(rank_in_States), state=names(rank_in_States), row.names = names(rank_in_States))
        
}