# Orgina³
dayloop2 <- function(temp){
    for (i in 1:nrow(temp)){    
        temp[i,10] <- i
        if (i > 1) {             
            if ((temp[i,6] == temp[i-1,6]) & (temp[i,3] == temp[i-1,3])) { 
                temp[i,10] <- temp[i,9] + temp[i-1,10]                    
            } else {
                temp[i,10] <- temp[i,9]                                    
            }
        } else {
            temp[i,10] <- temp[i,9]
        }
    }
    names(temp)[names(temp) == "V10"] <- "Kumm."
    return(temp)
}

dayloop2_A <- function(temp){
	res <- numeric(nrow(temp))
    for (i in 1:nrow(temp)){    
        res[i] <- i
        if (i > 1) {             
            if ((temp[i,6] == temp[i-1,6]) & (temp[i,3] == temp[i-1,3])) { 
                res[i] <- temp[i,9] + res[i-1]                   
            } else {
                res[i] <- temp[i,9]                                    
            }
        } else {
            res[i] <- temp[i,9]
        }
    }
    temp$`Kumm.` <- res
    return(temp)
}

dayloop2_AB <- function(temp){
	res <- numeric(nrow(temp))
    for (i in 1:nrow(temp)){    
       # res[i] <- i
        if (i > 1) {             
            if ((temp[i,6] == temp[i-1,6]) & (temp[i,3] == temp[i-1,3])) { 
                res[i] <- temp[i,9] + res[i-1]                   
            } else {
                res[i] <- temp[i,9]                                    
            }
        } else {
            res[i] <- temp[i,9]
        }
    }
    temp$`Kumm.` <- res
    return(temp)
}

# Shane
f_Shane <- function(temp) {
	idx <- 1:nrow(temp)
	temp[,10] <- idx
	idx1 <- c(FALSE, (temp[-nrow(temp),6] == temp[-1,6]) & (temp[-nrow(temp),3] == temp[-1,3]))
	temp[idx1,10] <- temp[idx1,9] + temp[which(idx1)-1,10] 
	temp[!idx1,10] <- temp[!idx1,9]    
	temp[1,10] <- temp[1,9]
	names(temp)[names(temp) == "V10"] <- "Kumm."
	temp	
}

dayloop2_B <- function(temp){
	cond <- c(FALSE, (temp[-nrow(temp),6] == temp[-1,6]) & (temp[-nrow(temp),3] == temp[-1,3]))
	res <- temp[,9]
    for (i in 1:nrow(temp)) {
        if (cond[i]) res[i] <- temp[i,9] + res[i-1]
    }
    temp$`Kumm.` <- res
    return(temp)
}

dayloop2_BB <- function(temp){
	cond <- c(FALSE, (temp[-nrow(temp),6] == temp[-1,6]) & (temp[-nrow(temp),3] == temp[-1,3]))
	res <- temp[,9]
    for (i in (1:nrow(temp))[cond]) {
        res[i] <- temp[i,9] + res[i-1]
    }
    temp$`Kumm.` <- res
    return(temp)
}

dayloop2_C <- function(temp){
	cond <- c(FALSE, (temp[-nrow(temp),6] == temp[-1,6]) & (temp[-nrow(temp),3] == temp[-1,3]))
	res <- temp[,9]
    for (i in 1:nrow(temp)) {
        if (cond[i]) res[i] <- res[i] + res[i-1]
    }
    temp$`Kumm.` <- res
    return(temp)
}

dayloop2_D <- function(temp){
	cond <- c(FALSE, (temp[-nrow(temp),6] == temp[-1,6]) & (temp[-nrow(temp),3] == temp[-1,3]))
	res <- temp[,9]
    for (i in (1:nrow(temp))[cond]) {
        res[i] <- res[i] + res[i-1]
    }
    temp$`Kumm.` <- res
    return(temp)
}

