SimAnt <- function(nSimulations, MaxMoves, Question){
  
  PossibleMoves <- c("Up","Down", "Left", "Right")
  TotalMoves <- 0
  Diverge <- FALSE
  
  for (i in 1:nSimulations){
    Coords <- c(0,0)
    names(Coords) <- c("x","y")
    NumberMoves <- 0
    
    while (Condition(Coords, Question) == TRUE & NumberMoves < MaxMoves){ 
      Move <- sample(PossibleMoves, 1, prob = c(rep(1/4,4)))
      NumberMoves = NumberMoves + 1
      
      if (Move == "Up") Coords["y"] = Coords["y"] + 10
      if (Move == "Down") Coords["y"] = Coords["y"] - 10
      if (Move == "Left") Coords["x"] = Coords["x"] - 10
      if (Move == "Right") Coords["x"] = Coords["x"] + 10
      
      if (NumberMoves == MaxMoves){
        Diverge <- TRUE
        print(paste("Reached", MaxMoves, "moves, box is likely not bounded and avg moves diverges", sep=" "))
        break
      }
    }
    if (Diverge){
      print("Stopping...")
      break
    }
    TotalMoves <- TotalMoves + NumberMoves
  }
  
  if (!Diverge){
    print(paste("Took",TotalMoves/nSimulations,"on average", sep=" "))
    return(TotalMoves/nSimulations)
  }
}

Condition <- function(Coords, Question){
  
  if (Question == 1){
    if(Coords["x"] != 20 & 
       Coords["y"] != 20 & 
       Coords["x"] != -20 & 
       Coords["y"] != -20){
      return(TRUE)} else return(FALSE)
  }
  
  if (Question == 2){
    if (Coords["x"] + Coords["y"] < 10) return(TRUE) else return(FALSE)
  }
  
  if (Question == 3){
    if ((((Coords["x"]-2.5)/30)*2 + ((Coords["y"]-2.5)/40)*2) < 1) {
      return(TRUE)} else return(FALSE)
  }
}


# Question 1 
SimulationResult <- SimAnt(nSimulations = 1000, MaxMoves = 10000, Question = 1)

# Question 2 
SimulationResult <- SimAnt(nSimulations = 1000, MaxMoves = 10000, Question = 2)

# Question 3 
SimulationResult <- SimAnt(nSimulations = 1000, MaxMoves = 10000, Question = 3)
