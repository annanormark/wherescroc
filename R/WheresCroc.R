
#Anna och Marcus
hiddenMarkov=function(bestNodes, edges, readings, moveInfo){
  lastDestination = moveInfo$mem$destination
  lastReadings = moveInfo$mem$readings
  
  newDestination = list(pos = 0, prob = 0)
  #from the best 5 nodes, run hidden markov
  for(node in nrow(bestNodes)) {
    neighbours = getOptions(node, edges)
  
    prob = 0
    for(neighbour in neighbours) {
      transferProb = 1/length(getOptions(neighbour, edges))
      z_score = z_score(neighbour, lastReadings)
      n_prob = transferProb * z_score * node$prob
    
      if (n_prob > prob) {
        prob = n_prob
      }
    }
  
    if (newDestination$prob < prob) {
      newDestination$pos = node
      newDestination$prob = prob
    }
  }
  prob = 0
  lastDestinationChild = list(pos = 0, prob = 0)
  lastDestinationNeighbours = getOptions(lastDestination,edges)
  for(neighbour in lastDestinationNeighbours) {
    transferProb = 1/length(lastDestinationNeighbours)
    z_score = z_score(neighbour, readings)
    n_prob = z_score * (transferProb * lastDestination$prob + ((1 - lastDestination$prob) * (1/(length(getOptions(neighbour, edges)) - 1)))) 
    
    if (n_prob > prob) {
      prob = n_prob
      lastDestinationChild$pos = neighbour
      lastDestinationChild$prob = prob
    }
  }
  
  if (newDestination$prob < lastDestinationChild$prob) {
    newDestination$pos = lastDestinationChild$pos
    newDestination$prob = lastDestinationChild$prob
  }
  
  return(newDestination)
}

#Lukas
findShortestPath=function() {
  
}

#Jonas
#checka dnorm, verkar g??ra detta ganska l??tt! /M&A
z_score=function(readings, probs) {
  A = matrix(
    nrow = 40,
    ncol = 3
  )
  z_list <- c("list",40)
  
  for(i in 1:40){
    z_list[i] = 0
  }
  
  for(i in 1:40){
    A[i,1] = dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2], FALSE)
    A[i,2] = dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2], FALSE) 
    A[i,3] = dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2], FALSE)
    z_list[i] = A[i,1] * A[i,2] * A[i,3]  # Multiplicerar varje rad och l'gger i z_list, 40 element totalt
  }
  
  #print(as.numeric(z_list))
  
  highestValue = 0
  secondHighest = 0
  valueIndex = 0
  for(i in 1:40){
      if((as.numeric(z_list[i])) > highestValue){
          secondHighest = highestValue
          highestValue = as.numeric(z_list[i])
          #print(highestValue)
          #print(z_list[i])
          valueIndex = i
      }
      
  }
  summary = sum(as.numeric(z_list))
  maximum = max(as.numeric(z_list))
  print(valueIndex)
  
  #print("difference")
  #print(highestValue - secondHighest)
  
  #print(maximum)
  #print("round")
  #print(z_list)
  
  #print(summary)
  #print(maximum)
 
  return(maximum)
}

ourWC=function(moveInfo,readings,positions,edges,probs) {
  #z_score on all nodes
  probabilityList = list()
  for(count in nrow(probs[1])) {
    probability = z_score(count)
    c(probabilityList, c(position=count, prob=probability))
  }
  probabilityList = order(lapply(probabilityList, function(x) x[2]), decreasing = TRUE)
  hiddenMarkov(probabilityList[1:5], edges, readings, moveInfo)
  return(moveInfo)
}

tourist_eaten = function(turist_point){
  if(turist_point < 0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


generate_e_matrix =function(){
  
}

prob_edge_movement = function(point,edges){
  options=getOptions(point,edges)
  div_arg = length(options)
  prob = 1/div_arg
  return(prob)
}

#__________________________________________OUR CODE END HERE_____________________________________________________________

#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)  
  #print(moveInfo)
  #print(readings)
  #print(positions)
  #print(edges)
  #rint(probs)
  #rint(A)
  #rint(readings)
  #print("croc at: ")
  z_score(readings, probs)
  return(moveInfo)
}

#' @export
manualWC=function(moveInfo,readings,positions,edges,probs) {
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) { 
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")    
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

testWC = function(moveInfo,readings,positions,edges,probs){
  options=getOptions(7,edges)
  prob_edge = prob_edge_movement(7,edges)
  print(prob_edge)
  print("Move 1 options (plus 0 for search):")
  print(options)  
  
  print(readings)
  points = getPoints()
  print(points)
  
  edges=getEdges()
  print(edges)
  probs=getProbs()
  print(probs)
  
  
}

#' Run Where's Croc
#' 
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park. 
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record 
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also 
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in 
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game. 
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' @param makeMoves Your function that takes five arguments: 
#' (1) A list of information for the move.
#' This has two fiels. The first is a vector of numbers called 'moves', where you will enter 
#' the moves you want to make. You should enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the 
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called 'mem' that you can use to store information you want to remember from turn to turn. 
#' (2) A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current 
#' location. 
#' (3) A vector giving the positions of the two tourists and yourself. If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist 
#' was eaten by Croc in a previous turn, then the position will be NA. 
#' (4) a matrix giving the edges paths between waterholes (edges) present. 
#' (5) a list of three matrices giving the mean and standard deviation of readings for salinity, 
#' phosphate and nitrogen respectively at each waterhole.
#' 
#' Your function should return the first argument passed with an updated moves vector 
#' and any changes to the 'mem' field you wish to access later on.
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Ignore this.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves=randomWC,showCroc=T,pause=1) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list())
  while (!is.na(positions[1])) {
    move=move+1
    positions[1]=sample(getOptions(positions[1],edges),1)
    if (!is.na(positions[2])&&positions[2]>0) {
      positions[2]=sample(getOptions(positions[2],edges),1)
    } else if (!is.na(positions[2]) && positions[2]<0) {
      positions[2]=NA
    }
    if (!is.na(positions[3])&&positions[3]>0) {
      positions[3]=sample(getOptions(positions[3],edges),1)
    } else if (!is.na(positions[3]) && positions[3]<0) {
      positions[3]=NA
    }
    if (!is.na(positions[2]) && positions[2]==positions[1]) {
      positions[2]=-positions[2]
    }
    if (!is.na(positions[3]) && positions[3]==positions[1]) {
      positions[3]=-positions[3]
    }
    plotGameboard(points,edges,move,positions,showCroc)
    
    Sys.sleep(pause)
    
    readings=getReadings(positions[1],probs)
    Sys.sleep(0.5)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          print(paste("Congratualations! You got croc at move ",move,".",sep=""))
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }      
    }
  }
}
#' @export
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @export
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @export
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @export
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @export
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)      
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @export
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}