#' return_node_with_highest_f
#' Returns the node with highest f value.
return_node_with_highest_f=function(f_list){
  highestValue = -100000
  valueIndex = 1
  for(node in 1:40){
    if(highestValue < f_list[node]){
      highestValue = f_list[node]
      valueIndex = node
    }
    best_node = valueIndex
  }
  return(best_node)
}

#' generate_new_f
#' Performs a forward algorithm calculation for a given node, returning
#' the current probability.
generate_new_f = function(node,current_density_scores,past_f_list,edges){
  neighbours=getOptions(node,edges)
  new_f=0
  for(neigh in neighbours){
    new_f = new_f + (past_f_list[neigh]*transition_prob(neigh,edges)) 
  }
  new_f=new_f*current_density_scores[node]
  return(new_f)
}

#' @param hiddenMarkov
#' Performs one iteration of a hidden markov sequence calculation 
#' using the forward algorithm, returning the node with the highest probability
#' within the current sequence of steps.
hiddenMarkov=function(edges, readings, moveInfo,probs){
  #retrieve past f values
  past_f_list = moveInfo$mem$past_f
  #get new normalized probability density list
  current_density_scores = normalized_dnorm_list(readings,probs)
  #update f values for all nodes
  new_f_list=numeric()  
  for(node in 1:40){
    new_f_list[node] = generate_new_f(node,current_density_scores,past_f_list,edges)
  }
  #normalize new_f_list
  f_sum=sum(new_f_list,na.rm=TRUE)
  for(node in 1:40){
    new_f_list[node]= new_f_list[node]/f_sum
  }
  #pick node with highest f
  new_dest = return_node_with_highest_f(new_f_list)
  #update moveInfo
  moveInfo$mem$destination = new_dest
  moveInfo$mem$past_f = new_f_list
  return(moveInfo)
}

#' bfs
#' Performs bfs and returns a numerical vector prev representing the
#' parent/child relationship of nodes encountered during the bfs search.
#' For a child node A, prev[node] will return its parent node B.
bfs=function(node,dest,edges){
  visited = c(node)
  open = c(node)
  prev= numeric()
  prev[node]=-1
  current = node
  while(length(open) != 0){
    current=head(open,n=1)#head
    open =setdiff(open,c(current))#dequeue head
    neigh = getOptions(current,edges)#get children
    neigh= setdiff(neigh,c(current))#remove current from children
    neigh = setdiff(neigh,visited)#remove nodes already visited from niegh
    for(n in neigh){#for each unvisited neighbour of current
      if(!(n %in% visited)){#if n not in visited
        open=c(open,n)#queue n in open
        prev[n]=current#set current as parent node to n
        visited=c(visited,c(n))#append n in visited
      }
    }
  }
  return(prev)
}

#' findShortestPath
#' Performs bfs and returns a numerical vector
#' of nodes representing the shortest path from point to dest.
findShortestPath=function(point,dest,edges){
  prev_list = bfs(point,dest,edges)
  shortest_path = c(dest)
  current=dest
  #iterate through prev_list to from child to parent until no parents remain
  while(current != -1){
    if(prev_list[current] != -1){
      shortest_path= c(prev_list[current],shortest_path)
    }
      current=prev_list[current]
  }
  return(shortest_path)
}

#' highest_prob_nodes
#' Returns a numerical vector representing the ten nodes having the 
#' highest probability density values.
highest_prob_nodes=function(list){
  best_nodes = numeric()
  highestValue = 0
  valueIndex = 0
  for(j in 1:10){
    for(i in 1:40){
      if(highestValue < list[i]){
        highestValue = list[i]
        valueIndex = i
      }
    }
    best_nodes <- c(best_nodes, valueIndex)
    list[valueIndex] = 0
    highestValue = 0
  }
  return(best_nodes)
}

#' normalized_dnorm_list
#' Returns a numerical vector containing the normalized
#' probability density values from the readings made by the croc.
normalized_dnorm_list=function(readings, probs) {
  A = matrix(
    nrow = 40,
    ncol = 3
  )
  #save dnom results in matrix A and multiply all column values
  dnorm_list = numeric()
  for(i in 1:40){
    A[i,1] = dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2], FALSE)
    A[i,2] = dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2], FALSE) 
    A[i,3] = dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2], FALSE)
    dnorm_list[i] = A[i,1] * A[i,2] * A[i,3]
  }
  #normalize dnorm_list
  sum_z = sum(dnorm_list)
  for(i in 1:40){
    dnorm_list[i]=dnorm_list[i]/sum_z
  }
  return(dnorm_list)
}

#' tourist_eaten
#' If tourist has been eaten then return true, 
#' else return false.
tourist_eaten = function(turist_point){
  if(!is.na(turist_point) && turist_point < 0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#' transition_prob
#' Return the transition probability for a specific node.
transition_prob = function(node,edges){
  options=getOptions(node,edges)
  div_arg = length(options)
  prob = 1/div_arg
  return(prob)
}
#' deepHouseWC
#' Performs a combination of the hidden markov forward algorithm and bfs search
#' to find the croc in as few moves as possible.
deepHouseWC = function(moveInfo,readings,positions,edges,probs){

  #init moveInfo$mem
  init_f=numeric()
  if(!("destination"  %in% names(moveInfo$mem))){
    #init density_score_list
    density_score_list=normalized_dnorm_list(readings,probs)
    #init f_list with initial probabilitys
    for(i in 1:40){
      init_f[i]=(1/40)*density_score_list[i]
    }
    #normalize init_f
    sum_init = sum(init_f)
    for(i in 1:40){
      init_f[i]=init_f[i]/sum_init
    }
    #select initial destination node 
    best_nodes = highest_prob_nodes(density_score_list)
    best_node=best_nodes[1]
    moveInfo$mem = list(destination=best_node,past_f=init_f) #init mem
  }
  
  #if tourist A eaten, set all f_values to zero, and then set f_value for tourist A to 1
  if(tourist_eaten(positions[1])){
    for(i in 1:40){
      init_f[i]=0
    }
    init_f[abs(positions[1])]=1
    moveInfo$mem$past_f=init_f
  }
  
  #if tourist B eaten, set all f_values to zero, and then set f_value for tourist B to 1
  if(tourist_eaten(positions[2])){
    for(i in 1:40){
      init_f[i]=0
    }
    init_f[abs(positions[2])]=1
    moveInfo$mem$past_f=init_f
  }

  #perform HM, save new destinaion and past f values in moveInfo
  moveInfo = hiddenMarkov(edges,readings,moveInfo,probs)
  
  #update destination and pick shortest path
  new_dest_node = moveInfo$mem$destination
  shortest_path = findShortestPath(positions[3],new_dest_node,edges)

  #two steps away or more from destination
  if(length(shortest_path) >= 3){
    moveInfo$moves = c(shortest_path[2],shortest_path[3])
  }
  
  #one step away from destination
  if(length(shortest_path) == 2){
    moveInfo$moves = c(shortest_path[2],0)
  }
  
  #if destination amoung neighbours, go to neighbour
  neigh = getOptions(positions[3],edges)
  if(new_dest_node %in% neigh){
    moveInfo$moves = c(new_dest_node,0)
  }
  
  #if at destination: dont move, just check for croc
  if(length(shortest_path) == 1){
    moveInfo$moves=c(0,0)  
  }
  return(moveInfo)
}

#' averageTest
#' Used for testing
averageTest <- function(tests){
  sum = 0
  for (i in 1:tests) {
    sum=sum+runWheresCroc(makeMoves=deepHouseWC,showCroc=F,pause=0)
    if(i%%10==0){
      print(i)
      print(sum/i)
    }
  }
  print(sum/i)
  return(0)
}


#__________________________________________OUR CODE ENDS HERE_____________________________________________________________

#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)  
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
runWheresCroc=function(makeMoves=deepHouseWC,showCroc=T,pause=1) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list())
  while (!is.na(positions[1])) {
    move=move+1
    positions[1]=sample(getOptions(positions[1],edges),1)
    
   # print(paste("CrocPos: ", positions[1]))
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