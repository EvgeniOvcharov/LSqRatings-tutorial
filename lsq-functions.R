#The file contains functions that implement the Least Squares Rating System

# This function generates the matrix and free term 
# (and the names of the vector components, which are typically team names)
# of the normal equations

lsq.system.ha = function(x, ha) {
  teams = sort(unique(c(x$Winner, x$Loser)))
  d = length(teams)
  #initial game matrix
  game = matrix(0, nrow = d, ncol = d)
  #initial point difference vector
  pd = numeric(d)
  
  for(k in 1:nrow(x)){
    i = match(x$Winner[k], teams)
    j = match(x$Loser[k], teams)
    h = x$HA[k]
    y = x$PD[k]
    
    game[i,j] = game[i,j] - 1
    game[j,i] = game[i,j]
    game[i,i] = game[i,i] + 1
    game[j,j] = game[j,j] + 1
    
    pd[i] = pd[i] + y - h*ha
    pd[j] = pd[j] - y + h*ha
  }
  
  l = list(game = game, pd = pd, names = teams)
  class(l) = c("lsq.system", "list")
  l
}

# This function computes the Least Squares Ratings with known value of the home advantage parameter
# It rank-orders the team according to rating and presents the result as a data table 

lsq.ratings.ha = function(x, ha) {
  #define the variables
  l = lsq.system.ha(x, ha)
  d = length(l$pd)
  game = l$game
  pd = l$pd
  teams = l$names
  
  #modify the linear system by adding a linear constraint and solve it
  game[d, ] = rep(1,d)
  pd[d] = 0
  sol = solve(game,pd)
  
  #present the solution by ordering the teams by ratings
  rorder = order(rank(sol[1:d]), decreasing = T)
  rankings = data.table(team = teams[rorder], rating = round(sol[rorder],2))
  rankings
}

