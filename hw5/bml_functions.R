#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  combined = floor( p *( r * c ))
  Cars = sample(1:(r * c), combined, replace = FALSE)
  x = ceiling(Cars / r )
  y = (Cars %% r ) + 1
  m = data.frame(x, y)
  colors = 1:nrow(m)
  colors[sample(1:nrow(m), nrow(m)/2)] = "Red"
  colors[colors != "Red"] = "Blue"
  m = cbind(m, colors)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

# The move_north function sets movement of the blue particles to the north.
move_north = function(m, r) {
  north = m[order(m$x, m$y), ]
  dfnorth = data.frame(diff(north$x),diff(north$y))
  dfnorth1 = dfnorth[1] == 0
  dfnorth2 = dfnorth[2] == 1
  block_north = which(dfnorth1 & dfnorth2)
  top = subset(north,north$x %in% north$x[north$y == r & north$colors == "Blue"])
  cond = any(top$y == 1)
  if (cond) {
    block_north = c(block_north, which(north$x %in% top$x[top$y == 1] & north$y == r))
  }
  find_red = which(m$colors == "Red")
  numeric_form = -c(as.numeric(row.names(north[block_north, ])), find_red)
  m$y[numeric_form] = m$y[numeric_form] + 1
  row_find = (m$y == (r+1))
  m$y[row_find] = 1
  return(m)
}

# The move_e function sets the movement of red dots to the east.
move_east = function(m, c) {
  m_east = m[order(m$y, m$x), ]
  dfeast = data.frame(diff(m_east$x),diff(m_east$y))
  dfeast1 = dfeast[1] == 1
  dfeast2 = dfeast[2] == 0
  eblock = which(dfeast1&dfeast2)
  c_x_finder = m_east$x == c & m_east$colors == "Red"
  m_edge = subset(m_east,m_east$y %in% m_east$y[c_x_finder])
  cond = any(m_edge$x == 1)
  if (cond) {
    egridx = (m_east$x == c)
    eblock = c(eblock, which(m_east$y %in% m_edge$x[m_edge$x == 1] & egridx))
  }
  find_blue = m$colors == "Blue"
  numeric_form = -c(as.numeric(row.names(m_east[eblock, ])), which(find_blue))
  m$x[numeric_form] = m$x[numeric_form] + 1
  col_find = m$x == (c+1)
  m$x[col_find] = 1
  return(m)
}
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

# sim function creates the graph and provides the "block rate", given
bml_sim = function(r,c,p,mv){
  m = bml.init(r,c,p)
  index = 0
  for (i in 1:mv){
    if (i %% 2 == 1){
      m = move_east(m,c)
    } else {
      m = move_north(m,r)
    }
    index = i
  }
  title = c("BML Simulation with p =", p, "after", index/2, "steps")
  plot(m$x, m$y, pch = 15, col = as.character(m$colors), cex = 0.6, xlab = "X locdinates", ylab = "Y locdinates", main = title)
  
  north = m[order(m$x, m$y), ]
  dfnorth = data.frame(diff(north$x),diff(north$y))
  dfnorth1 = (dfnorth[1] == 0)
  dfnorth2 = (dfnorth[2] == 1)
  block_north = which(dfnorth1 & dfnorth2)[north$colors[which(dfnorth1 & dfnorth2)] == "Blue"]
  top = subset(north,north$x %in% north$x[north$y == r & north$colors == "Blue"])
  cond = any(top$y == 1)
  if (cond) {
    yGrid = north$y == r
    block_north = c(block_north, which(north$x %in% top$x[top$y == 1] & yGrid))
  }
  
  m_east = m[order(m$y, m$x), ]
  df_east = data.frame(diff(m_east$x),diff(m_east$y))
  eDiff1 = df_east[2] == 0
  eDiff2 = df_east[1] == 1
  eblock = (which(eDiff1 & eDiff2))[m_east$colors[which(eDiff1 & eDiff2)] == "Red"]
  m_edge = subset(m_east,m_east$y %in% m_east$y[m_east$x == c & m_east$colors == "Red"])
  cond = any(m_edge$x == 1)
  if (cond) {
    eblock = c(eblock, which(m_east$y %in% m_edge$x[m_edge$x == 1] & m_east$x == c))
  }
  block = length(block_north) + length(eblock)
  return(block/(r*c*p))
}