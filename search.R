PriorityQueue <- function() {
  keys <- values <- NULL
  insert <- function(key, value) {
    ord <- findInterval(key, keys)
    keys <<- append(keys, key, ord)
    values <<- append(values, value, ord)
  }
  pop <- function() {
    head <- list(key=keys[1],value=values[[1]])
    values <<- values[-1]
    keys <<- keys[-1]
    return(head)
  }
  empty <- function() length(keys) == 0
  environment()
}
 
# TODO
# 1. dependencies priority queue
# 2. arguments validation
# 3. function which returns dataframe with list of cities and total hours
# 4. print result

args = commandArgs(trailingOnly=TRUE)
start = args[1];
end = args[2];
fileName = args[3];

# returns dataFrame with name and children array 
# getCityData = function(data, cityName, neighbours) {
# 	cityData = subset(data, X == cityName, select = neighbours)
# 	return(cityData)
# }

# returns pathlen, pathtime and distance
getTravelData = function(cityInfo) {
	destinationInfo = cityInfo
	names(destinationInfo) = c("city")
	stringified = toString(destinationInfo$city)
	listified = strsplit(stringified, "\\|")
	numerical = as.numeric(listified[[1]])
	return(numerical)
}


astar = function(start, end, fileName) {
	data = read.csv(fileName)
	# Veliko Turnovo -> Veliko.Turnovo
	cities = gsub(" ", ".", data$X)

	visitedTable = rep("none", length(cities))
	names(visitedTable) = cities

	notvisited = cities[!cities == start]

	firstToSecond = subset(data, X == start, select=c(end))
	travelData = getTravelData(firstToSecond)

	g_score = 0
	f_score = travelData[3]

	pq = PriorityQueue()

	pq$insert(g_score+f_score, start)

	before = "started"
	
	while(!pq$empty()) {
		top = pq$pop()
		
		visitedTable[top$value] = before

		# does this check work?
		if(top == end) {
			return(recontructPath(came_from, end))
		}

		neighbours = subset(data, X == start, select=notvisited)

		reachable = Filter(function(v) {
			vectorized = as.vector(v)
			hasPath = grep("(-1)", vectorized)
			length(hasPath) < 1
		}, neighbours)

		insertCity = function(x, output) {
			wellName <- x[1]
			plateName <- x[2]
			wellID <- 1
			print(paste(wellID, x[3], x[4], sep=","))
			cat(paste(wellID, x[3], x[4], sep=","), file= output, append = T, fill = T)
			}

		apply(d, 1, f, output = 'outputfile')

		print(reachable)
	}
}

astar(start, end, fileName)

