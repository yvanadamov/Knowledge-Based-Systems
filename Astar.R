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

printPath = function(eurTable, end) {
	current = end
	visited = c()

	while(!current == "start") {
		visited = c(current, visited)
		current = eurTable[current,"cameFrom"]
	}

	print(visited)
	# uncomment to see the map at the end of algorith
	# print(eurTable)

	return(visited)
}

astar = function(start, end, fileName) {
	data = read.csv(fileName)
	# Veliko Turnovo -> Veliko.Turnovo
	data$X = gsub(" ", ".", data$X)
	
	cities = data$X

	# create table with cities euristics and anchors info
	euristics = apply(data[end], 1, function(x) {
		g = strsplit(x[end], '[|]')[[1]][3]
		return(as.integer(g))
	})

	euristicsTable = data.frame(euristic = euristics)

	euristicsTable$cameFrom = "none"
	euristicsTable$totalPathLen = 9999999
	euristicsTable$totalPathTime = 9999999
	
	rownames(euristicsTable) = cities

	# intial values for staring row
	euristicsTable[start, "totalPathLen"] = 0
	euristicsTable[start, "totalPathTime"] = 0
	euristicsTable[start, "cameFrom"] = "start"
	
	print(euristicsTable)

	g_score = 0
	f_score = euristicsTable[start, "euristic"]

	pq = PriorityQueue()

	pq$insert(g_score+f_score, start)

	notvisited = cities

	while(!pq$empty()) {
		top = pq$pop()
		current = top$value
		# Uncomment to view the elemetn with lowest priority on each step
		# print(paste0("Top: ", current))
		# print(paste0("Priority: ", top$key))
		
		if(current == end) {
			return(printPath(euristicsTable, end))
		}

		notvisited = notvisited[!notvisited == current]
		neighbours = subset(data, X == current, select=notvisited)

		reachable = Filter(function(v) {
			vectorized = as.vector(v)
			hasPath = grep("(-1)", vectorized)
			length(hasPath) < 1
		}, neighbours)

		# uncomment to see information about 
		# the destination between current element and reachable ones
		# print(reachable)

		for(city in names(reachable)) {
			cityInfo = as.vector(reachable[,city])
			cityData = strsplit(cityInfo, '[|]')[[1]]
			numCityData = as.integer(cityData)

			euristicsTable[city, "cameFrom"] = current

			timeFromAncestor = euristicsTable[current, "totalPathTime"] + numCityData[2]
			euristicsTable[city, "totalPathTime"] = timeFromAncestor
			
			lengthFromAncestor = euristicsTable[current, "totalPathLen"] + numCityData[1]
			euristicsTable[city, "totalPathLen"] = lengthFromAncestor
			
			euristic = euristicsTable[city, "euristic"]

			priority = lengthFromAncestor + euristic
			pq$insert(priority, city)

			# uncomment to see detailed info about adding to queue
			# print(paste0("Added city: ", city))
			# print(paste0("Priority: ", priority))	
			# print(paste0("euristic: ", euristicsTable[city, "euristic"]))
			# print(cityData)
			# print(paste0("Parent total len: ", euristicsTable[current, "totalPathLen"]))	
		}
		# optimization
		notvisited = notvisited[!notvisited %in% names(reachable)]
	}
}

astar(start, end, fileName)