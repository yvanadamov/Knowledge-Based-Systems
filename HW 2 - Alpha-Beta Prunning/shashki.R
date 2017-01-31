getHeuristic = function(state) {
	x = 0
	xx = 0
	o = 0
	oo = 0

	rows = 1 : nrow(state)
	cols = 1 : ncol(state) 
	
	for(i in rows) {
		for(j in cols) {
			switch(state[i, j],
				X = {x = x + 1},
				Xx = {xx = xx + 1},
				O = {o = o + 1},
				Oo = {oo = oo + 1}

			)
		}
	}

	if(o + oo < 1) {
		return(30)
	}

	return(x - o + 2*(xx-oo))
}

getPools = function(state, maximizing, singles) {
	pools = c()
	if(maximizing) {
		if(singles) {
			pools = "X"
		}
		else {
			pools = "Xx"
		}
	}
	else {
		if(singles) {
			pools = "O"
		}
		else {
			pools = "Oo"
		}
	}


	rows = 1 : nrow(state)
	cols = 1 : ncol(state)
	cords = list()
	index = 1

	for(i in rows) {
		for(j in cols) {
			if(state[i, j] == pools) {
				cords[[index]] = c(i, j)
    			index= index + 1
			}
		}
	}
	return(cords)
}

getDiagonals = function(pool, doubles, maximizing) {
	x = pool[1]
	y = pool[2]

	first = list()
	firstIndex = 1
	second = list()
	secondIndex = 1
	thirth = list()
	thirthIndex = 1
	fourth = list()
	fourthIndex = 1

	# first quadrant
	if(x > 1 && y > 1) {
		i = x
		j = y

		while(i > 0 && j > 0) {
			first[[firstIndex]] = c(i, j)
			firstIndex = firstIndex + 1
			i = i - 1
			j = j - 1
		}
	}


	# second
	if(x > 1 && y < 8) {
		i = x
		j = y

		while(i > 0 && j < 9) {
			second[[secondIndex]] = c(i, j)
			secondIndex = secondIndex + 1
			i = i - 1
			j = j + 1
		}
	}

	
	# thirth
	if(x < 8 && y < 8) {
		i = x
		j = y

		while(i < 9 && j < 9) {
			thirth[[thirthIndex]] = c(i, j)
			thirthIndex = thirthIndex + 1
			i = i + 1
			j = j + 1
		}
		
	}


	# fourth
	if(x < 8 && y > 1) {
		i = x
		j = y

		while(i < 9 && j > 0) {
			fourth[[fourthIndex]] = c(i, j)
			fourthIndex = fourthIndex + 1
			i = i + 1
			j = j - 1
		}
	}

	if(doubles) {
		l = list(first, second, thirth, fourth)
	}
	else if(maximizing) {
		l = list(thirth, fourth)
	}
	else {
		l = list(first, second)
	}

	ll = l[lapply(l, length) > 0]
	return(ll)
}

# [jumps, hasRemoved, state, end cords]
getNextState = function(state, diagonal, pool) {
	xx = c("X", "Xx")
	opposite = c()
	empty = "_"

	if(pool %in% xx) {
		opposite = c("O", "Oo")
	}
	else {
		opposite = xx
	}

	#  empty current position
	first = diagonal[[1]]
	firstI = first[1]
	firstJ = first[2]

	state[firstI, firstJ] = empty

	current = diagonal[[2]]
	currentI = current[1]
	currentJ = current[2]

	removed = (state[currentI, currentJ] %in% opposite) 
	
	state[currentI, currentJ] = pool
		
	if(!removed || length(diagonal) < 3) {
		return(list(0, removed, state, current))
	}

	nextStep = diagonal[[3]]
	nextStepI = nextStep[1]
	nextStepJ = nextStep[2]

	# no jumps
	if(!state[nextStepI, nextStepJ] == empty) {
		return(list(0, removed, state, current))	
	}

	# first jump
	jumps = 1
	state[currentI, currentJ] = empty
	state[nextStepI, nextStepJ] = pool


	index = 4
	while(index < length(diagonal) - 1) {
		current = diagonal[[index]]
		currentI = current[1]
		currentJ = current[2]
		
		nextStep = diagonal[[index + 1]]
		nextStepI = nextStep[1]
		nextStepJ = nextStep[2]
		
		notOpposite = (!state[currentI, currentJ] %in% opposite)
		
		if(notOpposite || !state[nextStepI, nextStepJ] == empty) {
			break
		}

		# empty the previous
		first = diagonal[[index - 1]]
		firstI = first[1]
		firstJ = first[2]

		
		state[firstI, firstJ] = empty
		state[currentI, currentJ] = empty
		state[nextStepI, nextStepJ] = pool

		jumps = jumps + 1
		index = index + 2
	}
	return(list(jumps, TRUE, state, nextStep))
}

filterStates = function(states) {
	maxJumps = 0
	for(i in 1:length(states)) {
		jumps = states[[i]][1][[1]]

		if(jumps > maxJumps) {
			maxJumps = jumps
		}
	}

	filtered = list()
	index = 1

	# filter the best jumps
	if(maxJumps > 0) {
		for(i in 1:length(states)) {
			jumps = states[[i]][1][[1]]
			
			if(jumps == maxJumps) {
				filtered[[index]] = states[[i]]
				index = index + 1
			}
		}
		return(filtered)
	}

	# no jumps, filter only states with removing turns
	for(i in 1:length(states)) {
		hasRemoved = states[[i]][2][[1]]
		if(hasRemoved == TRUE) {
			filtered[[index]] = states[[i]]
			index = index + 1
		}
	}

	# no removing turns
	if(index > 1) {
		return(filtered)
	}
	else {
		return(states)
	}
}

getChildren = function(state, maximizing) {
	states = list()
	index = 1
	
	singles = getPools(state, maximizing, TRUE)
	
	if(length(singles) > 0) {
		for(i in 1:length(singles)) {
			cords = singles[[i]]
			pool = state[cords[1], cords[2]]

			diagonals = getDiagonals(cords, FALSE, maximizing)

			if(length(diagonals) < 1) {
				next
			}

			for(j in 1:length(diagonals)) {
				diagonal = diagonals[[j]]

				stateInfo = getNextState(state, diagonal, pool)
				stateInfo[[5]] = cords

				states[[index]] = stateInfo
				index = index + 1
			}
		}
	}


	doubles = getPools(state, maximizing, FALSE)
	if(length(doubles) > 1) {
		for(i in 1:length(doubles)) {
			cords = doubles[[i]]
			pool = state[cords[1], cords[2]]

			diagonals = getDiagonals(cords, TRUE, maximizing)

			if(length(diagonals) < 1) {
				next
			}

			for(j in 1:length(diagonals)) {
				diagonal = diagonals[[j]]

				stateInfo = getNextState(state, diagonal, pool)
				stateInfo[[5]] = cords

				states[[index]] = stateInfo
				index = index + 1
			}
		}
	}

	filteredStates = filterStates(states)
	return(filteredStates)
}

alphabeta = function(node, depth, alpha, beta, maximizing) {
	if(depth == 0) {
		return(getHeuristic(node))
	}

	children = getChildren(node, maximizing)
	
	if(maximizing) {
		v = -1000

		for(i in 1:length(children)) {
			child = children[[i]]
		
			maxChild = alphabeta(child[[3]], depth-1, alpha, beta, FALSE)

			if(v < maxChild) {
				v = maxChild
			}

			if(v > alpha) {
				alpha = v
			}

			# beta cut-off
			if(beta <= alpha) {
				break
			}
		}

		return(v)
	}

	v = 1000

	for(i in 1:length(children)) {
		child = children[[i]]
		
		minChild = alphabeta(child[[3]], depth-1, alpha, beta, TRUE)
		
		if(v > minChild) {
			v = minChild
		}

		if(v < beta) {
			beta = v
		}

		# alpha cut-off
		if(beta <= alpha) {
			break
		}
	}

	return(v)
}

minimax = function() {
	data = read.csv("input.csv", header=FALSE)
	playfield = as.matrix(data, ncols=8, nrows=8, byrow=TRUE)

	bestPool = alphabeta(playfield, 3, -1000, 1000, TRUE)

	print(bestPool)
}

minimax()