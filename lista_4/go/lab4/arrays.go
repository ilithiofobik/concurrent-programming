package lab4

func ChanArr(n int) ([]chan []offer, []chan bool, []chan bool, []chan offer, []chan offer) {
	senderUnlock := make([]chan []offer, n)
	senderLock := make([]chan bool, n)
	receiverUnlock := make([]chan bool, n)
	receiverLock := make([]chan offer, n)
	receiverGet := make([]chan offer, n)

	for i := 0; i < n; i++ {
		senderUnlock[i] = make(chan []offer, n)
		senderLock[i] = make(chan bool)
		receiverUnlock[i] = make(chan bool)
		receiverLock[i] = make(chan offer)
		receiverGet[i] = make(chan offer)
	}
	return senderUnlock, senderLock, receiverUnlock, receiverLock, receiverGet
}

func HostChannels(n int, h int) ([]chan standardPacket, []chan standardPacket, []chan int, []chan int, [][]chan standardPacket) {
	forwarderGet := make([]chan standardPacket, n)
	forwarderQueue := make([]chan standardPacket, n)
	forwarderLock := make([]chan int, n)
	forwarderUnlock := make([]chan int, n)
	hostGet := make([][]chan standardPacket, n)

	for i := 0; i < n; i++ {
		hostGet[i] = make([]chan standardPacket, 0)
		forwarderGet[i] = make(chan standardPacket)
		forwarderQueue[i] = make(chan standardPacket, 1000)
		forwarderLock[i] = make(chan int)
		forwarderUnlock[i] = make(chan int)
	}

	for i := 0; i < h; i++ {
		idx := MyRandInt(n)
		hostGet[idx] = append(hostGet[idx], make(chan standardPacket))
	}

	return forwarderGet, forwarderQueue, forwarderLock, forwarderUnlock, hostGet
}

func NeighboursGenerator(n int, d int) [][]int {
	arr := make([][]int, n)
	possible := make([]edge, 0)

	for i := 0; i < n; i++ {
		row := make([]int, 0)
		if i+1 < n {
			row = append(row, i+1)
		}
		if i-1 >= 0 {
			row = append(row, i-1)
		}
		arr[i] = row
	}

	for i := 0; i < n-2; i++ {
		for j := i + 2; j < n; j++ {
			possible = append(possible, edge{i, j})
		}
	}

	shortcuts := Min(d, (n*n-n)/2-(n-1))

	for i := 0; i < shortcuts; i++ {
		idx := MyRandInt(len(possible))
		arr[possible[idx].from] = append(arr[possible[idx].from], possible[idx].to)
		arr[possible[idx].to] = append(arr[possible[idx].to], possible[idx].from)
		possible = append(possible[:idx], possible[idx+1:]...)
	}

	return arr
}

func RoutingTableGenerator(j int, n int, N []int) map[int]state {
	R := make(map[int]state)
	for i := 0; i < j; i++ {
		R[i] = state{changed: true, cost: j - i, nexthop: j - 1}
	}
	for i := j + 1; i < n; i++ {
		R[i] = state{changed: true, cost: i - j, nexthop: j + 1}
	}
	for _, val := range N {
		R[val] = state{changed: true, cost: 1, nexthop: val}
	}
	return R
}
