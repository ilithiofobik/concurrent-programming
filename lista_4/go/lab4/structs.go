package lab4

type edge struct {
	from int
	to   int
}

type state struct {
	changed bool
	nexthop int
	cost    int
}

type offer struct {
	l     int
	j     int
	costJ int
}

type standardPacket struct {
	rs             int
	hs             int
	rd             int
	hd             int
	visitedRouters []int
}
