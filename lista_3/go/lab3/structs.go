package lab3

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
	l      int
	j      int
	cost_j int
}
