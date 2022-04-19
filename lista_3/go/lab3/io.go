package lab3

import "fmt"

func Input() (int, int) {
	fmt.Println("Give the n parameter: ")
	var n int
	fmt.Scan(&n)

	fmt.Println("Give the d parameter: ")
	var d int
	fmt.Scan(&d)

	return n, d
}

func PrintGraph(edges *[][]int, n int) {
	fmt.Println("GRAPH:")
	for i := 0; i < n; i++ {
		for _, j := range (*edges)[i] {
			fmt.Println(i, " -> ", j)
		}
	}
}
