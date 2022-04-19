package lab4

import "fmt"

func Input() (int, int, int) {
	fmt.Println("Give the n parameter: ")
	var n int
	fmt.Scan(&n)

	fmt.Println("Give the d parameter: ")
	var d int
	fmt.Scan(&d)

	fmt.Println("Give the h parameter: ")
	var h int
	fmt.Scan(&h)

	return n, d, h
}

func PrintGraph(edges *[][]int, n int) {
	fmt.Println("GRAPH:")
	for i := 0; i < n; i++ {
		for _, j := range (*edges)[i] {
			fmt.Println(i, " -> ", j)
		}
	}
}

func PrintRouters(hosts *[][]chan standardPacket, n int) {
	fmt.Println("ROUTERS: ")
	for i := 0; i < n; i++ {
		fmt.Println(i, "has", len((*hosts)[i]), "hosts")
	}

}
