package lab2

import "fmt"

func Input() (int, int, int, int, int) {
	fmt.Println("Give the n parameter: ")
	var n int
	fmt.Scan(&n)

	fmt.Println("Give the d parameter: ")
	var d int
	fmt.Scan(&d)

	fmt.Println("Give the b parameter: ")
	var b int
	fmt.Scan(&b)

	fmt.Println("Give the h parameter: ")
	var h int
	fmt.Scan(&h)

	fmt.Println("Give the k parameter: ")
	var k int
	fmt.Scan(&k)

	return n, d, b, h, k
}

func PrintReport(packReport []string, nodeReport []string, roundsReport []int, stepsReport []int) {
	fmt.Print("\nPACKETS:")
	for i := 0; i < len(packReport); i++ {
		fmt.Print(packReport[i])
	}
	fmt.Print("\nNODES:")
	for i := 0; i < len(nodeReport); i++ {
		fmt.Print(nodeReport[i])
	}
	fmt.Print("\nAVG ROUNDS:")
	for i := 0; i < len(roundsReport); i++ {
		val := -1.0
		if len(nodeReport[i]) > 7 {
			val = float64(roundsReport[i]) / float64((len(nodeReport[i])-7)/2)
		}
		fmt.Print("\nNode ", i, ": ", val)
	}
	fmt.Print("\nAVG STEPS:")
	for i := 0; i < len(stepsReport); i++ {
		val := -1.0
		if roundsReport[i] > 0 {
			val = float64(stepsReport[i]) / float64(roundsReport[i])
		}
		fmt.Print("\nNode ", i, ": ", val)
	}
}

func PrintGraph(edges [][]bool, n int) {
	fmt.Println("GRAPH:")
	for i := 0; i < n-1; i++ {
		for j := 0; j < n; j++ {
			if edges[i][j] {
				fmt.Println(i, " -> ", j)
			}
		}
	}
}
