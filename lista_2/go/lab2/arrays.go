package lab2

import (
	"fmt"
	"math/rand"
	"time"
)

func ChanArr(n int) []chan packet {
	arr := make([]chan packet, n+1)
	for i := 0; i <= n; i++ {
		arr[i] = make(chan packet)
	}
	return arr
}

func EdgesArr(n int, d int, b int) [][]bool {
	arr := make([][]bool, n)
	possibleInc := make([]edge, 0)
	possibleDec := make([]edge, 0)

	for i := 0; i < n; i++ {
		row := make([]bool, n)
		for j := 0; j < n; j++ {
			row[j] = false
		}
		if i+1 < n {
			row[i+1] = true
		}
		arr[i] = row
	}

	for i := 0; i < n-2; i++ {
		for j := i + 2; j < n; j++ {
			possibleInc = append(possibleInc, edge{i, j})
		}
	}

	for i := 1; i < n; i++ {
		for j := i - 1; j >= 0; j-- {
			possibleDec = append(possibleDec, edge{i, j})
		}
	}

	shortcutsInc := Min(d, (n*n-n)/2-(n-1))
	shortcutsDec := Min(b, (n*n-n)/2)

	for i := 0; i < shortcutsInc; i++ {
		idx := MyRandInt(len(possibleInc))
		arr[possibleInc[idx].from][possibleInc[idx].to] = true
		possibleInc = append(possibleInc[:idx], possibleInc[idx+1:]...)
	}

	for i := 0; i < shortcutsDec; i++ {
		idx := MyRandInt(len(possibleDec))
		arr[possibleDec[idx].from][possibleDec[idx].to] = true
		possibleDec = append(possibleDec[:idx], possibleDec[idx+1:]...)
	}

	return arr
}

func ReportArrs(n int, k int) ([]string, []string) {
	packReport := make([]string, k)
	nodeReport := make([]string, n)

	for i := 0; i < k; i++ {
		packReport[i] = fmt.Sprint("\nPacket ", i, ":")
	}
	for i := 0; i < n; i++ {
		nodeReport[i] = fmt.Sprint("\nNode ", i, ":")
	}
	return packReport, nodeReport
}

func RoundsArrs(n int) ([]int, []int) {
	roundArr := make([]int, n)
	stepArr := make([]int, n)

	for i := 0; i < n; i++ {
		roundArr[i] = 0
		stepArr[i] = 0
	}
	return roundArr, stepArr
}

func KnuthShuffle(arr []chan packet) {
	rand.Seed(time.Now().UnixNano())
	for i := len(arr) - 1; i > 0; i-- {
		j := rand.Intn(i + 1)
		arr[i], arr[j] = arr[j], arr[i]
	}
}

func TrapArr(n int) []chan bool {
	arr := make([]chan bool, n)
	for i := 0; i < n; i++ {
		arr[i] = make(chan bool)
	}
	return arr
}
