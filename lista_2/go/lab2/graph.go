package lab2

func FindSucc(channels []chan packet, edges [][]bool, idx int, n int) []chan packet {
	var succ []chan packet
	succ = append(succ, channels[idx+1])
	for j := idx + 2; j < n; j++ {
		if edges[idx][j] {
			succ = append(succ, channels[j])
		}
	}
	return succ
}
