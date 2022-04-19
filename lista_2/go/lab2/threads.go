package lab2

import (
	"fmt"
	"time"
)

const N = 1
const H = 5

func Print(ch chan string) {
	for val := range ch {
		fmt.Println(val)
	}
}

func Source(sourceLink chan packet, k int, h int) {
	for i := 0; i < k; i++ {
		time.Sleep(MyRandTime(N))
		sourceLink <- packet{i, h}
	}
}

func Node(input chan packet, outputs []chan packet, idx int, packReport []string, nodeReport []string,
	printer chan string, endChan chan packet, trapSetter chan bool, roundsReport []int, stepsReport []int) {
	trapSet := false
	for {
		select {
		case product := <-input:
			printer <- fmt.Sprint("PACKET ", product.id, " IS IN NODE ", idx)
			time.Sleep(MyRandTime(N))
			product.ttl -= 1
			packReport[product.id] = fmt.Sprint(packReport[product.id], " ", idx)
			nodeReport[idx] = fmt.Sprint(nodeReport[idx], " ", product.id)
			if trapSet {
				product.ttl = -2
				trapSet = false
				endChan <- product
			} else if product.ttl == 0 {
				product.ttl = -1
				endChan <- product
			} else {
			Loop:
				for {
					roundsReport[idx] += 1
					KnuthShuffle(outputs)
					for k := range outputs {
						stepsReport[idx] += 1
						select {
						case outputs[k] <- product:
							break Loop
						default:
						}
					}
					time.Sleep(MyRandTime(N))
				}
			}
		case <-trapSetter:
			trapSet = true
		}
	}
}

func End(endLink chan packet, k int, end chan bool, printer chan string) {
	count := 0
	for product := range endLink {
		count++
		if product.ttl == -1 {
			printer <- fmt.Sprint("PACKET ", product.id, " DIED OF NATURAL CAUSES")
		} else if product.ttl == -2 {
			printer <- fmt.Sprint("PACKET ", product.id, " TRAPPED")
		} else {
			printer <- fmt.Sprint("PACKET ", product.id, " WITH TTL ", product.ttl, "  WAS RECEIVED")
		}
		time.Sleep(MyRandTime(N))
		if count == k {
			end <- true
		}
	}
}

func Hunter(arr []chan bool) {
	for {
		time.Sleep(MyRandTime(H))
		arr[MyRandInt(len(arr))] <- true
	}
}
