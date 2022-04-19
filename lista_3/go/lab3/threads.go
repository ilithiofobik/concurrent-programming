package lab3

import (
	"fmt"
	"sync/atomic"
	"time"
)

const TIME = 1

func Sender(idx int, senderUnlock chan []offer, senderLock chan bool, receiverGets []chan offer, N []int) {
	for {
		time.Sleep(MyRandTime(TIME))
		senderLock <- true
		list := <-senderUnlock
		for _, offer := range list {
			for _, i := range N {
				fmt.Println("SENDER ", idx, ": Sending offer (", offer.j, ", ", offer.cost_j, ") to receiver ", i)
				receiverGets[i] <- offer
			}
		}
	}
}

func Receiver(i int, receiverGet chan offer, receiverLock chan offer, receiverUnlock chan bool) {
	for {
		offer := <-receiverGet
		fmt.Println("RECEIVER ", i, ": Received offer (", offer.j, ", ", offer.cost_j, ")")
		receiverLock <- offer
		<-receiverUnlock
	}
}

func Manager(i int, n int, senderUnlock chan []offer, senderLock chan bool, receiverUnlock chan bool, receiverLock chan offer, N []int, counter *uint64) {
	R := RoutingTableGenerator(i, n, N)
	for {
		select {
		case <-senderLock:
			list := make([]offer, 0)
			for idx, val := range R {
				if val.changed {
					R[idx] = state{changed: false, nexthop: R[idx].nexthop, cost: R[idx].cost}
					atomic.AddUint64(counter, ^uint64(0))
					fmt.Println("CHANGE: R_", i, "[", idx, "] = {changed: false, nexthop: ", R[idx].nexthop, ", cost: ", R[idx].cost, "}")
					list = append(list, offer{l: i, j: idx, cost_j: val.cost})
				}
			}
			senderUnlock <- list

		case offer := <-receiverLock:
			newCost := offer.cost_j + 1
			if R[offer.j].cost > newCost {
				if R[offer.j].changed == false {
					atomic.AddUint64(counter, 1)
				}
				R[offer.j] = state{changed: true, nexthop: offer.l, cost: newCost}
				fmt.Println("CHANGE: R_", i, "[", offer.j, "] = {changed: true, nexthop: ", offer.l, ", cost: ", newCost, "}")
			}
			receiverUnlock <- true
		}
	}
}
