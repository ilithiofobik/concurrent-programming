package lab4

import (
	"fmt"
	"time"
)

const TIME = 3
const HOSTTIME = 1

func Sender(idx int, senderUnlock chan []offer, senderLock chan bool, receiverGets []chan offer, N []int) {
	for {
		time.Sleep(MyRandTime(TIME))
		senderLock <- true
		list := <-senderUnlock
		for _, offer := range list {
			for _, i := range N {
				//fmt.Println("SENDER ", idx, ": Sending offer (", offer.j,  ", ", offer.cost_j, ") to receiver ", i)
				receiverGets[i] <- offer
			}
		}
	}
}

func Receiver(i int, receiverGet chan offer, receiverLock chan offer, receiverUnlock chan bool) {
	for {
		offer := <-receiverGet
		//fmt.Println("RECEIVER ", i, ": Received offer (", offer.j, ", ", offer.cost_j, ")")
		receiverLock <- offer
		<-receiverUnlock
	}
}

func Manager(i int, n int, senderUnlock chan []offer, senderLock chan bool, receiverUnlock chan bool, receiverLock chan offer, forwarderUnlock chan int, forwarderLock chan int, N []int) {
	R := RoutingTableGenerator(i, n, N)
	for {
		select {
		case <-senderLock:
			list := make([]offer, 0)
			for idx, val := range R {
				if val.changed {
					R[idx] = state{changed: false, nexthop: R[idx].nexthop, cost: R[idx].cost}
					fmt.Println("CHANGE: R_", i, "[", idx, "] = {changed: false, nexthop: ", R[idx].nexthop, ", cost: ", R[idx].cost, "}")
					list = append(list, offer{l: i, j: idx, costJ: val.cost})
				}
			}
			senderUnlock <- list

		case offer := <-receiverLock:
			newCost := offer.costJ + 1
			if R[offer.j].cost > newCost {
				R[offer.j] = state{changed: true, nexthop: offer.l, cost: newCost}
				fmt.Println("CHANGE: R_", i, "[", offer.j, "] = {changed: true, nexthop: ", offer.l, ", cost: ", newCost, "}")
			}
			receiverUnlock <- true

		case idx := <-forwarderLock:
			forwarderUnlock <- R[idx].nexthop
		}
	}
}

func ForwarderReceiver(forwarderGet chan standardPacket, forwarderQueue chan standardPacket) {
	for {
		packet := <-forwarderGet
		select {
		case forwarderQueue <- packet:
			{
			}
		case <-time.After(500 * time.Millisecond):
			{
			}
		}
	}
}

func ForwarderSender(r int, myHosts []chan standardPacket, forwarderQueue chan standardPacket, forwarderLock chan int, forwarderUnlock chan int, forwarderGets []chan standardPacket) {
	for {
		packet := <-forwarderQueue
		packet = standardPacket{rs: packet.rs, hs: packet.hs, rd: packet.rd, hd: packet.hd, visitedRouters: append(packet.visitedRouters, r)}
		if r == packet.rd {
			myHosts[packet.hd] <- packet
		} else {
			forwarderLock <- packet.rd
			nextHop := <-forwarderUnlock
			forwarderGets[nextHop] <- packet
		}
	}
}

func Host(r int, h int, initR int, initH int, hostGet chan standardPacket, forwarderGet chan standardPacket) {
	packet := standardPacket{rs: r, hs: h, rd: initR, hd: initH, visitedRouters: make([]int, 0)}
	forwarderGet <- packet
	for {
		packet := <-hostGet
		fmt.Println("PACKET: from (", packet.rs, ",", packet.hs, ") to (", packet.rd, ",", packet.hd, "), routers:", packet.visitedRouters)
		time.Sleep(MyRandTime(HOSTTIME))
		newPacket := standardPacket{rs: r, hs: h, rd: packet.rs, hd: packet.hs, visitedRouters: make([]int, 0)}
		forwarderGet <- newPacket
	}
}
