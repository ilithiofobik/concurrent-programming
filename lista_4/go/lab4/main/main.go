package main

import (
	"lab4"
)

func main() {
	n, d, h := lab4.Input()
	toIninityAndBeyond := make(chan bool)

	N := lab4.NeighboursGenerator(n, d)
	lab4.PrintGraph(&N, n)

	senderUnlock, senderLock, receiverUnlock, receiverLock, receiverGet := lab4.ChanArr(n)
	forwarderGet, forwarderQueue, forwarderLock, forwarderUnlock, hostGet := lab4.HostChannels(n, h)

	lab4.PrintRouters(&hostGet, n)

	for i := 0; i < n; i++ {
		go lab4.Manager(i, n, senderUnlock[i], senderLock[i], receiverUnlock[i], receiverLock[i], forwarderUnlock[i], forwarderLock[i], N[i])
	}

	for i := 0; i < n; i++ {
		go lab4.Receiver(i, receiverGet[i], receiverLock[i], receiverUnlock[i])
	}

	for i := 0; i < n; i++ {
		go lab4.Sender(i, senderUnlock[i], senderLock[i], receiverGet, N[i])
	}

	for i := 0; i < n; i++ {
		go lab4.ForwarderReceiver(forwarderGet[i], forwarderQueue[i])
		go lab4.ForwarderSender(i, hostGet[i], forwarderQueue[i], forwarderLock[i], forwarderUnlock[i], forwarderGet)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < len(hostGet[i]); j++ {
			initR, initH := lab4.RandomHost(n, i, j, hostGet)
			go lab4.Host(i, j, initR, initH, hostGet[i][j], forwarderGet[i])
		}
	}

	<-toIninityAndBeyond
}
