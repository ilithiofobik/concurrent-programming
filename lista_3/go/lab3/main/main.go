package main

import (
	"lab3"
	"time"
)

func main() {
	n, d := lab3.Input()
	var globalCounter uint64 = uint64(n * (n - 1))

	N := lab3.NeigboursGenerator(n, d)
	lab3.PrintGraph(&N, n)

	senderUnlock, senderLock, receiverUnlock, receiverLock, receiverGet := lab3.ChanArr(n)

	for i := 0; i < n; i++ {
		go lab3.Manager(i, n, senderUnlock[i], senderLock[i], receiverUnlock[i], receiverLock[i], N[i], &globalCounter)
	}

	for i := 0; i < n; i++ {
		go lab3.Receiver(i, receiverGet[i], receiverLock[i], receiverUnlock[i])
	}

	for i := 0; i < n; i++ {
		go lab3.Sender(i, senderUnlock[i], senderLock[i], receiverGet, N[i])
	}

	for {
		time.Sleep(time.Second * time.Duration(1))
		if globalCounter == 0 {
			time.Sleep(time.Second * time.Duration(5))
			if globalCounter == 0 {
				break
			}
		}
	}
}
