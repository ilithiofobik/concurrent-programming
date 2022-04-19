package lab4

import (
	"math/rand"
	"time"
)

var s1 = rand.NewSource(time.Now().UTC().UnixNano())
var r1 = rand.New(s1)

func MyRandInt(bound int) int {
	return r1.Intn(bound)
}

func MyRandTime(n int) time.Duration {
	sec := MyRandInt(n*1000) + n*500
	return time.Millisecond * time.Duration(sec)
}

func RandomHost(n int, notR int, notH int, hostGet [][]chan standardPacket) (int, int) {
	for {
		r := MyRandInt(n)
		hosts := hostGet[r]
		if len(hosts) > 0 {
			h := MyRandInt(len(hosts))
			if r != notR || h != notH {
				return r, h
			}
		}
	}
}
