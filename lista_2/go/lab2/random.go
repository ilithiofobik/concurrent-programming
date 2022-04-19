package lab2

import (
	"math/rand"
	"time"
)

func MyRandInt(bound int) int {
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	return r1.Intn(bound)
}

func MyRandTime(n int) time.Duration {
	sec := MyRandInt(n*1000) + 100
	return time.Millisecond * time.Duration(sec)
}
