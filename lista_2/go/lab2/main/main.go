package main

import "lab2"

func main() {
	n, d, b, h, k := lab2.Input()

	printer := make(chan string, 100)
	terminate := make(chan bool)
	go lab2.Print(printer)

	edges := lab2.EdgesArr(n, d, b)
	channels := lab2.ChanArr(n)
	packReport, nodeReport := lab2.ReportArrs(n, k)
	trapChannels := lab2.TrapArr(n)
	roundsReport, stepsReport := lab2.RoundsArrs(n)

	lab2.PrintGraph(edges, n)

	for i := 0; i < n; i++ {
		go lab2.Node(channels[i], lab2.FindSucc(channels, edges, i, n), i, packReport,
			nodeReport, printer, channels[n], trapChannels[i], roundsReport, stepsReport)
	}

	go lab2.End(channels[n], k, terminate, printer)

	go lab2.Hunter(trapChannels)

	go lab2.Source(channels[0], k, h)

	<- terminate

	lab2.PrintReport(packReport, nodeReport, roundsReport, stepsReport)
}
