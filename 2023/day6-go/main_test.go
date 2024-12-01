package main

import (
	"fmt"
	"testing"
)

func threshold2(avail, threshold []int64, disruptions int64, pendingRemoval int) (meanAvail float64, meanThreshold float64) {
	unavailablePeriod := 24
	var total int64
	for _, i := range avail {
		total += i
	}
	total -= int64(len(avail) * pendingRemoval)
	total -= disruptions * int64(unavailablePeriod)
	meanTotal := float64(total / int64(len(avail)))
	return meanTotal, mean(threshold)
}

func threshold(avail, threshold []int64, next int64, pendingRemoval int) (meanAvail float64, meanThreshold float64) {
	unavailablePeriod := int64(len(avail) / 7)
	var total int64
	for _, i := range avail {
		total += i - int64(pendingRemoval)
	}
	total -= (next * unavailablePeriod)
	meanTotal := float64(total / int64(len(avail)))
	return meanTotal, mean(threshold)
}

func TestName(t *testing.T) {
	avail := []int64{702, 702, 702, 702, 702, 702, 702, 702, 702, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703, 703}
	tt := []int64{697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697, 697}
	fmt.Println(threshold(avail, tt, 5, 3))
	fmt.Println(threshold2(avail, tt, 5, 3))
}

func mean(nums []int64) float64 {
	k := .0
	for _, v := range nums {
		k += float64(v)
	}
	return k / float64(len(nums))
}
