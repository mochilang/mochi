package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Entry struct {
	Ts int `json:"ts"`
	Cnt int `json:"cnt"`
}

type HitCounter struct {
	Data []Entry `json:"data"`
}

func newCounter() HitCounter {
	return HitCounter{Data: _cast[[]Entry]([]any{})}
}

func clean(c HitCounter, timestamp int) HitCounter {
	var d []Entry = c.Data
	var i int = 0
	for (i < len(d)) {
		var ent Entry = d[i]
		_ = ent
		if ((timestamp - ent.Ts) >= 300) {
			i = (i + 1)
		} else {
			break
		}
	}
	d = d[i:len(d)]
	return HitCounter{Data: d}
}

func hit(c HitCounter, timestamp int) HitCounter {
	var counter HitCounter = clean(c, timestamp)
	_ = counter
	var d []Entry = counter.Data
	if (len(d) > 0) {
		var last Entry = d[(len(d) - 1)]
		_ = last
		if (last.Ts == timestamp) {
			d = append(append([]Entry{}, d[0:(len(d) - 1)]...), []Entry{Entry{Ts: last.Ts, Cnt: (last.Cnt + 1)}}...)
		} else {
			d = append(append([]Entry{}, d...), []Entry{Entry{Ts: timestamp, Cnt: 1}}...)
		}
	} else {
		d = append(append([]Entry{}, d...), []Entry{Entry{Ts: timestamp, Cnt: 1}}...)
	}
	return HitCounter{Data: d}
}

func getHits(c HitCounter, timestamp int) int {
	var counter HitCounter = clean(c, timestamp)
	_ = counter
	var sum int = 0
	for _, e := range counter.Data {
		sum = (sum + e.Cnt)
	}
	return sum
}

func example() {
	var c HitCounter = newCounter()
	c = hit(c, 1)
	c = hit(c, 2)
	c = hit(c, 3)
	expect((getHits(c, 4) == 3))
	c = hit(c, 300)
	expect((getHits(c, 300) == 4))
	expect((getHits(c, 301) == 3))
}

func expire() {
	var c HitCounter = newCounter()
	c = hit(c, 1)
	c = hit(c, 10)
	c = hit(c, 300)
	expect((getHits(c, 300) == 3))
	c = hit(c, 601)
	expect((getHits(c, 601) == 1))
}

func main() {
	example()
	expire()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

