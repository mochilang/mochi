package testpkg

import "time"

func TodayISO() string {
	return time.Now().Format("2006-01-02")
}

func TodayLong() string {
	return time.Now().Format("Monday, January 2, 2006")
}
