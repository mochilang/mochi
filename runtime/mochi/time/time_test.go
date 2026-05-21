package time

import (
	"fmt"
	gotime "time"
	"testing"
)

func TestNowMonotonic(t *testing.T) {
	a := Now()
	gotime.Sleep(gotime.Millisecond)
	b := Now()
	if b <= a {
		t.Errorf("Now non-monotonic: %d -> %d", a, b)
	}
}

func TestRFC3339RoundTrip(t *testing.T) {
	const ts = "2026-05-21T00:00:00Z"
	nanos, err := ParseRFC3339(ts)
	if err != nil {
		t.Fatal(err)
	}
	if got := FormatRFC3339(nanos); got != ts {
		t.Errorf("Format(Parse(%q)) = %q", ts, got)
	}
}

func TestParseError(t *testing.T) {
	if _, err := ParseRFC3339("not a timestamp"); err == nil {
		t.Error("ParseRFC3339 garbage should error")
	}
}

func TestNowMono(t *testing.T) {
	a := NowMono()
	gotime.Sleep(gotime.Millisecond)
	b := NowMono()
	if b <= a {
		t.Errorf("NowMono non-monotonic: %d -> %d", a, b)
	}
}

func TestSleep(t *testing.T) {
	start := gotime.Now()
	Sleep(int64(gotime.Millisecond))
	if gotime.Since(start) < gotime.Millisecond {
		t.Errorf("Sleep returned too early")
	}
}

func ExampleFormatRFC3339() {
	fmt.Println(FormatRFC3339(0))
	// Output: 1970-01-01T00:00:00Z
}
