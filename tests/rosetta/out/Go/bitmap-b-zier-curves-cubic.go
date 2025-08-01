//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

type v = Pixel

type Pixel struct {
	R int `json:"r"`
	G int `json:"g"`
	B int `json:"b"`
}

// line 9
func pixelFromRgb(rgb int) Pixel {
	r := int(((float64(rgb) / float64(65536)) % float64(256)))
	g := int(((float64(rgb) / float64(256)) % float64(256)))
	b := (rgb % 256)
	return Pixel{R: r, G: g, B: b}
}

// line 16
func newBitmap(cols int, rows int) map[string]any {
	var d [][]Pixel = [][]Pixel{}
	y := 0
	for y < rows {
		var row []Pixel = []Pixel{}
		x := 0
		for x < cols {
			row = append(_toAnySlice(row), any(Pixel{R: 0, G: 0, B: 0}))
			x = (x + 1)
		}
		d = append(_toAnySlice(d), any(row))
		y = (y + 1)
	}
	return map[string]any{
		"cols": cols,
		"rows": rows,
		"data": d,
	}
}

// line 32
func setPx(b *map[string]any, x int, y int, p Pixel) {
	cols := (b["cols"]).(int)
	rows := (b["rows"]).(int)
	if (((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows) {
		b["data"][y][x] = any(any(any(p)))
	}
}

// line 40
func fill(b *map[string]any, p Pixel) {
	cols := (b["cols"]).(int)
	rows := (b["rows"]).(int)
	y := 0
	for y < rows {
		x := 0
		for x < cols {
			b["data"][y][x] = any(any(any(p)))
			x = (x + 1)
		}
		y = (y + 1)
	}
}

// line 54
func fillRgb(b map[string]any, rgb int) {
	fill(b, &pixelFromRgb(rgb))
}

// line 56
func line(b map[string]any, x0 int, y0 int, x1 int, y1 int, p Pixel) {
	dx := (x1 - x0)
	if dx < 0 {
		dx = -dx
	}
	dy := (y1 - y0)
	if dy < 0 {
		dy = -dy
	}
	sx := -1
	if x0 < x1 {
		sx = 1
	}
	sy := -1
	if y0 < y1 {
		sy = 1
	}
	err := (dx - dy)
	for {
		setPx(b, x0, y0, &p)
		if (x0 == x1) && (y0 == y1) {
			break
		}
		e2 := (2 * err)
		if e2 > (0 - dy) {
			err = (err - dy)
			x0 = (x0 + sx)
		}
		if e2 < dx {
			err = (err + dx)
			y0 = (y0 + sy)
		}
	}
}

// line 81
func bezier3(b map[string]any, x1 int, y1 int, x2 int, y2 int, x3 int, y3 int, x4 int, y4 int, p Pixel) {
	var px []int = []int{}
	var py []int = []int{}
	i := 0
	for i <= b3Seg {
		px = append(_toAnySlice(px), any(0))
		py = append(_toAnySlice(py), any(0))
		i = (i + 1)
	}
	fx1 := float64(x1)
	fy1 := float64(y1)
	fx2 := float64(x2)
	fy2 := float64(y2)
	fx3 := float64(x3)
	fy3 := float64(y3)
	fx4 := float64(x4)
	fy4 := float64(y4)
	i = 0
	for i <= b3Seg {
		d := ((float64(i)) / (float64(b3Seg)))
		a := (1.0 - d)
		bcoef := (a * a)
		ccoef := (d * d)
		a2 := (a * bcoef)
		b2 := ((3.0 * bcoef) * d)
		c2 := ((3.0 * a) * ccoef)
		d2 := (ccoef * d)
		px[i] = int(((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)) + (d2 * fx4)))
		py[i] = int(((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)) + (d2 * fy4)))
		i = (i + 1)
	}
	x0 := px[0]
	y0 := py[0]
	i = 1
	for i <= b3Seg {
		x := px[i]
		y := py[i]
		line(b, x0, y0, x, y, &p)
		x0 = x
		y0 = y
		i = (i + 1)
	}
}

var b3Seg int
var b map[string]any

func main() {
	b3Seg = 30
	b = newBitmap(400, 300)
	fillRgb(b, 16773055)
	bezier3(b, 20, 200, 700, 50, -300, 50, 380, 150, &pixelFromRgb(4165615))
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}
