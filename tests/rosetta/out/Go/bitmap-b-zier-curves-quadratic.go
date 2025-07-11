//go:build ignore

package main

type Pixel struct {
	R int `json:"r"`
	G int `json:"g"`
	B int `json:"b"`
}

// line 9
func pixelFromRgb(rgb int) Pixel {
	var r int = int(((float64(rgb) / float64(65536)) % float64(256)))
	var g int = int(((float64(rgb) / float64(256)) % float64(256)))
	var b int = (rgb % 256)
	return Pixel{R: r, G: g, B: b}
}

// line 16
func newBitmap(cols int, rows int) map[string]any {
	var d [][]Pixel = [][]Pixel{}
	var y int = 0
	for {
		if !(y < rows) {
			break
		}
		var row []Pixel = []Pixel{}
		var x int = 0
		for {
			if !(x < cols) {
				break
			}
			row = append(_convSlice[Pixel, any](row), Pixel{R: 0, G: 0, B: 0})
			x = (x + 1)
		}
		d = append(_convSlice[[]Pixel, any](d), row)
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
	var cols int = (b["cols"]).(int)
	var rows int = (b["rows"]).(int)
	if (((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows) {
		b["data"][y][x] = p
	}
}

// line 40
func fill(b *map[string]any, p Pixel) {
	var cols int = (b["cols"]).(int)
	var rows int = (b["rows"]).(int)
	var y int = 0
	for {
		if !(y < rows) {
			break
		}
		var x int = 0
		for {
			if !(x < cols) {
				break
			}
			b["data"][y][x] = p
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
	var dx int = (x1 - x0)
	if dx < 0 {
		dx = -dx
	}
	var dy int = (y1 - y0)
	if dy < 0 {
		dy = -dy
	}
	var sx int = -1
	if x0 < x1 {
		sx = 1
	}
	var sy int = -1
	if y0 < y1 {
		sy = 1
	}
	var err int = (dx - dy)
	for {
		setPx(b, x0, y0, &p)
		if (x0 == x1) && (y0 == y1) {
			break
		}
		var e2 int = (2 * err)
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
func bezier2(b map[string]any, x1 int, y1 int, x2 int, y2 int, x3 int, y3 int, p Pixel) {
	var px []int = []int{}
	var py []int = []int{}
	var i int = 0
	for {
		if !(i <= b2Seg) {
			break
		}
		px = append(_convSlice[int, any](px), 0)
		py = append(_convSlice[int, any](py), 0)
		i = (i + 1)
	}
	var fx1 float64 = float64(x1)
	var fy1 float64 = float64(y1)
	var fx2 float64 = float64(x2)
	var fy2 float64 = float64(y2)
	var fx3 float64 = float64(x3)
	var fy3 float64 = float64(y3)
	i = 0
	for {
		if !(i <= b2Seg) {
			break
		}
		var c float64 = ((float64(i)) / (float64(b2Seg)))
		var a float64 = (1.0 - c)
		var a2 float64 = (a * a)
		var b2 float64 = ((2.0 * c) * a)
		var c2 float64 = (c * c)
		px[i] = int((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)))
		py[i] = int((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)))
		i = (i + 1)
	}
	var x0 int = px[0]
	var y0 int = py[0]
	i = 1
	for {
		if !(i <= b2Seg) {
			break
		}
		var x int = px[i]
		var y int = py[i]
		line(b, x0, y0, x, y, &p)
		x0 = x
		y0 = y
		i = (i + 1)
	}
}

var b2Seg int
var b map[string]any

func main() {
	b2Seg = 20
	b = newBitmap(400, 300)
	fillRgb(b, 14614575)
	bezier2(b, 20, 150, 500, -100, 300, 280, &pixelFromRgb(4165615))
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
