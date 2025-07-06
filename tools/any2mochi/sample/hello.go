package sample

// Greeter demonstrates interface conversion
// and slice parameter support

// Greeter interface
// shows trait conversion
//
// More comments
//
// Another line

// Greeter is simple

/* Example
multi line
comment
*/

type Greeter interface {
	Greet(name string) string
}

var ids []int

func Sum(nums []int) int {
	total := 0
	for _, n := range nums {
		total += n
	}
	return total
}

// person implements Greeter
type person struct{}

func (person) Greet(name string) string { return "hi " + name }
