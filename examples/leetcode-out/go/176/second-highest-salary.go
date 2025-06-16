package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Employee struct {
	Id int `json:"id"`
	Salary int `json:"salary"`
}

func secondHighestSalary(employees []Employee) int {
	if (len(employees) < 2) {
		return 0
	}
	var firstEmp Employee = employees[0]
	_ = firstEmp
	var first int = firstEmp.Salary
	var second int = first
	var i int = 1
	for (i < len(employees)) {
		var emp Employee = employees[i]
		_ = emp
		var s int = emp.Salary
		if (s > first) {
			second = first
			first = s
		} else 		if ((s != first) && (((second == first) || (s > second)))) {
			second = s
		}
		i = (i + 1)
	}
	if (second == first) {
		return 0
	}
	return second
}

func example() {
	expect((secondHighestSalary(employees) == 200))
}

func no_second_salary() {
	expect((secondHighestSalary([]Employee{Employee{Id: 1, Salary: 100}}) == 0))
}

func duplicates() {
	var list []Employee = []Employee{Employee{Id: 1, Salary: 100}, Employee{Id: 2, Salary: 100}, Employee{Id: 3, Salary: 100}}
	_ = list
	expect((secondHighestSalary(list) == 0))
}

var employees []Employee = []Employee{Employee{Id: 1, Salary: 100}, Employee{Id: 2, Salary: 200}, Employee{Id: 3, Salary: 300}, Employee{Id: 4, Salary: 200}}
func main() {
	example()
	no_second_salary()
	duplicates()
}

