package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

type Row struct {
	dept   string
	name   string
	salary int
}

func solveCase(departments [][2]string, employees [][4]string) string {
	deptName := map[int]string{}
	for _, d := range departments {
		id, _ := strconv.Atoi(d[0])
		deptName[id] = d[1]
	}
	grouped := map[int][]Row{}
	for _, e := range employees {
		salary, _ := strconv.Atoi(e[2])
		deptID, _ := strconv.Atoi(e[3])
		grouped[deptID] = append(grouped[deptID], Row{name: e[1], salary: salary})
	}
	out := []Row{}
	for deptID, items := range grouped {
		salarySet := map[int]bool{}
		salaries := []int{}
		for _, item := range items {
			if !salarySet[item.salary] {
				salarySet[item.salary] = true
				salaries = append(salaries, item.salary)
			}
		}
		sort.Sort(sort.Reverse(sort.IntSlice(salaries)))
		keep := map[int]bool{}
		for i, salary := range salaries {
			if i >= 3 {
				break
			}
			keep[salary] = true
		}
		for _, item := range items {
			if keep[item.salary] {
				item.dept = deptName[deptID]
				out = append(out, item)
			}
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].dept != out[j].dept {
			return out[i].dept < out[j].dept
		}
		if out[i].salary != out[j].salary {
			return out[i].salary > out[j].salary
		}
		return out[i].name < out[j].name
	})
	lines := []string{strconv.Itoa(len(out))}
	for _, row := range out {
		lines = append(lines, fmt.Sprintf("%s,%s,%d", row.dept, row.name, row.salary))
	}
	return stringsJoin(lines, "\n")
}

func stringsJoin(parts []string, sep string) string {
	if len(parts) == 0 {
		return ""
	}
	out := parts[0]
	for i := 1; i < len(parts); i++ {
		out += sep + parts[i]
	}
	return out
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	next := func() string { sc.Scan(); return sc.Text() }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	out := []string{}
	for tc := 0; tc < t; tc++ {
		d, _ := strconv.Atoi(next())
		e, _ := strconv.Atoi(next())
		departments := make([][2]string, d)
		for i := 0; i < d; i++ {
			departments[i] = [2]string{next(), next()}
		}
		employees := make([][4]string, e)
		for i := 0; i < e; i++ {
			employees[i] = [4]string{next(), next(), next(), next()}
		}
		out = append(out, solveCase(departments, employees))
	}
	fmt.Print(stringsJoin(out, "\n\n"))
}
