package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

type Codec struct{}

func (Codec) serialize(root *TreeNode) string {
	if root == nil {
		return "[]"
	}
	out := []string{}
	q := []*TreeNode{root}
	for len(q) > 0 {
		node := q[0]
		q = q[1:]
		if node == nil {
			out = append(out, "null")
		} else {
			out = append(out, strconv.Itoa(node.Val))
			q = append(q, node.Left, node.Right)
		}
	}
	for len(out) > 0 && out[len(out)-1] == "null" {
		out = out[:len(out)-1]
	}
	return "[" + strings.Join(out, ",") + "]"
}

func (Codec) deserialize(data string) *TreeNode {
	data = strings.TrimSpace(data)
	if data == "[]" {
		return nil
	}
	parts := strings.Split(data[1:len(data)-1], ",")
	v, _ := strconv.Atoi(parts[0])
	root := &TreeNode{Val: v}
	q := []*TreeNode{root}
	i := 1
	for len(q) > 0 && i < len(parts) {
		node := q[0]
		q = q[1:]
		if i < len(parts) && parts[i] != "null" {
			v, _ := strconv.Atoi(parts[i])
			node.Left = &TreeNode{Val: v}
			q = append(q, node.Left)
		}
		i++
		if i < len(parts) && parts[i] != "null" {
			v, _ := strconv.Atoi(parts[i])
			node.Right = &TreeNode{Val: v}
			q = append(q, node.Right)
		}
		i++
	}
	return root
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	lines := []string{}
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line != "" {
			lines = append(lines, line)
		}
	}
	if len(lines) == 0 {
		return
	}
	t, _ := strconv.Atoi(lines[0])
	codec := Codec{}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		if tc > 0 {
			fmt.Fprintln(out)
			fmt.Fprintln(out)
		}
		fmt.Fprint(out, codec.serialize(codec.deserialize(lines[tc+1])))
	}
}
