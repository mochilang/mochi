package avltree

import (
	"encoding/json"
	"strings"
)

// Node is a node in an AVL tree storing ints.
type Node struct {
	Data    int      `json:"Data"`
	Balance int      `json:"Balance"`
	Link    [2]*Node `json:"Link"`
}

func opp(dir int) int { return 1 - dir }

func single(root *Node, dir int) *Node {
	save := root.Link[opp(dir)]
	root.Link[opp(dir)] = save.Link[dir]
	save.Link[dir] = root
	return save
}

func double(root *Node, dir int) *Node {
	save := root.Link[opp(dir)].Link[dir]

	root.Link[opp(dir)].Link[dir] = save.Link[opp(dir)]
	save.Link[opp(dir)] = root.Link[opp(dir)]
	root.Link[opp(dir)] = save

	save = root.Link[opp(dir)]
	root.Link[opp(dir)] = save.Link[dir]
	save.Link[dir] = root
	return save
}

func adjustBalance(root *Node, dir, bal int) {
	n := root.Link[dir]
	nn := n.Link[opp(dir)]
	switch nn.Balance {
	case 0:
		root.Balance = 0
		n.Balance = 0
	case bal:
		root.Balance = -bal
		n.Balance = 0
	default:
		root.Balance = 0
		n.Balance = bal
	}
	nn.Balance = 0
}

func insertBalance(root *Node, dir int) *Node {
	n := root.Link[dir]
	bal := 2*dir - 1
	if n.Balance == bal {
		root.Balance = 0
		n.Balance = 0
		return single(root, opp(dir))
	}
	adjustBalance(root, dir, bal)
	return double(root, opp(dir))
}

func insertR(root *Node, data int) (*Node, bool) {
	if root == nil {
		return &Node{Data: data}, false
	}
	dir := 0
	if root.Data < data {
		dir = 1
	}
	var done bool
	root.Link[dir], done = insertR(root.Link[dir], data)
	if done {
		return root, true
	}
	root.Balance += 2*dir - 1
	switch root.Balance {
	case 0:
		return root, true
	case 1, -1:
		return root, false
	}
	return insertBalance(root, dir), true
}

// Insert inserts data into the tree.
func Insert(tree **Node, data int) { *tree, _ = insertR(*tree, data) }

func removeBalance(root *Node, dir int) (*Node, bool) {
	n := root.Link[opp(dir)]
	bal := 2*dir - 1
	switch n.Balance {
	case -bal:
		root.Balance = 0
		n.Balance = 0
		return single(root, dir), false
	case bal:
		adjustBalance(root, opp(dir), -bal)
		return double(root, dir), false
	}
	root.Balance = -bal
	n.Balance = bal
	return single(root, dir), true
}

func removeR(root *Node, data int) (*Node, bool) {
	if root == nil {
		return nil, false
	}
	if root.Data == data {
		switch {
		case root.Link[0] == nil:
			return root.Link[1], false
		case root.Link[1] == nil:
			return root.Link[0], false
		}
		heir := root.Link[0]
		for heir.Link[1] != nil {
			heir = heir.Link[1]
		}
		root.Data = heir.Data
		data = heir.Data
	}
	dir := 0
	if root.Data < data {
		dir = 1
	}
	var done bool
	root.Link[dir], done = removeR(root.Link[dir], data)
	if done {
		return root, true
	}
	root.Balance += 1 - 2*dir
	switch root.Balance {
	case 1, -1:
		return root, true
	case 0:
		return root, false
	}
	return removeBalance(root, dir)
}

// Remove removes data from the tree.
func Remove(tree **Node, data int) { *tree, _ = removeR(*tree, data) }

// Dump returns a JSON representation with indentation.
func Dump(tree *Node) string {
	b, _ := json.MarshalIndent(tree, "", "   ")
	return string(b)
}

// ExampleOutput returns the output for the Rosetta task.
func ExampleOutput() string {
	var tree *Node
	var sb strings.Builder
	sb.WriteString("Empty tree:\n")
	sb.WriteString(Dump(tree))
	sb.WriteString("\n\nInsert test:\n")
	Insert(&tree, 3)
	Insert(&tree, 1)
	Insert(&tree, 4)
	Insert(&tree, 1)
	Insert(&tree, 5)
	sb.WriteString(Dump(tree))
	sb.WriteString("\n\nRemove test:\n")
	Remove(&tree, 3)
	Remove(&tree, 1)
	sb.WriteString(Dump(tree))
	return sb.String()
}
