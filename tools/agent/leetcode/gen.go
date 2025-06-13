package leetcode

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"mochi/mcp"
	"mochi/runtime/llm"
)

// GenMochi generates Mochi code to solve the given LeetCode problem text.
func GenMochi(id int, problemText string) (string, error) {
	prompt := fmt.Sprintf("Mochi cheatsheet:\n%s\n\nLeetCode problem #%d:\n%s\n\nWrite Mochi code to solve this problem. Output Mochi code only.", mcp.Cheatsheet(), id, problemText)
	resp, err := llm.Chat(context.Background(), []llm.Message{{Role: "user", Content: prompt}})
	if err != nil {
		return "", err
	}
	return extractMochiCode(resp.Message.Content), nil
}

var codeBlockRE = regexp.MustCompile("(?s)```(?:mochi)?\\s*(.*?)```")

func extractMochiCode(s string) string {
	if m := codeBlockRE.FindStringSubmatch(s); len(m) > 1 {
		return strings.TrimSpace(m[1])
	}
	return strings.TrimSpace(s)
}
