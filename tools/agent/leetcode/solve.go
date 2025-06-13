package leetcode

import (
	"context"
	"fmt"
	"strings"
	"time"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/llm"
	"mochi/runtime/mod"
	"mochi/types"
)

// Solve attempts to generate Mochi code for a LeetCode problem and iterate
// with the LLM until the inline tests pass. The returned Mochi code is the
// final version that satisfied all tests or the last attempt if iterations
// were exhausted.
func Solve(id int, iterate int) (string, error) {
	var text string
	var err error

	// Try downloading the problem a few times in case of transient failures.
	for i := 0; i < 3; i++ {
		start := time.Now()
		text, err = Download(id)
		dur := time.Since(start)
		fmt.Printf("download problem #%d step %d took %s\n", id, i+1, dur)
		if err == nil {
			break
		}
		if i == 2 {
			return "", err
		}
	}
	fmt.Printf("problem #%d:\n%s\n", id, text)

	// Generate initial Mochi solution.
	start := time.Now()
	code, err := GenMochi(id, text)
	dur := time.Since(start)
	if err != nil {
		return "", err
	}
	fmt.Printf("%s\n", code)
	fmt.Printf("initial generation took %s\n", dur)

	for i := 0; i < iterate; i++ {
		// Run tests defined inside the generated Mochi source.
		out, testErr := runTests(code)
		if testErr == nil {
			fmt.Printf("tests passed on iteration %d\n", i+1)
			return code, nil
		}
		fmt.Printf("iteration %d tests failed: %v\n", i+1, testErr)

		if i == iterate-1 {
			return code, fmt.Errorf("tests failed after %d iterations", iterate)
		}

		// Ask the LLM to fix the code using the failing output as context.
		prompt := fmt.Sprintf("The following Mochi program failed its tests:\n\n%s\n\nError:%s\nError output:\n%s\n\nPlease provide a corrected version of the code only.", code, testErr.Error(), out)
		start = time.Now()
		fmt.Println(prompt)
		resp, err := llm.Chat(context.Background(), []llm.Message{{Role: "user", Content: prompt}})
		dur = time.Since(start)
		if err != nil {
			return "", err
		}
		fmt.Printf("refinement %d took %s\n", i+1, dur)
		code = extractMochiCode(resp.Message.Content)
	}

	return code, nil
}

// runTests parses the source code and executes all inline tests.
func runTests(src string) (string, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return "", err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", fmt.Errorf("type error: %v", errs[0])
	}
	modRoot, _ := mod.FindRoot(".")
	out := &strings.Builder{}
	interp := interpreter.New(prog, env, modRoot)
	interp.Env().SetWriter(out)
	err = interp.Test()
	return out.String(), err
}
