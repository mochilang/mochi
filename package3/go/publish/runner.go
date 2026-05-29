package publish

import (
	"fmt"
	"os/exec"
	"strings"
)

// Runner is the command-execution boundary the publish driver
// uses. Run executes a command for its side effect; Output runs the
// command and returns combined stdout for the caller. Both methods
// receive the working directory as the first argument.
//
// The interface lets the unit tests substitute a recording runner
// (RecordingRunner in runner_test.go) for the real exec runner so
// the publish flow can be validated without touching git on disk.
type Runner interface {
	Run(dir string, name string, args ...string) error
	Output(dir string, name string, args ...string) (string, error)
}

// NewExecRunner returns the default Runner that shells out to the
// host's binaries via os/exec. The env passed to each command is
// the parent process's env minus GIT_DIR / GIT_WORK_TREE so that
// nested git workspaces (the host repo) cannot leak into the
// publish workspace.
func NewExecRunner() Runner {
	return execRunner{}
}

type execRunner struct{}

func (execRunner) Run(dir, name string, args ...string) error {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	cmd.Env = sanitisedEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("%s %s: %v\n%s", name, strings.Join(args, " "), err, out)
	}
	return nil
}

func (execRunner) Output(dir, name string, args ...string) (string, error) {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	cmd.Env = sanitisedEnv()
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("%s %s: %v\n%s", name, strings.Join(args, " "), err, out)
	}
	return string(out), nil
}

// sanitisedEnv returns the parent process env minus the variables
// that would let a nested git context leak into the publish
// workspace. The publish workspace is always a fresh repo; we never
// want GIT_DIR / GIT_WORK_TREE / GIT_INDEX_FILE from the parent.
func sanitisedEnv() []string {
	parent := getenv()
	out := make([]string, 0, len(parent))
	for _, kv := range parent {
		k := strings.SplitN(kv, "=", 2)[0]
		switch k {
		case "GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_OBJECT_DIRECTORY":
			continue
		}
		out = append(out, kv)
	}
	return out
}
