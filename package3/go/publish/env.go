package publish

import "os"

// getenv is the os.Environ wrapper the runner uses. It is split out
// so unit tests can stub it without touching real process env.
var getenv = func() []string { return os.Environ() }
