# Go Backend TODOs for JOB Queries

The JOB dataset queries rely heavily on the `in` operator for list membership which the Go backend does not currently lower correctly. Queries beyond `q2` fail to compile due to generated statements using variables out of scope.

Remaining work:

- [ ] Implement proper support for the `in` operator when the right hand side is a list.
- [ ] Re-generate golden code and output for JOB queries `q3` through `q10` once compilation succeeds.

