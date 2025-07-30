# fuzz test

The directory contains the scripts that test the implementation to avoid the problem that a symbol is parsed as a number or vice versa.

## Resource

* Racket 8.11.1 (cs)
* ~3 minutes to run
* 288M generated files

## Run

```shell
$ cd fuzztest
# 18s
$ racket gen_cases.rkt
cpu time: 17972 real time: 18077 gc time: 475
5156699 cases generated
$ cd ..
$ tree-sitter generate
# 92s
$ tree-sitter parse fuzztest/case.txt > fuzztest/res1.txt
# 50s
$ cd fuzztest && racket postprocess.rkt
# should show nothing
$ sdiff -s <(cat -n expect.txt) <(cat -n res.txt)

# If there is some error, run
$ sdiff -s <(cat -n expect.txt) <(cat -n res.txt) | less
# then get the first error case at `N`-th line
$ cat case.txt | sed -n 'Np'
```

You can edit `gen_cases.rkt` to generate less cases during development.

