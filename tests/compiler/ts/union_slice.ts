// Generated by Mochi TypeScript compiler

type Empty = {
  __name: "Empty";
};

type _Node = {
  __name: "Node";
  child: Foo;
};

type Foo = Empty | _Node;

function listit(): Foo[] {
  return [{ __name: "Empty" }];
}

function main(): void {
  console.log(listit().length);
}
main();
