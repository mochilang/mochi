let people = [{name: "Alice", age: 30}, {name: "Bob", age: 25}];
(() => {
  for (const _tmp1 of people) {
    console.log(JSON.stringify(_tmp1));
  }
})();
