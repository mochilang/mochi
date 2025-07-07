| products sorted expensive |
products := {
  Dictionary newFrom: {#name->'Laptop'. #price->1500}.
  Dictionary newFrom: {#name->'Smartphone'. #price->900}.
  Dictionary newFrom: {#name->'Tablet'. #price->600}.
  Dictionary newFrom: {#name->'Monitor'. #price->300}.
  Dictionary newFrom: {#name->'Keyboard'. #price->100}.
  Dictionary newFrom: {#name->'Mouse'. #price->50}.
  Dictionary newFrom: {#name->'Headphones'. #price->200}
} asOrderedCollection.
sorted := products asSortedCollection: [:a :b | (a at: #price) > (b at: #price)].
expensive := sorted copyFrom: 2 to: 4.
Transcript show: '--- Top products (excluding most expensive) ---'; cr.
expensive do: [:item |
  Transcript
    show: (item at: #name);
    show: ' costs $';
    show: ((item at: #price) printString);
    cr
].
