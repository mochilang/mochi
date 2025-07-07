| customers orders |
customers := {
  Dictionary newFrom: {#id->1. #name->'Alice'}.
  Dictionary newFrom: {#id->2. #name->'Bob'}.
  Dictionary newFrom: {#id->3. #name->'Charlie'}
}.
orders := {
  Dictionary newFrom: {#id->100. #customerId->1. #total->250}.
  Dictionary newFrom: {#id->101. #customerId->2. #total->125}.
  Dictionary newFrom: {#id->102. #customerId->1. #total->300}
}.
Transcript show: '--- Cross Join: All order-customer pairs ---'; cr.
orders do: [:o |
  customers do: [:c |
    Transcript
      show: 'Order ', (o at: #id) printString;
      show: ' (customerId: ', (o at: #customerId) printString;
      show: ', total: $', (o at: #total) printString;
      show: ') paired with ', (c at: #name);
      cr
  ]
].
