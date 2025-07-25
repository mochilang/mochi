" Code generated by Mochi transpiler on 2025-07-21 16:07 +0700
customers := #(
  Dictionary newFrom: {
    'id'->1.
    'name'->'Alice'
  }.
  Dictionary newFrom: {
    'id'->2.
    'name'->'Bob'
  }
).
orders := #(
  Dictionary newFrom: {
    'customerId'->1.
    'id'->100
  }.
  Dictionary newFrom: {
    'customerId'->2.
    'id'->101
  }
).
items := #(
  Dictionary newFrom: {
    'orderId'->100.
    'sku'->'a'
  }
).
result := #(
  Dictionary newFrom: {
    'item'->Dictionary newFrom: {
      'orderId'->100.
      'sku'->'a'
    }.
    'name'->'Alice'.
    'orderId'->100
  }
).
Transcript show:'--- Left Join Multi ---'; cr
Transcript show:100; show:' '; show:'Alice'; show:' '; show:Dictionary newFrom: {
  'orderId'->100.
  'sku'->'a'
}; cr
