| orders lineitem start_date end_date date_filtered_orders late_orders result |
orders := {Dictionary from:{'o_orderkey'->1. 'o_orderdate'->'1993-07-01'. 'o_orderpriority'->'1-URGENT'}. Dictionary from:{'o_orderkey'->2. 'o_orderdate'->'1993-07-15'. 'o_orderpriority'->'2-HIGH'}. Dictionary from:{'o_orderkey'->3. 'o_orderdate'->'1993-08-01'. 'o_orderpriority'->'3-NORMAL'}}.
lineitem := {Dictionary from:{'l_orderkey'->1. 'l_commitdate'->'1993-07-10'. 'l_receiptdate'->'1993-07-12'}. Dictionary from:{'l_orderkey'->1. 'l_commitdate'->'1993-07-12'. 'l_receiptdate'->'1993-07-10'}. Dictionary from:{'l_orderkey'->2. 'l_commitdate'->'1993-07-20'. 'l_receiptdate'->'1993-07-25'}. Dictionary from:{'l_orderkey'->3. 'l_commitdate'->'1993-08-02'. 'l_receiptdate'->'1993-08-01'}. Dictionary from:{'l_orderkey'->3. 'l_commitdate'->'1993-08-05'. 'l_receiptdate'->'1993-08-10'}}.
start_date := '1993-07-01'.
end_date := '1993-08-01'.
date_filtered_orders := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    (((((o at: 'o_orderdate') >= start_date)) and: [(((o at: 'o_orderdate') < end_date))])) ifTrue: [
      tmp add: o.
    ].
  ].
  tmp
] value.
late_orders := [ | tmp |
  tmp := OrderedCollection new.
  date_filtered_orders do: [:o |
    (exists value: [ | tmp |
  tmp := OrderedCollection new.
  lineitem do: [:l |
    (((((l at: 'l_orderkey') = (o at: 'o_orderkey'))) and: [(((l at: 'l_commitdate') < (l at: 'l_receiptdate')))])) ifTrue: [
      tmp add: l.
    ].
  ].
  tmp
] value) ifTrue: [
      tmp add: o.
    ].
  ].
  tmp
] value.
result := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  late_orders do: [:o |
    | g |
    g := groups at: o at: 'o_orderpriority' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary from:{#o->o}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary from:{'key'->k. 'items'->grp}.
    tmp add: Dictionary from:{'o_orderpriority'->g.key. 'order_count'->(g size)}.
  ].
  tmp := tmp asSortedCollection: [:a :b | g.key < g.key].
  tmp
] value.
json value: result.
((result = ({Dictionary from:{'o_orderpriority'->'1-URGENT'. 'order_count'->1}. Dictionary from:{'o_orderpriority'->'2-HIGH'. 'order_count'->1}}))) ifTrue: [Transcript show:'ok'; cr] ifFalse: [Transcript show:'fail'; cr].
