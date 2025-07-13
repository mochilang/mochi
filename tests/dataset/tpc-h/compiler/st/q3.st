| customer orders lineitem cutoff segment building_customers valid_orders valid_lineitems order_line_join |
customer := {Dictionary from:{'c_custkey'->1. 'c_mktsegment'->'BUILDING'}. Dictionary from:{'c_custkey'->2. 'c_mktsegment'->'AUTOMOBILE'}}.
orders := {Dictionary from:{'o_orderkey'->100. 'o_custkey'->1. 'o_orderdate'->'1995-03-14'. 'o_shippriority'->1}. Dictionary from:{'o_orderkey'->200. 'o_custkey'->2. 'o_orderdate'->'1995-03-10'. 'o_shippriority'->2}}.
lineitem := {Dictionary from:{'l_orderkey'->100. 'l_extendedprice'->1000. 'l_discount'->0.05. 'l_shipdate'->'1995-03-16'}. Dictionary from:{'l_orderkey'->100. 'l_extendedprice'->500. 'l_discount'->0. 'l_shipdate'->'1995-03-20'}. Dictionary from:{'l_orderkey'->200. 'l_extendedprice'->1000. 'l_discount'->0.1. 'l_shipdate'->'1995-03-14'}}.
cutoff := '1995-03-15'.
segment := 'BUILDING'.
building_customers := [ | tmp |
  tmp := OrderedCollection new.
  customer do: [:c |
    (((c at: 'c_mktsegment') = segment)) ifTrue: [
      tmp add: c.
    ].
  ].
  tmp
] value.
valid_orders := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    building_customers do: [:c |
      ((((o at: 'o_orderdate') < cutoff) and: [(o.o_custkey = c.c_custkey)])) ifTrue: [
        tmp add: o.
      ].
    ].
  ].
  tmp
] value.
valid_lineitems := [ | tmp |
  tmp := OrderedCollection new.
  lineitem do: [:l |
    (((l at: 'l_shipdate') > cutoff)) ifTrue: [
      tmp add: l.
    ].
  ].
  tmp
] value.
order_line_join := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  valid_orders do: [:o |
    valid_lineitems do: [:l |
      ((l.l_orderkey = o.o_orderkey)) ifTrue: [
        | g |
        g := groups at: Dictionary from:{'o_orderkey'->o at: 'o_orderkey'. 'o_orderdate'->o at: 'o_orderdate'. 'o_shippriority'->o at: 'o_shippriority'} ifAbsentPut:[OrderedCollection new].
        g add: Dictionary from:{#o->o. #l->l}.
      ].
    ].
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary from:{'key'->k. 'items'->grp}.
    tmp add: Dictionary from:{'l_orderkey'->g.key.o_orderkey. 'revenue'->([ | tmp |
  tmp := OrderedCollection new.
  g do: [:r |
    tmp add: ((r at: 'l' at: 'l_extendedprice') * ((1 - (r at: 'l' at: 'l_discount')))).
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]). 'o_orderdate'->g.key.o_orderdate. 'o_shippriority'->g.key.o_shippriority}.
  ].
  tmp := tmp asSortedCollection: [:a :b | {-([ | tmp |
  tmp := OrderedCallectian new.
  g da: [:r |
    tmp add: ((r at: 'l' at: 'l_extendedprice') * ((1 - (r at: 'l' at: 'l_discaunt')))).
  ].
  tmp
] value inject: 0 inta: [:s :x | s + x]). g.key.a_arderdate} < {-([ | tmp |
  tmp := OrderedCbllectibn new.
  g db: [:r |
    tmp add: ((r at: 'l' at: 'l_extendedprice') * ((1 - (r at: 'l' at: 'l_discbunt')))).
  ].
  tmp
] value inject: 0 intb: [:s :x | s + x]). g.key.b_brderdate}].
  tmp
] value.
json value: order_line_join.
((order_line_join = ({Dictionary from:{'l_orderkey'->100. 'revenue'->(((1000 * 0.95)) + 500). 'o_orderdate'->'1995-03-14'. 'o_shippriority'->1}}))) ifTrue: [Transcript show:'ok'; cr] ifFalse: [Transcript show:'fail'; cr].
