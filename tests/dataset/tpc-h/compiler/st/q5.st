| region nation customer supplier orders lineitem asia_nations local_customer_supplier_orders result |
region := {Dictionary from:{'r_regionkey'->0. 'r_name'->'ASIA'}. Dictionary from:{'r_regionkey'->1. 'r_name'->'EUROPE'}}.
nation := {Dictionary from:{'n_nationkey'->10. 'n_regionkey'->0. 'n_name'->'JAPAN'}. Dictionary from:{'n_nationkey'->20. 'n_regionkey'->0. 'n_name'->'INDIA'}. Dictionary from:{'n_nationkey'->30. 'n_regionkey'->1. 'n_name'->'FRANCE'}}.
customer := {Dictionary from:{'c_custkey'->1. 'c_nationkey'->10}. Dictionary from:{'c_custkey'->2. 'c_nationkey'->20}}.
supplier := {Dictionary from:{'s_suppkey'->100. 's_nationkey'->10}. Dictionary from:{'s_suppkey'->200. 's_nationkey'->20}}.
orders := {Dictionary from:{'o_orderkey'->1000. 'o_custkey'->1. 'o_orderdate'->'1994-03-15'}. Dictionary from:{'o_orderkey'->2000. 'o_custkey'->2. 'o_orderdate'->'1994-06-10'}. Dictionary from:{'o_orderkey'->3000. 'o_custkey'->2. 'o_orderdate'->'1995-01-01'}}.
lineitem := {Dictionary from:{'l_orderkey'->1000. 'l_suppkey'->100. 'l_extendedprice'->1000. 'l_discount'->0.05}. Dictionary from:{'l_orderkey'->2000. 'l_suppkey'->200. 'l_extendedprice'->800. 'l_discount'->0.1}. Dictionary from:{'l_orderkey'->3000. 'l_suppkey'->200. 'l_extendedprice'->900. 'l_discount'->0.05}}.
asia_nations := [ | tmp |
  tmp := OrderedCollection new.
  region do: [:r |
    nation do: [:n |
      ((((r at: 'r_name') = 'ASIA') and: [(n.n_regionkey = r.r_regionkey)])) ifTrue: [
        tmp add: n.
      ].
    ].
  ].
  tmp
] value.
local_customer_supplier_orders := [ | tmp |
  tmp := OrderedCollection new.
  customer do: [:c |
    asia_nations do: [:n |
      orders do: [:o |
        lineitem do: [:l |
          supplier do: [:s |
            (((((((((((o at: 'o_orderdate') >= '1994-01-01')) and: [(((o at: 'o_orderdate') < '1995-01-01'))])) and: [(((s at: 's_nationkey') = (c at: 'c_nationkey')))]) and: [(c.c_nationkey = n.n_nationkey)]) and: [(o.o_custkey = c.c_custkey)]) and: [(l.l_orderkey = o.o_orderkey)]) and: [(s.s_suppkey = l.l_suppkey)])) ifTrue: [
              tmp add: Dictionary from:{nation->n at: 'n_name'. 'revenue'->((l at: 'l_extendedprice') * ((1 - (l at: 'l_discount'))))}.
            ].
          ].
        ].
      ].
    ].
  ].
  tmp
] value.
result := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  local_customer_supplier_orders do: [:r |
    | g |
    g := groups at: r at: 'nation' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary from:{#r->r}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary from:{'key'->k. 'items'->grp}.
    tmp add: Dictionary from:{'n_name'->g.key. 'revenue'->([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: x at: 'revenue'.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])}.
  ].
  tmp := tmp asSortedCollection: [:a :b | -([ | tmp |
  tmp := OadeaedCollection new.
  g do: [:x |
    tmp add: x at: 'aevenue'.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]) < -([ | tmp |
  tmp := ObdebedCollection new.
  g do: [:x |
    tmp add: x at: 'bevenue'.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])].
  tmp
] value.
json value: result.
((result = ({Dictionary from:{'n_name'->'JAPAN'. 'revenue'->950}. Dictionary from:{'n_name'->'INDIA'. 'revenue'->720}}))) ifTrue: [Transcript show:'ok'; cr] ifFalse: [Transcript show:'fail'; cr].
