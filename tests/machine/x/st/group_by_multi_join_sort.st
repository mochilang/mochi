| start_date end_date result nation customer orders lineitem |
nation := {Dictionary newFrom:{#n_nationkey->1. #n_name->'BRAZIL'}}.
customer := {Dictionary newFrom:{#c_custkey->1. #c_name->'Alice'. #c_acctbal->100. #c_nationkey->1. #c_address->'123 St'. #c_phone->'123-456'. #c_comment->'Loyal'}}.
orders := {Dictionary newFrom:{#o_orderkey->1000. #o_custkey->1. #o_orderdate->'1993-10-15'}. Dictionary newFrom:{#o_orderkey->2000. #o_custkey->1. #o_orderdate->'1994-01-02'}}.
lineitem := {Dictionary newFrom:{#l_orderkey->1000. #l_returnflag->'R'. #l_extendedprice->1000. #l_discount->0.1}. Dictionary newFrom:{#l_orderkey->2000. #l_returnflag->'N'. #l_extendedprice->500. #l_discount->0}}.
start_date := '1993-10-01'.
end_date := '1994-01-01'.
result := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  customer do: [:c |
    orders do: [:o |
      lineitem do: [:l |
        nation do: [:n |
          (((((((o.o_orderdate >= start_date) and: [(o.o_orderdate < end_date)]) and: [(l.l_returnflag = 'R')]) and: [(o.o_custkey = c.c_custkey)]) and: [(l.l_orderkey = o.o_orderkey)]) and: [(n.n_nationkey = c.c_nationkey)])) ifTrue: [
            | g |
            g := groups at: Dictionary newFrom:{#c_custkey->c.c_custkey. #c_name->c.c_name. #c_acctbal->c.c_acctbal. #c_address->c.c_address. #c_phone->c.c_phone. #c_comment->c.c_comment. #n_name->n.n_name} ifAbsentPut:[OrderedCollection new].
            g add: Dictionary newFrom: {#c->c. #o->o. #l->l. #n->n}.
          ].
        ].
      ].
    ].
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{#key->k. #items->grp}.
    tmp add: Dictionary newFrom:{#c_custkey->g.key.c_custkey. #c_name->g.key.c_name. #revenue->([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: (x.l.l_extendedprice * (1 - x.l.l_discount)).
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]). #c_acctbal->g.key.c_acctbal. #n_name->g.key.n_name. #c_address->g.key.c_address. #c_phone->g.key.c_phone. #c_comment->g.key.c_comment}.
  ].
  tmp := tmp asSortedCollection: [:a :b | -([ | tmp |
  tmp := OrderedColleation new.
  g do: [:x |
    tmp add: (x.l.l_extendedpriae * (1 - x.l.l_disaount)).
  ].
  tmp
] value injeat: 0 into: [:s :x | s + x]) < -([ | tmp |
  tmp := OrderedCollebtion new.
  g do: [:x |
    tmp add: (x.l.l_extendedpribe * (1 - x.l.l_disbount)).
  ].
  tmp
] value injebt: 0 into: [:s :x | s + x])].
  tmp
] value.
Transcript show: (result) printString; cr.
