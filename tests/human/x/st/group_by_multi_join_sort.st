| nation customer orders lineitem startDate endDate filtered groups result |
nation := { Dictionary newFrom:{#n_nationkey->1. #n_name->'BRAZIL'} } asOrderedCollection.
customer := { Dictionary newFrom:{#c_custkey->1. #c_name->'Alice'. #c_acctbal->100.0. #c_nationkey->1. #c_address->'123 St'. #c_phone->'123-456'. #c_comment->'Loyal'} } asOrderedCollection.
orders := {
  Dictionary newFrom:{#o_orderkey->1000. #o_custkey->1. #o_orderdate->'1993-10-15'}.
  Dictionary newFrom:{#o_orderkey->2000. #o_custkey->1. #o_orderdate->'1994-01-02'}
} asOrderedCollection.
lineitem := {
  Dictionary newFrom:{#l_orderkey->1000. #l_returnflag->'R'. #l_extendedprice->1000.0. #l_discount->0.1}.
  Dictionary newFrom:{#l_orderkey->2000. #l_returnflag->'N'. #l_extendedprice->500.0. #l_discount->0.0}
} asOrderedCollection.
startDate := '1993-10-01'.
endDate := '1994-01-01'.

filtered := OrderedCollection new.
customer do: [:c |
  orders do: [:o |
    ((o at:#o_custkey) = (c at:#c_custkey) and: [ (o at:#o_orderdate) >= startDate and: [ (o at:#o_orderdate) < endDate ] ]) ifTrue:[
      lineitem do: [:l |
        ((l at:#l_orderkey) = (o at:#o_orderkey) and: [ (l at:#l_returnflag) = 'R' ]) ifTrue:[
          n := nation detect: [:nn | (nn at:#n_nationkey) = (c at:#c_nationkey)].
          filtered add:(Dictionary newFrom:{#c->c. #o->o. #l->l. #n->n})
        ]
      ]
    ]
  ]
].

groups := Dictionary new.
filtered do: [:x |
  key := (Dictionary newFrom:{
    #c_custkey->((x at:#c) at:#c_custkey).
    #c_name->((x at:#c) at:#c_name).
    #c_acctbal->((x at:#c) at:#c_acctbal).
    #c_address->((x at:#c) at:#c_address).
    #c_phone->((x at:#c) at:#c_phone).
    #c_comment->((x at:#c) at:#c_comment).
    #n_name->((x at:#n) at:#n_name)
  }).
  g := groups at: key ifAbsentPut:[OrderedCollection new].
  g add:x
].

result := OrderedCollection new.
groups keysAndValuesDo: [:key :grp |
  revenue := grp inject:0 into:[:s :v | s + ((v at:#l) at:#l_extendedprice) * (1 - ((v at:#l) at:#l_discount))].
  result add:(Dictionary newFrom:{
    #c_custkey->(key at:#c_custkey).
    #c_name->(key at:#c_name).
    #revenue->revenue.
    #c_acctbal->(key at:#c_acctbal).
    #n_name->(key at:#n_name).
    #c_address->(key at:#c_address).
    #c_phone->(key at:#c_phone).
    #c_comment->(key at:#c_comment)
  })
].
result := result asSortedCollection: [:a :b | (a at:#revenue) > (b at:#revenue)].
Transcript show: result printString; cr.
