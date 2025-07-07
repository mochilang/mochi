| items groups result |
items := {
  Dictionary newFrom:{#cat->'a'. #val->3}.
  Dictionary newFrom:{#cat->'a'. #val->1}.
  Dictionary newFrom:{#cat->'b'. #val->5}.
  Dictionary newFrom:{#cat->'b'. #val->2}
} asOrderedCollection.

groups := Dictionary new.
items do: [:i |
  | g |
  g := groups at:(i at:#cat) ifAbsentPut:[OrderedCollection new].
  g add:i
].

result := OrderedCollection new.
groups keysAndValuesDo: [:cat :grp |
  total := grp inject:0 into:[:s :x | s + (x at:#val)].
  result add:(Dictionary newFrom:{#cat->cat. #total->total})
].
result := result asSortedCollection: [:a :b | (a at:#total) > (b at:#total)].
Transcript show: result printString; cr.
