| data groups tmp result |
data := {
  Dictionary newFrom:{#tag->'a'. #val->1}.
  Dictionary newFrom:{#tag->'a'. #val->2}.
  Dictionary newFrom:{#tag->'b'. #val->3}
} asOrderedCollection.

groups := Dictionary new.
data do: [:d |
  | g |
  g := groups at:(d at:#tag) ifAbsentPut:[OrderedCollection new].
  g add:d
].

tmp := OrderedCollection new.
groups keysAndValuesDo: [:tag :grp |
  total := 0.
  grp do: [:x | total := total + (x at:#val)].
  tmp add:(Dictionary newFrom:{#tag->tag. #total->total})
].
result := tmp asSortedCollection: [:a :b | (a at:#tag) <= (b at:#tag)].
Transcript show: result printString; cr.
