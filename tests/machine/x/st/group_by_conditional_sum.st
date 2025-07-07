| items groups result |
items := {
  Dictionary newFrom: {#cat->'a'. #val->10. #flag->true}.
  Dictionary newFrom: {#cat->'a'. #val->5. #flag->false}.
  Dictionary newFrom: {#cat->'b'. #val->20. #flag->true}
} asOrderedCollection.

groups := Dictionary new.
items do: [:i |
  | g |
  g := groups at: (i at: #cat) ifAbsentPut: [OrderedCollection new].
  g add: i
].

result := OrderedCollection new.
groups keysAndValuesDo: [:cat :grp |
  | sumFlag sumAll share |
  sumFlag := grp inject: 0 into: [:s :x | s + (((x at: #flag) ifTrue: [x at: #val] ifFalse: [0]))].
  sumAll := grp inject: 0 into: [:s :x | s + (x at: #val)].
  share := sumFlag / sumAll.
  result add: (Dictionary newFrom: {#cat->cat. #share->share})
].
result := result asSortedCollection: [:a :b | (a at: #cat) <= (b at: #cat)].
Transcript show: result printString; cr.
