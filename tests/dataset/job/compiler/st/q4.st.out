Smalltalk at: #info_type put: nil.
Smalltalk at: #keyword put: nil.
Smalltalk at: #movie_info_idx put: nil.
Smalltalk at: #movie_keyword put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #rows put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q4_returns_minimum_rating_and_title_for_sequels
    ((result = Array with: (Dictionary from: {'rating' -> '6.2'. 'movie_title' -> 'Alpha Movie'}))) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'runtime'!
__min: v
    (v respondsTo: #do:) ifFalse: [ ^ self error: 'min() expects collection' ]
    | m |
    m := nil.
    v do: [:it | m isNil ifTrue: [ m := it ] ifFalse: [ (it < m) ifTrue: [ m := it ] ] ].
    ^ m
!
!!
info_type := Array with: (Dictionary from: {'id' -> 1. 'info' -> 'rating'}) with: (Dictionary from: {'id' -> 2. 'info' -> 'other'}).
keyword := Array with: (Dictionary from: {'id' -> 1. 'keyword' -> 'great sequel'}) with: (Dictionary from: {'id' -> 2. 'keyword' -> 'prequel'}).
title := Array with: (Dictionary from: {'id' -> 10. 'title' -> 'Alpha Movie'. 'production_year' -> 2006}) with: (Dictionary from: {'id' -> 20. 'title' -> 'Beta Film'. 'production_year' -> 2007}) with: (Dictionary from: {'id' -> 30. 'title' -> 'Old Film'. 'production_year' -> 2004}).
movie_keyword := Array with: (Dictionary from: {'movie_id' -> 10. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 20. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 30. 'keyword_id' -> 1}).
movie_info_idx := Array with: (Dictionary from: {'movie_id' -> 10. 'info_type_id' -> 1. 'info' -> '6.2'}) with: (Dictionary from: {'movie_id' -> 20. 'info_type_id' -> 1. 'info' -> '7.8'}) with: (Dictionary from: {'movie_id' -> 30. 'info_type_id' -> 1. 'info' -> '4.5'}).
rows := ((| res |
res := OrderedCollection new.
((info_type) select: [:it | ((((((((((((it at: 'info' = 'rating') and: [(k at: 'keyword' at: 'contains' value: 'sequel')]) and: [mi at: 'info']) > '5.0') and: [t at: 'production_year']) > 2005) and: [mk at: 'movie_id']) = mi at: 'movie_id') and: [(it at: 'id' = mi at: 'info_type_id')]) and: [(t at: 'id' = mi at: 'movie_id')]) and: [(mk at: 'movie_id' = t at: 'id')]) and: [(k at: 'id' = mk at: 'keyword_id')])]) do: [:it |
    (movie_info_idx) do: [:mi |
        (title) do: [:t |
            (movie_keyword) do: [:mk |
                (keyword) do: [:k |
                    res add: Dictionary from: {'rating' -> mi at: 'info'. 'title' -> t at: 'title'}.
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'rating' -> (Main __min: ((| res |
res := OrderedCollection new.
(rows) do: [:r |
    res add: r at: 'rating'.
]
res := res asArray.
res))). 'movie_title' -> (Main __min: ((| res |
res := OrderedCollection new.
(rows) do: [:r |
    res add: r at: 'title'.
]
res := res asArray.
res)))}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q4_returns_minimum_rating_and_title_for_sequels.
