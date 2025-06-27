Smalltalk at: #allowed_infos put: nil.
Smalltalk at: #candidate_titles put: nil.
Smalltalk at: #keyword put: nil.
Smalltalk at: #movie_info put: nil.
Smalltalk at: #movie_keyword put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q3_returns_lexicographically_smallest_sequel_title
    ((result = Array with: (Dictionary from: {'movie_title' -> 'Alpha'}))) ifFalse: [ self error: 'expect failed' ]
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
keyword := Array with: (Dictionary from: {'id' -> 1. 'keyword' -> 'amazing sequel'}) with: (Dictionary from: {'id' -> 2. 'keyword' -> 'prequel'}).
movie_info := Array with: (Dictionary from: {'movie_id' -> 10. 'info' -> 'Germany'}) with: (Dictionary from: {'movie_id' -> 30. 'info' -> 'Sweden'}) with: (Dictionary from: {'movie_id' -> 20. 'info' -> 'France'}).
movie_keyword := Array with: (Dictionary from: {'movie_id' -> 10. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 30. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 20. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 10. 'keyword_id' -> 2}).
title := Array with: (Dictionary from: {'id' -> 10. 'title' -> 'Alpha'. 'production_year' -> 2006}) with: (Dictionary from: {'id' -> 30. 'title' -> 'Beta'. 'production_year' -> 2008}) with: (Dictionary from: {'id' -> 20. 'title' -> 'Gamma'. 'production_year' -> 2009}).
allowed_infos := Array with: 'Sweden' with: 'Norway' with: 'Germany' with: 'Denmark' with: 'Swedish' with: 'Denish' with: 'Norwegian' with: 'German'.
candidate_titles := ((| res |
res := OrderedCollection new.
((keyword) select: [:k | ((((((((allowed_infos includes: (k at: 'keyword' at: 'contains' value: 'sequel')) and: [t at: 'production_year']) > 2005) and: [mk at: 'movie_id']) = mi at: 'movie_id') and: [(mk at: 'keyword_id' = k at: 'id')]) and: [(mi at: 'movie_id' = mk at: 'movie_id')]) and: [(t at: 'id' = mi at: 'movie_id')])]) do: [:k |
    (movie_keyword) do: [:mk |
        (movie_info) do: [:mi |
            (title) do: [:t |
                res add: t at: 'title'.
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'movie_title' -> (Main __min: candidate_titles)}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q3_returns_lexicographically_smallest_sequel_title.
