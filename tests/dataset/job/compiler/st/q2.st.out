Smalltalk at: #company_name put: nil.
Smalltalk at: #keyword put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #movie_keyword put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #title put: nil.
Smalltalk at: #titles put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q2_finds_earliest_title_for_German_companies_with_character_keyword
    ((result = 'Der Film')) ifFalse: [ self error: 'expect failed' ]
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
company_name := Array with: (Dictionary from: {'id' -> 1. 'country_code' -> '[de]'}) with: (Dictionary from: {'id' -> 2. 'country_code' -> '[us]'}).
keyword := Array with: (Dictionary from: {'id' -> 1. 'keyword' -> 'character-name-in-title'}) with: (Dictionary from: {'id' -> 2. 'keyword' -> 'other'}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 100. 'company_id' -> 1}) with: (Dictionary from: {'movie_id' -> 200. 'company_id' -> 2}).
movie_keyword := Array with: (Dictionary from: {'movie_id' -> 100. 'keyword_id' -> 1}) with: (Dictionary from: {'movie_id' -> 200. 'keyword_id' -> 2}).
title := Array with: (Dictionary from: {'id' -> 100. 'title' -> 'Der Film'}) with: (Dictionary from: {'id' -> 200. 'title' -> 'Other Movie'}).
titles := ((| res |
res := OrderedCollection new.
((company_name) select: [:cn | (((((((((cn at: 'country_code' = '[de]') and: [k at: 'keyword']) = 'character-name-in-title') and: [mc at: 'movie_id']) = mk at: 'movie_id') and: [(mc at: 'company_id' = cn at: 'id')]) and: [(mc at: 'movie_id' = t at: 'id')]) and: [(mk at: 'movie_id' = t at: 'id')]) and: [(mk at: 'keyword_id' = k at: 'id')])]) do: [:cn |
    (movie_companies) do: [:mc |
        (title) do: [:t |
            (movie_keyword) do: [:mk |
                (keyword) do: [:k |
                    res add: t at: 'title'.
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := (Main __min: titles).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q2_finds_earliest_title_for_German_companies_with_character_keyword.
