Smalltalk at: #company_type put: nil.
Smalltalk at: #filtered put: nil.
Smalltalk at: #info_type put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #movie_info_idx put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production
    ((result = Dictionary from: {'production_note' -> 'ACME (co-production)'. 'movie_title' -> 'Good Movie'. 'movie_year' -> 1995})) ifFalse: [ self error: 'expect failed' ]
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
company_type := Array with: (Dictionary from: {'id' -> 1. 'kind' -> 'production companies'}) with: (Dictionary from: {'id' -> 2. 'kind' -> 'distributors'}).
info_type := Array with: (Dictionary from: {'id' -> 10. 'info' -> 'top 250 rank'}) with: (Dictionary from: {'id' -> 20. 'info' -> 'bottom 10 rank'}).
title := Array with: (Dictionary from: {'id' -> 100. 'title' -> 'Good Movie'. 'production_year' -> 1995}) with: (Dictionary from: {'id' -> 200. 'title' -> 'Bad Movie'. 'production_year' -> 2000}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 100. 'company_type_id' -> 1. 'note' -> 'ACME (co-production)'}) with: (Dictionary from: {'movie_id' -> 200. 'company_type_id' -> 1. 'note' -> 'MGM (as Metro-Goldwyn-Mayer Pictures)'}).
movie_info_idx := Array with: (Dictionary from: {'movie_id' -> 100. 'info_type_id' -> 10}) with: (Dictionary from: {'movie_id' -> 200. 'info_type_id' -> 20}).
filtered := ((| res |
res := OrderedCollection new.
((company_type) select: [:ct | (((((((((ct at: 'kind' = 'production companies') and: [it at: 'info']) = 'top 250 rank') and: [(((mc at: 'note' at: 'contains' value: '(as Metro-Goldwyn-Mayer Pictures)')) not)]) and: [(((mc at: 'note' at: 'contains' value: '(co-production)') or: [(mc at: 'note' at: 'contains' value: '(presents)')]))]) and: [(ct at: 'id' = mc at: 'company_type_id')]) and: [(t at: 'id' = mc at: 'movie_id')]) and: [(mi at: 'movie_id' = t at: 'id')]) and: [(it at: 'id' = mi at: 'info_type_id')])]) do: [:ct |
    (movie_companies) do: [:mc |
        (title) do: [:t |
            (movie_info_idx) do: [:mi |
                (info_type) do: [:it |
                    res add: Dictionary from: {'note' -> mc at: 'note'. 'title' -> t at: 'title'. 'year' -> t at: 'production_year'}.
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Dictionary from: {'production_note' -> (Main __min: ((| res |
res := OrderedCollection new.
(filtered) do: [:r |
    res add: r at: 'note'.
]
res := res asArray.
res))). 'movie_title' -> (Main __min: ((| res |
res := OrderedCollection new.
(filtered) do: [:r |
    res add: r at: 'title'.
]
res := res asArray.
res))). 'movie_year' -> (Main __min: ((| res |
res := OrderedCollection new.
(filtered) do: [:r |
    res add: r at: 'year'.
]
res := res asArray.
res)))}.
(Array with: result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production.
