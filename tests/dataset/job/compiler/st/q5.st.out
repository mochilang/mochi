Smalltalk at: #candidate_titles put: nil.
Smalltalk at: #company_type put: nil.
Smalltalk at: #info_type put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #movie_info put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q5_finds_the_lexicographically_first_qualifying_title
    ((result = Array with: (Dictionary from: {'typical_european_movie' -> 'A Film'}))) ifFalse: [ self error: 'expect failed' ]
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
company_type := Array with: (Dictionary from: {'ct_id' -> 1. 'kind' -> 'production companies'}) with: (Dictionary from: {'ct_id' -> 2. 'kind' -> 'other'}).
info_type := Array with: (Dictionary from: {'it_id' -> 10. 'info' -> 'languages'}).
title := Array with: (Dictionary from: {'t_id' -> 100. 'title' -> 'B Movie'. 'production_year' -> 2010}) with: (Dictionary from: {'t_id' -> 200. 'title' -> 'A Film'. 'production_year' -> 2012}) with: (Dictionary from: {'t_id' -> 300. 'title' -> 'Old Movie'. 'production_year' -> 2000}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 100. 'company_type_id' -> 1. 'note' -> 'ACME (France) (theatrical)'}) with: (Dictionary from: {'movie_id' -> 200. 'company_type_id' -> 1. 'note' -> 'ACME (France) (theatrical)'}) with: (Dictionary from: {'movie_id' -> 300. 'company_type_id' -> 1. 'note' -> 'ACME (France) (theatrical)'}).
movie_info := Array with: (Dictionary from: {'movie_id' -> 100. 'info' -> 'German'. 'info_type_id' -> 10}) with: (Dictionary from: {'movie_id' -> 200. 'info' -> 'Swedish'. 'info_type_id' -> 10}) with: (Dictionary from: {'movie_id' -> 300. 'info' -> 'German'. 'info_type_id' -> 10}).
candidate_titles := ((| res |
res := OrderedCollection new.
((company_type) select: [:ct | ((((((((mc at: 'note' includes: ct at: 'kind') and: [t at: 'production_year']) > 2005) and: [((Array with: 'Sweden' with: 'Norway' with: 'Germany' with: 'Denmark' with: 'Swedish' with: 'Denish' with: 'Norwegian' with: 'German' includes: mi at: 'info'))]) and: [(mc at: 'company_type_id' = ct at: 'ct_id')]) and: [(mi at: 'movie_id' = mc at: 'movie_id')]) and: [(it at: 'it_id' = mi at: 'info_type_id')]) and: [(t at: 't_id' = mc at: 'movie_id')])]) do: [:ct |
    (movie_companies) do: [:mc |
        (movie_info) do: [:mi |
            (info_type) do: [:it |
                (title) do: [:t |
                    res add: t at: 'title'.
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'typical_european_movie' -> (Main __min: candidate_titles)}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q5_finds_the_lexicographically_first_qualifying_title.
