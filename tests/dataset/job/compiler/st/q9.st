Smalltalk at: #aka_name put: nil.
Smalltalk at: #cast_info put: nil.
Smalltalk at: #char_name put: nil.
Smalltalk at: #company_name put: nil.
Smalltalk at: #matches put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #name put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #role_type put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q9_selects_minimal_alternative_name__character_and_movie
    ((result = Array with: (Dictionary from: {'alternative_name' -> 'A. N. G.'. 'character_name' -> 'Angel'. 'movie' -> 'Famous Film'}))) ifFalse: [ self error: 'expect failed' ]
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
aka_name := Array with: (Dictionary from: {'person_id' -> 1. 'name' -> 'A. N. G.'}) with: (Dictionary from: {'person_id' -> 2. 'name' -> 'J. D.'}).
char_name := Array with: (Dictionary from: {'id' -> 10. 'name' -> 'Angel'}) with: (Dictionary from: {'id' -> 20. 'name' -> 'Devil'}).
cast_info := Array with: (Dictionary from: {'person_id' -> 1. 'person_role_id' -> 10. 'movie_id' -> 100. 'role_id' -> 1000. 'note' -> '(voice)'}) with: (Dictionary from: {'person_id' -> 2. 'person_role_id' -> 20. 'movie_id' -> 200. 'role_id' -> 1000. 'note' -> '(voice)'}).
company_name := Array with: (Dictionary from: {'id' -> 100. 'country_code' -> '[us]'}) with: (Dictionary from: {'id' -> 200. 'country_code' -> '[gb]'}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 100. 'company_id' -> 100. 'note' -> 'ACME Studios (USA)'}) with: (Dictionary from: {'movie_id' -> 200. 'company_id' -> 200. 'note' -> 'Maple Films'}).
name := Array with: (Dictionary from: {'id' -> 1. 'name' -> 'Angela Smith'. 'gender' -> 'f'}) with: (Dictionary from: {'id' -> 2. 'name' -> 'John Doe'. 'gender' -> 'm'}).
role_type := Array with: (Dictionary from: {'id' -> 1000. 'role' -> 'actress'}) with: (Dictionary from: {'id' -> 2000. 'role' -> 'actor'}).
title := Array with: (Dictionary from: {'id' -> 100. 'title' -> 'Famous Film'. 'production_year' -> 2010}) with: (Dictionary from: {'id' -> 200. 'title' -> 'Old Movie'. 'production_year' -> 1999}).
matches := ((| res |
res := OrderedCollection new.
((aka_name) select: [:an | (((((((((((((((((((((Array with: '(voice)' with: ('(voice: Japanese version)') with: ('(voice) (uncredited)') with: ('(voice: English version)') includes: ci at: 'note')) and: [cn at: 'country_code']) = '[us]') and: [(((mc at: 'note' at: 'contains' value: '(USA)') or: [(mc at: 'note' at: 'contains' value: '(worldwide)')]))]) and: [n at: 'gender']) = 'f') and: [(n at: 'name' at: 'contains' value: 'Ang')]) and: [rt at: 'role']) = 'actress') and: [t at: 'production_year']) >= 2005) and: [t at: 'production_year']) <= 2015) and: [(an at: 'person_id' = n at: 'id')]) and: [(ci at: 'person_id' = n at: 'id')]) and: [(chn at: 'id' = ci at: 'person_role_id')]) and: [(t at: 'id' = ci at: 'movie_id')]) and: [(mc at: 'movie_id' = t at: 'id')]) and: [(cn at: 'id' = mc at: 'company_id')]) and: [(rt at: 'id' = ci at: 'role_id')])]) do: [:an |
    (name) do: [:n |
        (cast_info) do: [:ci |
            (char_name) do: [:chn |
                (title) do: [:t |
                    (movie_companies) do: [:mc |
                        (company_name) do: [:cn |
                            (role_type) do: [:rt |
                                res add: Dictionary from: {'alt' -> an at: 'name'. 'character' -> chn at: 'name'. 'movie' -> t at: 'title'}.
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'alternative_name' -> (Main __min: ((| res |
res := OrderedCollection new.
(matches) do: [:x |
    res add: x at: 'alt'.
]
res := res asArray.
res))). 'character_name' -> (Main __min: ((| res |
res := OrderedCollection new.
(matches) do: [:x |
    res add: x at: 'character'.
]
res := res asArray.
res))). 'movie' -> (Main __min: ((| res |
res := OrderedCollection new.
(matches) do: [:x |
    res add: x at: 'movie'.
]
res := res asArray.
res)))}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q9_selects_minimal_alternative_name__character_and_movie.
