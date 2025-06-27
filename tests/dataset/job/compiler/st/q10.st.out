Smalltalk at: #cast_info put: nil.
Smalltalk at: #char_name put: nil.
Smalltalk at: #company_name put: nil.
Smalltalk at: #company_type put: nil.
Smalltalk at: #matches put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #role_type put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q10_finds_uncredited_voice_actor_in_Russian_movie
    ((result = Array with: (Dictionary from: {'uncredited_voiced_character' -> 'Ivan'. 'russian_movie' -> 'Vodka Dreams'}))) ifFalse: [ self error: 'expect failed' ]
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
char_name := Array with: (Dictionary from: {'id' -> 1. 'name' -> 'Ivan'}) with: (Dictionary from: {'id' -> 2. 'name' -> 'Alex'}).
cast_info := Array with: (Dictionary from: {'movie_id' -> 10. 'person_role_id' -> 1. 'role_id' -> 1. 'note' -> 'Soldier (voice) (uncredited)'}) with: (Dictionary from: {'movie_id' -> 11. 'person_role_id' -> 2. 'role_id' -> 1. 'note' -> '(voice)'}).
company_name := Array with: (Dictionary from: {'id' -> 1. 'country_code' -> '[ru]'}) with: (Dictionary from: {'id' -> 2. 'country_code' -> '[us]'}).
company_type := Array with: (Dictionary from: {'id' -> 1}) with: (Dictionary from: {'id' -> 2}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 10. 'company_id' -> 1. 'company_type_id' -> 1}) with: (Dictionary from: {'movie_id' -> 11. 'company_id' -> 2. 'company_type_id' -> 1}).
role_type := Array with: (Dictionary from: {'id' -> 1. 'role' -> 'actor'}) with: (Dictionary from: {'id' -> 2. 'role' -> 'director'}).
title := Array with: (Dictionary from: {'id' -> 10. 'title' -> 'Vodka Dreams'. 'production_year' -> 2006}) with: (Dictionary from: {'id' -> 11. 'title' -> 'Other Film'. 'production_year' -> 2004}).
matches := ((| res |
res := OrderedCollection new.
((char_name) select: [:chn | ((((((((((((((ci at: 'note' at: 'contains' value: '(voice)') and: [(ci at: 'note' at: 'contains' value: '(uncredited)')]) and: [cn at: 'country_code']) = '[ru]') and: [rt at: 'role']) = 'actor') and: [t at: 'production_year']) > 2005) and: [(chn at: 'id' = ci at: 'person_role_id')]) and: [(rt at: 'id' = ci at: 'role_id')]) and: [(t at: 'id' = ci at: 'movie_id')]) and: [(mc at: 'movie_id' = t at: 'id')]) and: [(cn at: 'id' = mc at: 'company_id')]) and: [(ct at: 'id' = mc at: 'company_type_id')])]) do: [:chn |
    (cast_info) do: [:ci |
        (role_type) do: [:rt |
            (title) do: [:t |
                (movie_companies) do: [:mc |
                    (company_name) do: [:cn |
                        (company_type) do: [:ct |
                            res add: Dictionary from: {'character' -> chn at: 'name'. 'movie' -> t at: 'title'}.
                        ]
                    ]
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'uncredited_voiced_character' -> (Main __min: ((| res |
res := OrderedCollection new.
(matches) do: [:x |
    res add: x at: 'character'.
]
res := res asArray.
res))). 'russian_movie' -> (Main __min: ((| res |
res := OrderedCollection new.
(matches) do: [:x |
    res add: x at: 'movie'.
]
res := res asArray.
res)))}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q10_finds_uncredited_voice_actor_in_Russian_movie.
