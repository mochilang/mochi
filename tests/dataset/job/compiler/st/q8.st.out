Smalltalk at: #aka_name put: nil.
Smalltalk at: #cast_info put: nil.
Smalltalk at: #company_name put: nil.
Smalltalk at: #eligible put: nil.
Smalltalk at: #movie_companies put: nil.
Smalltalk at: #name put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #role_type put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing
    ((result = Array with: (Dictionary from: {'actress_pseudonym' -> 'Y. S.'. 'japanese_movie_dubbed' -> 'Dubbed Film'}))) ifFalse: [ self error: 'expect failed' ]
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
aka_name := Array with: (Dictionary from: {'person_id' -> 1. 'name' -> 'Y. S.'}).
cast_info := Array with: (Dictionary from: {'person_id' -> 1. 'movie_id' -> 10. 'note' -> '(voice: English version)'. 'role_id' -> 1000}).
company_name := Array with: (Dictionary from: {'id' -> 50. 'country_code' -> '[jp]'}).
movie_companies := Array with: (Dictionary from: {'movie_id' -> 10. 'company_id' -> 50. 'note' -> 'Studio (Japan)'}).
name := Array with: (Dictionary from: {'id' -> 1. 'name' -> 'Yoko Ono'}) with: (Dictionary from: {'id' -> 2. 'name' -> 'Yuichi'}).
role_type := Array with: (Dictionary from: {'id' -> 1000. 'role' -> 'actress'}).
title := Array with: (Dictionary from: {'id' -> 10. 'title' -> 'Dubbed Film'}).
eligible := ((| res |
res := OrderedCollection new.
((aka_name) select: [:an1 | (((((((((((((((ci at: 'note' = '(voice: English version)') and: [cn at: 'country_code']) = '[jp]') and: [(mc at: 'note' at: 'contains' value: '(Japan)')]) and: [(((mc at: 'note' at: 'contains' value: '(USA)')) not)]) and: [(n1 at: 'name' at: 'contains' value: 'Yo')]) and: [(((n1 at: 'name' at: 'contains' value: 'Yu')) not)]) and: [rt at: 'role']) = 'actress') and: [(n1 at: 'id' = an1 at: 'person_id')]) and: [(ci at: 'person_id' = an1 at: 'person_id')]) and: [(t at: 'id' = ci at: 'movie_id')]) and: [(mc at: 'movie_id' = ci at: 'movie_id')]) and: [(cn at: 'id' = mc at: 'company_id')]) and: [(rt at: 'id' = ci at: 'role_id')])]) do: [:an1 |
    (name) do: [:n1 |
        (cast_info) do: [:ci |
            (title) do: [:t |
                (movie_companies) do: [:mc |
                    (company_name) do: [:cn |
                        (role_type) do: [:rt |
                            res add: Dictionary from: {'pseudonym' -> an1 at: 'name'. 'movie_title' -> t at: 'title'}.
                        ]
                    ]
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
result := Array with: (Dictionary from: {'actress_pseudonym' -> (Main __min: ((| res |
res := OrderedCollection new.
(eligible) do: [:x |
    res add: x at: 'pseudonym'.
]
res := res asArray.
res))). 'japanese_movie_dubbed' -> (Main __min: ((| res |
res := OrderedCollection new.
(eligible) do: [:x |
    res add: x at: 'movie_title'.
]
res := res asArray.
res)))}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing.
