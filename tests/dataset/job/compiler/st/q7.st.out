Smalltalk at: #aka_name put: nil.
Smalltalk at: #cast_info put: nil.
Smalltalk at: #info_type put: nil.
Smalltalk at: #link_type put: nil.
Smalltalk at: #movie_link put: nil.
Smalltalk at: #name put: nil.
Smalltalk at: #person_info put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #rows put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q7_finds_movie_features_biography_for_person
    ((result = Array with: (Dictionary from: {'of_person' -> 'Alan Brown'. 'biography_movie' -> 'Feature Film'}))) ifFalse: [ self error: 'expect failed' ]
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
aka_name := Array with: (Dictionary from: {'person_id' -> 1. 'name' -> 'Anna Mae'}) with: (Dictionary from: {'person_id' -> 2. 'name' -> 'Chris'}).
cast_info := Array with: (Dictionary from: {'person_id' -> 1. 'movie_id' -> 10}) with: (Dictionary from: {'person_id' -> 2. 'movie_id' -> 20}).
info_type := Array with: (Dictionary from: {'id' -> 1. 'info' -> 'mini biography'}) with: (Dictionary from: {'id' -> 2. 'info' -> 'trivia'}).
link_type := Array with: (Dictionary from: {'id' -> 1. 'link' -> 'features'}) with: (Dictionary from: {'id' -> 2. 'link' -> 'references'}).
movie_link := Array with: (Dictionary from: {'linked_movie_id' -> 10. 'link_type_id' -> 1}) with: (Dictionary from: {'linked_movie_id' -> 20. 'link_type_id' -> 2}).
name := Array with: (Dictionary from: {'id' -> 1. 'name' -> 'Alan Brown'. 'name_pcode_cf' -> 'B'. 'gender' -> 'm'}) with: (Dictionary from: {'id' -> 2. 'name' -> 'Zoe'. 'name_pcode_cf' -> 'Z'. 'gender' -> 'f'}).
person_info := Array with: (Dictionary from: {'person_id' -> 1. 'info_type_id' -> 1. 'note' -> 'Volker Boehm'}) with: (Dictionary from: {'person_id' -> 2. 'info_type_id' -> 1. 'note' -> 'Other'}).
title := Array with: (Dictionary from: {'id' -> 10. 'title' -> 'Feature Film'. 'production_year' -> 1990}) with: (Dictionary from: {'id' -> 20. 'title' -> 'Late Film'. 'production_year' -> 2000}).
rows := ((| res |
res := OrderedCollection new.
((aka_name) select: [:an | ((((((((((((((((((((((((((((((((an at: 'name' at: 'contains' value: 'a') and: [it at: 'info']) = 'mini biography') and: [lt at: 'link']) = 'features') and: [n at: 'name_pcode_cf']) >= 'A') and: [n at: 'name_pcode_cf']) <= 'F') and: [(((n at: 'gender' = 'm') or: [(((n at: 'gender' = 'f') and: [(n at: 'name' at: 'starts_with' value: 'B')]))]))]) and: [pi at: 'note']) = 'Volker Boehm') and: [t at: 'production_year']) >= 1980) and: [t at: 'production_year']) <= 1995) and: [pi at: 'person_id']) = an at: 'person_id') and: [pi at: 'person_id']) = ci at: 'person_id') and: [an at: 'person_id']) = ci at: 'person_id') and: [ci at: 'movie_id']) = ml at: 'linked_movie_id')) and: [(n at: 'id' = an at: 'person_id')]) and: [(pi at: 'person_id' = an at: 'person_id')]) and: [(it at: 'id' = pi at: 'info_type_id')]) and: [(ci at: 'person_id' = n at: 'id')]) and: [(t at: 'id' = ci at: 'movie_id')]) and: [(ml at: 'linked_movie_id' = t at: 'id')]) and: [(lt at: 'id' = ml at: 'link_type_id')])]) do: [:an |
    (name) do: [:n |
        (person_info) do: [:pi |
            (info_type) do: [:it |
                (cast_info) do: [:ci |
                    (title) do: [:t |
                        (movie_link) do: [:ml |
                            (link_type) do: [:lt |
                                res add: Dictionary from: {'person_name' -> n at: 'name'. 'movie_title' -> t at: 'title'}.
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
result := Array with: (Dictionary from: {'of_person' -> (Main __min: ((| res |
res := OrderedCollection new.
(rows) do: [:r |
    res add: r at: 'person_name'.
]
res := res asArray.
res))). 'biography_movie' -> (Main __min: ((| res |
res := OrderedCollection new.
(rows) do: [:r |
    res add: r at: 'movie_title'.
]
res := res asArray.
res)))}).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q7_finds_movie_features_biography_for_person.
