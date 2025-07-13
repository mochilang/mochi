Smalltalk at: #cast_info put: nil.
Smalltalk at: #keyword put: nil.
Smalltalk at: #movie_keyword put: nil.
Smalltalk at: #name put: nil.
Smalltalk at: #result put: nil.
Smalltalk at: #title put: nil.

Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'tests'!
test_Q6_finds_marvel_movie_with_Robert_Downey
    ((result = Array with: (Dictionary from: {'movie_keyword' -> 'marvel-cinematic-universe'. 'actor_name' -> 'Downey Robert Jr.'. 'marvel_movie' -> 'Iron Man 3'}))) ifFalse: [ self error: 'expect failed' ]
!

!!
cast_info := Array with: (Dictionary from: {'movie_id' -> 1. 'person_id' -> 101}) with: (Dictionary from: {'movie_id' -> 2. 'person_id' -> 102}).
keyword := Array with: (Dictionary from: {'id' -> 100. 'keyword' -> 'marvel-cinematic-universe'}) with: (Dictionary from: {'id' -> 200. 'keyword' -> 'other'}).
movie_keyword := Array with: (Dictionary from: {'movie_id' -> 1. 'keyword_id' -> 100}) with: (Dictionary from: {'movie_id' -> 2. 'keyword_id' -> 200}).
name := Array with: (Dictionary from: {'id' -> 101. 'name' -> 'Downey Robert Jr.'}) with: (Dictionary from: {'id' -> 102. 'name' -> 'Chris Evans'}).
title := Array with: (Dictionary from: {'id' -> 1. 'title' -> 'Iron Man 3'. 'production_year' -> 2013}) with: (Dictionary from: {'id' -> 2. 'title' -> 'Old Movie'. 'production_year' -> 2000}).
result := ((| res |
res := OrderedCollection new.
((cast_info) select: [:ci | (((((((((k at: 'keyword' = 'marvel-cinematic-universe') and: [(n at: 'name' at: 'contains' value: 'Downey')]) and: [(n at: 'name' at: 'contains' value: 'Robert')]) and: [t at: 'production_year']) > 2010) and: [(ci at: 'movie_id' = mk at: 'movie_id')]) and: [(mk at: 'keyword_id' = k at: 'id')]) and: [(ci at: 'person_id' = n at: 'id')]) and: [(ci at: 'movie_id' = t at: 'id')])]) do: [:ci |
    (movie_keyword) do: [:mk |
        (keyword) do: [:k |
            (name) do: [:n |
                (title) do: [:t |
                    res add: Dictionary from: {'movie_keyword' -> k at: 'keyword'. 'actor_name' -> n at: 'name'. 'marvel_movie' -> t at: 'title'}.
                ]
            ]
        ]
    ]
]
res := res asArray.
res)).
(result toJSON) displayOn: Transcript. Transcript cr.
Main test_Q6_finds_marvel_movie_with_Robert_Downey.
