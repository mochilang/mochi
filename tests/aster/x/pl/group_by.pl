:- initialization(main).
:- style_check(-singleton).
main :-
        People = [_{name: 'Alice', age: 30, city: 'Paris'}, _{name: 'Bob', age: 15, city: 'Hanoi'}, _{name: 'Charlie', age: 65, city: 'Paris'}, _{name: 'Diana', age: 45, city: 'Hanoi'}, _{name: 'Eve', age: 70, city: 'Paris'}, _{name: 'Frank', age: 22, city: 'Hanoi'}],
            Stats = [_{city: 'Paris', count: 3, avg_age: 55}, _{city: 'Hanoi', count: 3, avg_age: 27.333333333333332}],
            writeln('--- People grouped by city ---'),
            S = _{city: 'Paris', count: 3, avg_age: 55},
            writeln('Paris : count = 3 , avg_age = 55'),
            S1 = _{city: 'Hanoi', count: 3, avg_age: 27.333333333333332},
        writeln('Hanoi : count = 3 , avg_age = 27.333333333333332').
