:- style_check(-singleton).
:- initialization(main).
main :-
        People = [_{name: 'Alice', age: 30}, _{name: 'Bob', age: 15}, _{name: 'Charlie', age: 65}, _{name: 'Diana', age: 45}],
            Adults = [_{name: 'Alice', age: 30, is_senior: false}, _{name: 'Charlie', age: 65, is_senior: true}, _{name: 'Diana', age: 45, is_senior: false}],
            writeln('--- Adults ---'),
            Person = _{name: 'Alice', age: 30, is_senior: false},
            writeln('Alice is 30 '),
            Person1 = _{name: 'Charlie', age: 65, is_senior: true},
            writeln('Charlie is 65  (senior)'),
            Person2 = _{name: 'Diana', age: 45, is_senior: false},
        writeln('Diana is 45 ').
