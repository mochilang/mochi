:- initialization(main).
:- style_check(-singleton).

main :-
    Nations = [{id: 1, name: "A"}, {id: 2, name: "B"}],
    Suppliers = [{id: 1, nation: 1}, {id: 2, nation: 2}],
    Partsupp = [{part: 100, supplier: 1, cost: 10, qty: 2}, {part: 100, supplier: 2, cost: 20, qty: 1}, {part: 200, supplier: 1, cost: 5, qty: 3}],
    Filtered = [],
    Grouped = [],
.
