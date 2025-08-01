:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("TOP COVERAGE = 0.409167"),
    writeln(""),
    writeln("NAME HIERARCHY                 | WEIGHT | COVERAGE |"),
    writeln("cleaning|    1   | 0.409167 |"),
    writeln("house1|   40   | 0.331250 |"),
    writeln("bedrooms|    1   | 0.250000 |"),
    writeln("bathrooms|    1   | 0.500000 |"),
    writeln("bathroom1|    1   | 0.500000 |"),
    writeln("bathroom2|    1   | 0.000000 |"),
    writeln("outside_lavatory|    1   | 1.000000 |"),
    writeln("attic|    1   | 0.750000 |"),
    writeln("kitchen|    1   | 0.100000 |"),
    writeln("living_rooms|    1   | 0.250000 |"),
    writeln("lounge|    1   | 0.000000 |"),
    writeln("dining_room|    1   | 0.000000 |"),
    writeln("conservatory|    1   | 0.000000 |"),
    writeln("playroom|    1   | 1.000000 |"),
    writeln("basement|    1   | 0.000000 |"),
    writeln("garage|    1   | 0.000000 |"),
    writeln("garden|    1   | 0.800000 |"),
    writeln("house2|   60   | 0.461111 |"),
    writeln("upstairs|    1   | 0.150000 |"),
    writeln("bedrooms|    1   | 0.000000 |"),
    writeln("suite_1|    1   | 0.000000 |"),
    writeln("suite_2|    1   | 0.000000 |"),
    writeln("bedroom_3|    1   | 0.000000 |"),
    writeln("bedroom_4|    1   | 0.000000 |"),
    writeln("bathroom|    1   | 0.000000 |"),
    writeln("toilet|    1   | 0.000000 |"),
    writeln("attics|    1   | 0.600000 |"),
    writeln("groundfloor|    1   | 0.316667 |"),
    writeln("kitchen|    1   | 0.000000 |"),
    writeln("living_rooms|    1   | 0.000000 |"),
    writeln("lounge|    1   | 0.000000 |"),
    writeln("dining_room|    1   | 0.000000 |"),
    writeln("conservatory|    1   | 0.000000 |"),
    writeln("playroom|    1   | 0.000000 |"),
    writeln("wet_room_&_toilet|    1   | 0.000000 |"),
    writeln("garage|    1   | 0.000000 |"),
    writeln("garden|    1   | 0.900000 |"),
    writeln("hot_tub_suite|    1   | 1.000000 |"),
    writeln("basement|    1   | 0.916667 |"),
    writeln("cellars|    1   | 1.000000 |"),
    writeln("wine_cellar|    1   | 1.000000 |"),
    writeln("cinema|    1   | 0.750000 |"),
    writeln(""),
    writeln("If the coverage of the Cinema node were increased from 0.75 to 1"),
    writeln("the top level coverage would increase by 0.016667 to 0.425833").
