# Errors

- dataset.erl: parse error: parse error: 17:6: unexpected token "-" (expected PostfixExpr)
- dataset_sort_take_limit.erl: parse error: parse error: 40:27: lexer: invalid input text ";\n  _ ->\n  Sorte..."
- group_by.erl: parse error: parse error: 8:3: lexer: invalid input text "#{k => maps:get(..."
- load_save_json.erl: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6:     Rows = mochi_load("", #{format => "json"}),
  7:     mochi_save(Rows, "", #{format => "json"}).
  8: 
  9: 
 10: mochi_load(Path, Opts) ->
- method.erl: parse error: parse error: 10:39: unexpected token "," (expected "}")
- outer_join.erl: parse error: parse error: 6:4: lexer: invalid input text "#{id => 1, name ..."
- q1.erl: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, test_q1_aggregates_revenue_and_quantity_by_returnflag___linestatus/0]).
  4: 
  5: test_q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() ->
  6:     mochi_expect((Result == [#{returnflag => "N", linestatus => "O", sum_qty => 53, sum_base_price => 3000, sum_disc_price => (950 + 1800), sum_charge => ((950 * 1.07) + (1800 * 1.05)), avg_qty => 26.5, avg_price => 1500, avg_disc => 0.07500000000000001, count_order => 2}])).
  7: 
  8: main(_) ->
  9:     Lineitem = [#{l_quantity => 17, l_extendedprice => 1000, l_discount => 0.05, l_tax => 0.07, l_returnflag => "N", l_linestatus => "O", l_shipdate => "1998-08-01"}, #{l_quantity => 36, l_extendedprice => 2000, l_discount => 0.1, l_tax => 0.05, l_returnflag => "N", l_linestatus => "O", l_shipdate => "1998-09-01"}, #{l_quantity => 25, l_extendedprice => 1500, l_discount => 0, l_tax => 0.08, l_returnflag => "R", l_linestatus => "F", l_shipdate => "1998-09-03"}],
 10:     Result = [#{returnflag => maps:get(returnflag, maps:get(key, G)), linestatus => maps:get(linestatus, maps:get(key, G)), sum_qty => mochi_sum([maps:get(l_quantity, X) || X <- G]), sum_base_price => mochi_sum([maps:get(l_extendedprice, X) || X <- G]), sum_disc_price => mochi_sum([(maps:get(l_extendedprice, X) * (1 - maps:get(l_discount, X))) || X <- G]), sum_charge => mochi_sum([((maps:get(l_extendedprice, X) * (1 - maps:get(l_discount, X))) * (1 + maps:get(l_tax, X))) || X <- G]), avg_qty => mochi_avg([maps:get(l_quantity, X) || X <- G]), avg_price => mochi_avg([maps:get(l_extendedprice, X) || X <- G]), avg_disc => mochi_avg([maps:get(l_discount, X) || X <- G]), count_order => mochi_count(G)} || G <- mochi_group_by([Row || Row <- Lineitem, (maps:get(l_shipdate, Row) =< "1998-09-02")], fun(Row) -> #{returnflag => maps:get(l_returnflag, Row), linestatus => maps:get(l_linestatus, Row)} end)],
- right_join.erl: parse error: parse error: 6:4: lexer: invalid input text "#{id => 1, name ..."
- tpch_q1.erl: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, test_q1_aggregates_revenue_and_quantity_by_returnflag___linestatus/0]).
  4: 
  5: test_q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() ->
  6:     mochi_expect((Result == [#{returnflag => "N", linestatus => "O", sum_qty => 53, sum_base_price => 3000, sum_disc_price => (950 + 1800), sum_charge => ((950 * 1.07) + (1800 * 1.05)), avg_qty => 26.5, avg_price => 1500, avg_disc => 0.07500000000000001, count_order => 2}])).
  7: 
  8: main(_) ->
  9:     Lineitem = [#{l_quantity => 17, l_extendedprice => 1000, l_discount => 0.05, l_tax => 0.07, l_returnflag => "N", l_linestatus => "O", l_shipdate => "1998-08-01"}, #{l_quantity => 36, l_extendedprice => 2000, l_discount => 0.1, l_tax => 0.05, l_returnflag => "N", l_linestatus => "O", l_shipdate => "1998-09-01"}, #{l_quantity => 25, l_extendedprice => 1500, l_discount => 0, l_tax => 0.08, l_returnflag => "R", l_linestatus => "F", l_shipdate => "1998-09-03"}],
 10:     Result = [#{returnflag => maps:get(returnflag, maps:get(key, G)), linestatus => maps:get(linestatus, maps:get(key, G)), sum_qty => mochi_sum([maps:get(l_quantity, X) || X <- G]), sum_base_price => mochi_sum([maps:get(l_extendedprice, X) || X <- G]), sum_disc_price => mochi_sum([(maps:get(l_extendedprice, X) * (1 - maps:get(l_discount, X))) || X <- G]), sum_charge => mochi_sum([((maps:get(l_extendedprice, X) * (1 - maps:get(l_discount, X))) * (1 + maps:get(l_tax, X))) || X <- G]), avg_qty => mochi_avg([maps:get(l_quantity, X) || X <- G]), avg_price => mochi_avg([maps:get(l_extendedprice, X) || X <- G]), avg_disc => mochi_avg([maps:get(l_discount, X) || X <- G]), count_order => mochi_count(G)} || G <- mochi_group_by([Row || Row <- Lineitem, (maps:get(l_shipdate, Row) =< "1998-09-02")], fun(Row) -> #{returnflag => maps:get(l_returnflag, Row), linestatus => maps:get(l_linestatus, Row)} end)],
- update_statement.erl: parse error: parse error: 20:52: lexer: invalid input text ";\n  _ ->\n  Item\n..."
