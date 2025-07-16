object Main {
    def test_TPCDS_Q4_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val customer: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val store_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val catalog_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val web_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val year_total: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = (((_group_by((() => {
    val src = customer
    val res = _query(src, Seq(
        Map("items" -> store_sales, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val s = args(1)
    (c.c_customer_sk == s.ss_customer_sk)
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val s = args(1)
    val d = args(2)
    (s.ss_sold_date_sk == d.d_date_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val c = args(0)
    c
})))
    res
})(), (c: Any) => scala.collection.mutable.Map("id" -> c.c_customer_id, "first" -> c.c_first_name, "last" -> c.c_last_name, "login" -> c.c_login, "year" -> d.d_year)).map(g => scala.collection.mutable.Map("customer_id" -> g.key.id, "customer_first_name" -> g.key.first, "customer_last_name" -> g.key.last, "customer_login" -> g.key.login, "dyear" -> g.key.year, "year_total" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    ((((((x.ss_ext_list_price - x.ss_ext_wholesale_cost) - x.ss_ext_discount_amt)) + x.ss_ext_sales_price)) / 2)
})))
    res
})()), "sale_type" -> "s")).toSeq) ++ (_group_by((() => {
    val src = customer
    val res = _query(src, Seq(
        Map("items" -> catalog_sales, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val cs = args(1)
    (c.c_customer_sk == cs.cs_bill_customer_sk)
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val cs = args(1)
    val d = args(2)
    (cs.cs_sold_date_sk == d.d_date_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val c = args(0)
    c
})))
    res
})(), (c: Any) => scala.collection.mutable.Map("id" -> c.c_customer_id, "first" -> c.c_first_name, "last" -> c.c_last_name, "login" -> c.c_login, "year" -> d.d_year)).map(g => scala.collection.mutable.Map("customer_id" -> g.key.id, "customer_first_name" -> g.key.first, "customer_last_name" -> g.key.last, "customer_login" -> g.key.login, "dyear" -> g.key.year, "year_total" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    ((((((x.cs_ext_list_price - x.cs_ext_wholesale_cost) - x.cs_ext_discount_amt)) + x.cs_ext_sales_price)) / 2)
})))
    res
})()), "sale_type" -> "c")).toSeq)).distinct ++ (_group_by((() => {
    val src = customer
    val res = _query(src, Seq(
        Map("items" -> web_sales, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val ws = args(1)
    (c.c_customer_sk == ws.ws_bill_customer_sk)
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val c = args(0)
    val ws = args(1)
    val d = args(2)
    (ws.ws_sold_date_sk == d.d_date_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val c = args(0)
    c
})))
    res
})(), (c: Any) => scala.collection.mutable.Map("id" -> c.c_customer_id, "first" -> c.c_first_name, "last" -> c.c_last_name, "login" -> c.c_login, "year" -> d.d_year)).map(g => scala.collection.mutable.Map("customer_id" -> g.key.id, "customer_first_name" -> g.key.first, "customer_last_name" -> g.key.last, "customer_login" -> g.key.login, "dyear" -> g.key.year, "year_total" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    ((((((x.ws_ext_list_price - x.ws_ext_wholesale_cost) - x.ws_ext_discount_amt)) + x.ws_ext_sales_price)) / 2)
})))
    res
})()), "sale_type" -> "w")).toSeq)).distinct
        val result: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = (() => {
    val src = year_total
    val res = _query(src, Seq(
        Map("items" -> year_total, "on" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    (s2.customer_id == s1.customer_id)
})),
        Map("items" -> year_total, "on" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    (c1.customer_id == s1.customer_id)
})),
        Map("items" -> year_total, "on" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    (c2.customer_id == s1.customer_id)
})),
        Map("items" -> year_total, "on" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    val w1 = args(4)
    (w1.customer_id == s1.customer_id)
})),
        Map("items" -> year_total, "on" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    val w1 = args(4)
    val w2 = args(5)
    (w2.customer_id == s1.customer_id)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    val w1 = args(4)
    val w2 = args(5)
    scala.collection.mutable.Map("customer_id" -> s2.customer_id, "customer_first_name" -> s2.customer_first_name, "customer_last_name" -> s2.customer_last_name, "customer_login" -> s2.customer_login)
}), "where" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    val w1 = args(4)
    val w2 = args(5)
    (((((((((((((((((s1.sale_type == "s") && (c1.sale_type == "c")) && (w1.sale_type == "w")) && (s2.sale_type == "s")) && (c2.sale_type == "c")) && (w2.sale_type == "w")) && (s1.dyear == 2001)) && (s2.dyear == 2002)) && (c1.dyear == 2001)) && (c2.dyear == 2002)) && (w1.dyear == 2001)) && (w2.dyear == 2002)) && (s1.year_total > 0)) && (c1.year_total > 0)) && (w1.year_total > 0)) && (((if ((c1.year_total > 0)) (c2.year_total / c1.year_total) else null)) > ((if ((s1.year_total > 0)) (s2.year_total / s1.year_total) else null)))) && (((if ((c1.year_total > 0)) (c2.year_total / c1.year_total) else null)) > ((if ((w1.year_total > 0)) (w2.year_total / w1.year_total) else null))))
}), "sortKey" -> ((args: Seq[Any]) => {
    val s1 = args(0)
    val s2 = args(1)
    val c1 = args(2)
    val c2 = args(3)
    val w1 = args(4)
    val w2 = args(5)
    scala.collection.mutable.ArrayBuffer(s2.customer_id, s2.customer_first_name, s2.customer_last_name, s2.customer_login)
})))
    res
})()
        _json(result)
        test_TPCDS_Q4_empty()
    }
    class _Group(var key: Any) {
            val Items = scala.collection.mutable.ArrayBuffer[Any]()
    }
    
    def expect(cond: Boolean): Unit = {
            if (!cond) throw new RuntimeException("expect failed")
    }
    
    def _group_by(src: Seq[Any], keyfn: Any => Any): Seq[_Group] = {
            val groups = scala.collection.mutable.LinkedHashMap[String,_Group]()
            for (it <- src) {
                    val key = keyfn(it)
                    val ks = key.toString
                    val g = groups.getOrElseUpdate(ks, new _Group(key))
                    g.Items.append(it)
            }
            groups.values.toSeq
    }
    
    def _json(v: Any): Unit = println(_to_json(v))
    
    def _query(src: Seq[Any], joins: Seq[Map[String,Any]], opts: Map[String,Any]): Seq[Any] = {
            var items = src.map(v => Seq[Any](v))
            for (j <- joins) {
                    val joined = scala.collection.mutable.ArrayBuffer[Seq[Any]]()
                    val jitems = j("items").asInstanceOf[Seq[Any]]
                    val on = j.get("on").map(_.asInstanceOf[Seq[Any] => Boolean])
                    val left = j.get("left").exists(_.asInstanceOf[Boolean])
                    val right = j.get("right").exists(_.asInstanceOf[Boolean])
                    if (left && right) {
                            val matched = Array.fill(jitems.length)(false)
                            for (leftRow <- items) {
                                    var m = false
                                    for ((rightRow, ri) <- jitems.zipWithIndex) {
                                            var keep = true
                                            if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                            if (keep) { m = true; matched(ri) = true; joined.append(leftRow :+ rightRow) }
                                    }
                                    if (!m) joined.append(leftRow :+ null)
                            }
                            for ((rightRow, ri) <- jitems.zipWithIndex) {
                                    if (!matched(ri)) {
                                            val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()
                                            joined.append(undef :+ rightRow)
                                    }
                            }
                    } else if (right) {
                            for (rightRow <- jitems) {
                                    var m = false
                                    for (leftRow <- items) {
                                            var keep = true
                                            if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                            if (keep) { m = true; joined.append(leftRow :+ rightRow) }
                                    }
                                    if (!m) {
                                            val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()
                                            joined.append(undef :+ rightRow)
                                    }
                            }
                    } else {
                            for (leftRow <- items) {
                                    var m = false
                                    for (rightRow <- jitems) {
                                            var keep = true
                                            if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                            if (keep) { m = true; joined.append(leftRow :+ rightRow) }
                                    }
                                    if (left && !m) joined.append(leftRow :+ null)
                            }
                    }
                    items = joined.toSeq
            }
            var it = items
            opts.get("where").foreach { f =>
                    val fn = f.asInstanceOf[Seq[Any] => Boolean]
                    it = it.filter(r => fn(r))
            }
            opts.get("sortKey").foreach { f =>
                    val fn = f.asInstanceOf[Seq[Any] => Any]
                    it = it.sortBy(r => fn(r))(_anyOrdering)
            }
            opts.get("skip").foreach { n => it = it.drop(n.asInstanceOf[Int]) }
            opts.get("take").foreach { n => it = it.take(n.asInstanceOf[Int]) }
            val sel = opts("select").asInstanceOf[Seq[Any] => Any]
            it.map(r => sel(r))
    }
    
    def _to_json(v: Any): String = v match {
            case null => "null"
            case s: String => "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
            case b: Boolean => b.toString
            case i: Int => i.toString
            case l: Long => l.toString
            case d: Double => d.toString
            case m: scala.collection.Map[_, _] =>
                    m.map{ case (k, v2) => "\"" + k.toString.replace("\"", "\\\"") + "\":" + _to_json(v2) }.mkString("{", ",", "}")
            case seq: Iterable[_] => seq.map(_to_json).mkString("[", ",", "]")
            case other => "\"" + other.toString.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    }
    
}
