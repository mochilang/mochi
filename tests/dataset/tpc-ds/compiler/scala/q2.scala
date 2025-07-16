object Main {
    def test_TPCDS_Q2_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val web_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val catalog_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val wscs: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = (((() => {
    val src = web_sales
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val ws = args(0)
    scala.collection.mutable.Map("sold_date_sk" -> ws.ws_sold_date_sk, "sales_price" -> ws.ws_ext_sales_price, "day" -> ws.ws_sold_date_name)
})))
    res
})()) ++ ((() => {
    val src = catalog_sales
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val cs = args(0)
    scala.collection.mutable.Map("sold_date_sk" -> cs.cs_sold_date_sk, "sales_price" -> cs.cs_ext_sales_price, "day" -> cs.cs_sold_date_name)
})))
    res
})())).distinct
        val wswscs: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = wscs
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val w = args(0)
    val d = args(1)
    (w.sold_date_sk == d.d_date_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val w = args(0)
    w
})))
    res
})(), (w: Any) => scala.collection.mutable.Map("week_seq" -> d.d_week_seq)).map(g => scala.collection.mutable.Map("d_week_seq" -> g.key.week_seq, "sun_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Sunday")
})))
    res
})()), "mon_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Monday")
})))
    res
})()), "tue_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Tuesday")
})))
    res
})()), "wed_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Wednesday")
})))
    res
})()), "thu_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Thursday")
})))
    res
})()), "fri_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Friday")
})))
    res
})()), "sat_sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sales_price
}), "where" -> ((args: Seq[Any]) => {
    val x = args(0)
    (x.day == "Saturday")
})))
    res
})()))).toSeq
        val result: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        _json(result)
        test_TPCDS_Q2_empty()
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
