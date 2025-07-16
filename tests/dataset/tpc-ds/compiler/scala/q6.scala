object Main {
    def test_TPCDS_Q6_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val customer_address: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val customer: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val store_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val item: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val target_month_seq = max((() => {
    val src = date_dim
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val d = args(0)
    d.d_month_seq
}), "where" -> ((args: Seq[Any]) => {
    val d = args(0)
    ((d.d_year == 1999) && (d.d_moy == 5))
})))
    res
})())
        val result: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = customer_address
    val res = _query(src, Seq(
        Map("items" -> customer, "on" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    (a.ca_address_sk == c.c_current_addr_sk)
})),
        Map("items" -> store_sales, "on" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    val s = args(2)
    (c.c_customer_sk == s.ss_customer_sk)
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    val s = args(2)
    val d = args(3)
    (s.ss_sold_date_sk == d.d_date_sk)
})),
        Map("items" -> item, "on" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    val s = args(2)
    val d = args(3)
    val i = args(4)
    (s.ss_item_sk == i.i_item_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val a = args(0)
    a
}), "where" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    val s = args(2)
    val d = args(3)
    val i = args(4)
    ((d.d_month_seq == target_month_seq) && (i.i_current_price > (1.2 * ((() => {
        val src = item
        val res = _query(src, Seq(
        ), Map("select" -> ((args: Seq[Any]) => {
        val j = args(0)
        j.i_current_price
    }), "where" -> ((args: Seq[Any]) => {
        val j = args(0)
        (j.i_category == i.i_category)
    })))
        res
    })().sum / (() => {
        val src = item
        val res = _query(src, Seq(
        ), Map("select" -> ((args: Seq[Any]) => {
        val j = args(0)
        j.i_current_price
    }), "where" -> ((args: Seq[Any]) => {
        val j = args(0)
        (j.i_category == i.i_category)
    })))
        res
    })().size))))
}), "sortKey" -> ((args: Seq[Any]) => {
    val a = args(0)
    val c = args(1)
    val s = args(2)
    val d = args(3)
    val i = args(4)
    scala.collection.mutable.ArrayBuffer(g.size, g.key)
}), "take" -> 100))
    res
})(), (a: Any) => a.ca_state).map(g => scala.collection.mutable.Map("state" -> g.key, "cnt" -> g.size)).toSeq
        _json(result)
        test_TPCDS_Q6_empty()
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
