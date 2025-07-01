object Main {
    def test_TPCDS_Q1_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val store_returns: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val store: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val customer: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val customer_total_return: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = store_returns
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val sr = args(0)
    val d = args(1)
    (sr.sr_returned_date_sk == d.d_date_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val sr = args(0)
    sr
}), "where" -> ((args: Seq[Any]) => {
    val sr = args(0)
    val d = args(1)
    (d.d_year == 1998)
})))
    res
})(), (sr: Any) => scala.collection.mutable.Map("customer_sk" -> sr.sr_customer_sk, "store_sk" -> sr.sr_store_sk)).map(g => scala.collection.mutable.Map("ctr_customer_sk" -> g.key.customer_sk, "ctr_store_sk" -> g.key.store_sk, "ctr_total_return" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sr_return_amt
})))
    res
})()))).toSeq
        val result: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = (() => {
    val src = customer_total_return
    val res = _query(src, Seq(
        Map("items" -> store, "on" -> ((args: Seq[Any]) => {
    val ctr1 = args(0)
    val s = args(1)
    (ctr1.ctr_store_sk == s.s_store_sk)
})),
        Map("items" -> customer, "on" -> ((args: Seq[Any]) => {
    val ctr1 = args(0)
    val s = args(1)
    val c = args(2)
    (ctr1.ctr_customer_sk == c.c_customer_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val ctr1 = args(0)
    val s = args(1)
    val c = args(2)
    scala.collection.mutable.Map("c_customer_id" -> c.c_customer_id)
}), "where" -> ((args: Seq[Any]) => {
    val ctr1 = args(0)
    val s = args(1)
    val c = args(2)
    ((ctr1.ctr_total_return > (((() => {
        val src = customer_total_return
        val res = _query(src, Seq(
        ), Map("select" -> ((args: Seq[Any]) => {
        val ctr2 = args(0)
        ctr2.ctr_total_return
    }), "where" -> ((args: Seq[Any]) => {
        val ctr2 = args(0)
        (ctr1.ctr_store_sk == ctr2.ctr_store_sk)
    })))
        res
    })().sum / (() => {
        val src = customer_total_return
        val res = _query(src, Seq(
        ), Map("select" -> ((args: Seq[Any]) => {
        val ctr2 = args(0)
        ctr2.ctr_total_return
    }), "where" -> ((args: Seq[Any]) => {
        val ctr2 = args(0)
        (ctr1.ctr_store_sk == ctr2.ctr_store_sk)
    })))
        res
    })().size) * 1.2)) && (s.s_state == "TN"))
}), "sortKey" -> ((args: Seq[Any]) => {
    val ctr1 = args(0)
    val s = args(1)
    val c = args(2)
    c.c_customer_id
})))
    res
})()
        _json(result)
        test_TPCDS_Q1_empty()
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
