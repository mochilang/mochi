object Main {
    def test_TPCDS_Q7_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val store_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val customer_demographics: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val item: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val promotion: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val result: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = store_sales
    val res = _query(src, Seq(
        Map("items" -> customer_demographics, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    (ss.ss_cdemo_sk == cd.cd_demo_sk)
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    val d = args(2)
    (ss.ss_sold_date_sk == d.d_date_sk)
})),
        Map("items" -> item, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    val d = args(2)
    val i = args(3)
    (ss.ss_item_sk == i.i_item_sk)
})),
        Map("items" -> promotion, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    val d = args(2)
    val i = args(3)
    val p = args(4)
    (ss.ss_promo_sk == p.p_promo_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val ss = args(0)
    ss
}), "where" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    val d = args(2)
    val i = args(3)
    val p = args(4)
    (((((cd.cd_gender == "M") && (cd.cd_marital_status == "S")) && (cd.cd_education_status == "College")) && (((p.p_channel_email == "N") || (p.p_channel_event == "N")))) && (d.d_year == 1998))
}), "sortKey" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val cd = args(1)
    val d = args(2)
    val i = args(3)
    val p = args(4)
    g.key.i_item_id
})))
    res
})(), (ss: Any) => scala.collection.mutable.Map("i_item_id" -> i.i_item_id)).map(g => scala.collection.mutable.Map("i_item_id" -> g.key.i_item_id, "agg1" -> ((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_quantity
})))
    res
})().sum / (() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_quantity
})))
    res
})().size), "agg2" -> ((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_list_price
})))
    res
})().sum / (() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_list_price
})))
    res
})().size), "agg3" -> ((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_coupon_amt
})))
    res
})().sum / (() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_coupon_amt
})))
    res
})().size), "agg4" -> ((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_sales_price
})))
    res
})().sum / (() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_sales_price
})))
    res
})().size))).toSeq
        _json(result)
        test_TPCDS_Q7_empty()
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
