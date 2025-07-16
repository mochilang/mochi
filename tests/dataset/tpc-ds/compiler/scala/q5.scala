object Main {
    def test_TPCDS_Q5_empty(): Unit = {
        expect((result.length == 0))
    }
    
    def main(args: Array[String]): Unit = {
        val store_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val store_returns: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val store: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val catalog_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val catalog_returns: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val catalog_page: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val web_sales: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val web_returns: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val web_site: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val date_dim: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val ss: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = store_sales
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val d = args(1)
    (ss.ss_sold_date_sk == d.d_date_sk)
})),
        Map("items" -> store, "on" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val d = args(1)
    val s = args(2)
    (ss.ss_store_sk == s.s_store_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val ss = args(0)
    ss
}), "where" -> ((args: Seq[Any]) => {
    val ss = args(0)
    val d = args(1)
    val s = args(2)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (ss: Any) => s.s_store_id).map(g => scala.collection.mutable.Map("channel" -> "store channel", "id" -> ("store" + g.key.toString()), "sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_ext_sales_price
})))
    res
})()), "returns" -> 0, "profit" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ss.ss_net_profit
})))
    res
})()), "profit_loss" -> 0)).toSeq
        val sr: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = store_returns
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val sr = args(0)
    val d = args(1)
    (sr.sr_returned_date_sk == d.d_date_sk)
})),
        Map("items" -> store, "on" -> ((args: Seq[Any]) => {
    val sr = args(0)
    val d = args(1)
    val s = args(2)
    (sr.sr_store_sk == s.s_store_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val sr = args(0)
    sr
}), "where" -> ((args: Seq[Any]) => {
    val sr = args(0)
    val d = args(1)
    val s = args(2)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (sr: Any) => s.s_store_id).map(g => scala.collection.mutable.Map("channel" -> "store channel", "id" -> ("store" + g.key.toString()), "sales" -> 0, "returns" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sr.sr_return_amt
})))
    res
})()), "profit" -> 0, "profit_loss" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.sr.sr_net_loss
})))
    res
})()))).toSeq
        val cs: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = catalog_sales
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val cs = args(0)
    val d = args(1)
    (cs.cs_sold_date_sk == d.d_date_sk)
})),
        Map("items" -> catalog_page, "on" -> ((args: Seq[Any]) => {
    val cs = args(0)
    val d = args(1)
    val cp = args(2)
    (cs.cs_catalog_page_sk == cp.cp_catalog_page_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val cs = args(0)
    cs
}), "where" -> ((args: Seq[Any]) => {
    val cs = args(0)
    val d = args(1)
    val cp = args(2)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (cs: Any) => cp.cp_catalog_page_id).map(g => scala.collection.mutable.Map("channel" -> "catalog channel", "id" -> ("catalog_page" + g.key.toString()), "sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.cs.cs_ext_sales_price
})))
    res
})()), "returns" -> 0, "profit" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.cs.cs_net_profit
})))
    res
})()), "profit_loss" -> 0)).toSeq
        val cr: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = catalog_returns
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val cr = args(0)
    val d = args(1)
    (cr.cr_returned_date_sk == d.d_date_sk)
})),
        Map("items" -> catalog_page, "on" -> ((args: Seq[Any]) => {
    val cr = args(0)
    val d = args(1)
    val cp = args(2)
    (cr.cr_catalog_page_sk == cp.cp_catalog_page_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val cr = args(0)
    cr
}), "where" -> ((args: Seq[Any]) => {
    val cr = args(0)
    val d = args(1)
    val cp = args(2)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (cr: Any) => cp.cp_catalog_page_id).map(g => scala.collection.mutable.Map("channel" -> "catalog channel", "id" -> ("catalog_page" + g.key.toString()), "sales" -> 0, "returns" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.cr.cr_return_amount
})))
    res
})()), "profit" -> 0, "profit_loss" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.cr.cr_net_loss
})))
    res
})()))).toSeq
        val ws: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = web_sales
    val res = _query(src, Seq(
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val ws = args(0)
    val d = args(1)
    (ws.ws_sold_date_sk == d.d_date_sk)
})),
        Map("items" -> web_site, "on" -> ((args: Seq[Any]) => {
    val ws = args(0)
    val d = args(1)
    val w = args(2)
    (ws.ws_web_site_sk == w.web_site_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val ws = args(0)
    ws
}), "where" -> ((args: Seq[Any]) => {
    val ws = args(0)
    val d = args(1)
    val w = args(2)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (ws: Any) => w.web_site_id).map(g => scala.collection.mutable.Map("channel" -> "web channel", "id" -> ("web_site" + g.key.toString()), "sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ws.ws_ext_sales_price
})))
    res
})()), "returns" -> 0, "profit" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.ws.ws_net_profit
})))
    res
})()), "profit_loss" -> 0)).toSeq
        val wr: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = web_returns
    val res = _query(src, Seq(
        Map("items" -> web_sales, "on" -> ((args: Seq[Any]) => {
    val wr = args(0)
    val ws = args(1)
    ((wr.wr_item_sk == ws.ws_item_sk) && (wr.wr_order_number == ws.ws_order_number))
})),
        Map("items" -> date_dim, "on" -> ((args: Seq[Any]) => {
    val wr = args(0)
    val ws = args(1)
    val d = args(2)
    (wr.wr_returned_date_sk == d.d_date_sk)
})),
        Map("items" -> web_site, "on" -> ((args: Seq[Any]) => {
    val wr = args(0)
    val ws = args(1)
    val d = args(2)
    val w = args(3)
    (ws.ws_web_site_sk == w.web_site_sk)
}))
    ), Map("select" -> ((args: Seq[Any]) => {
    val wr = args(0)
    wr
}), "where" -> ((args: Seq[Any]) => {
    val wr = args(0)
    val ws = args(1)
    val d = args(2)
    val w = args(3)
    ((d.d_date >= "1998-12-01") && (d.d_date <= "1998-12-15"))
})))
    res
})(), (wr: Any) => w.web_site_id).map(g => scala.collection.mutable.Map("channel" -> "web channel", "id" -> ("web_site" + g.key.toString()), "sales" -> 0, "returns" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.wr.wr_return_amt
})))
    res
})()), "profit" -> 0, "profit_loss" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.wr.wr_net_loss
})))
    res
})()))).toSeq
        val per_channel: scala.collection.mutable.ArrayBuffer[Any] = concat((ss ++ sr).distinct, (cs ++ cr).distinct, (ws ++ wr).distinct)
        val result: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Map[String, Any]] = _group_by((() => {
    val src = per_channel
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val p = args(0)
    p
}), "sortKey" -> ((args: Seq[Any]) => {
    val p = args(0)
    g.key.channel
})))
    res
})(), (p: Any) => scala.collection.mutable.Map("channel" -> p.channel, "id" -> p.id)).map(g => scala.collection.mutable.Map("channel" -> g.key.channel, "id" -> g.key.id, "sales" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.p.sales
})))
    res
})()), "returns" -> sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.p.returns
})))
    res
})()), "profit" -> (sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.p.profit
})))
    res
})()) - sum((() => {
    val src = g
    val res = _query(src, Seq(
    ), Map("select" -> ((args: Seq[Any]) => {
    val x = args(0)
    x.p.profit_loss
})))
    res
})())))).toSeq
        _json(result)
        test_TPCDS_Q5_empty()
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
