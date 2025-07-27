(ns main (:refer-clojure :exclude [Node getLink setLink opp single double adjustBalance insertBalance insertR Insert removeBalance removeR Remove indentStr dumpNode dump main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare Node getLink setLink opp single double adjustBalance insertBalance insertR Insert removeBalance removeR Remove indentStr dumpNode dump main)

(defn Node [data]
  (try (throw (ex-info "return" {:v {"Data" data "Balance" 0 "Link" [nil nil]}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn getLink [n dir]
  (try (throw (ex-info "return" {:v (nth (get n "Link") dir)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn setLink [n dir v]
  (do (def links (get n "Link")) (def links (assoc links dir v)) (def n (assoc n "Link" links))))

(defn opp [dir]
  (try (throw (ex-info "return" {:v (- 1 dir)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn single [root dir]
  (try (do (def tmp (getLink root (opp dir))) (setLink root (opp dir) (getLink tmp dir)) (setLink tmp dir root) (throw (ex-info "return" {:v tmp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn double [root dir]
  (try (do (def tmp (getLink (getLink root (opp dir)) dir)) (setLink (getLink root (opp dir)) dir (getLink tmp (opp dir))) (setLink tmp (opp dir) (getLink root (opp dir))) (setLink root (opp dir) tmp) (def tmp (getLink root (opp dir))) (setLink root (opp dir) (getLink tmp dir)) (setLink tmp dir root) (throw (ex-info "return" {:v tmp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn adjustBalance [root dir bal]
  (do (def n (getLink root dir)) (def nn (getLink n (opp dir))) (if (= (get nn "Balance") 0) (do (def root (assoc root "Balance" 0)) (def n (assoc n "Balance" 0))) (if (= (get nn "Balance") bal) (do (def root (assoc root "Balance" (- bal))) (def n (assoc n "Balance" 0))) (do (def root (assoc root "Balance" 0)) (def n (assoc n "Balance" bal))))) (def nn (assoc nn "Balance" 0))))

(defn insertBalance [root dir]
  (try (do (def n (getLink root dir)) (def bal (- (* 2 dir) 1)) (when (= (get n "Balance") bal) (do (def root (assoc root "Balance" 0)) (def n (assoc n "Balance" 0)) (throw (ex-info "return" {:v (single root (opp dir))})))) (adjustBalance root dir bal) (throw (ex-info "return" {:v (double root (opp dir))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn insertR [root data]
  (try (do (when (= root nil) (throw (ex-info "return" {:v {"node" (Node data) "done" false}}))) (def node root) (def dir 0) (when (< (int (get node "Data")) data) (def dir 1)) (def r (insertR (getLink node dir) data)) (setLink node dir (get r "node")) (when (get r "done") (throw (ex-info "return" {:v {"node" node "done" true}}))) (def node (assoc node "Balance" (+ (int (get node "Balance")) (- (* 2 dir) 1)))) (when (= (get node "Balance") 0) (throw (ex-info "return" {:v {"node" node "done" true}}))) (if (or (= (get node "Balance") 1) (= (get node "Balance") (- 1))) {"node" node "done" false} {"node" (insertBalance node dir) "done" true})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn Insert [tree data]
  (try (do (def r (insertR tree data)) (throw (ex-info "return" {:v (get r "node")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn removeBalance [root dir]
  (try (do (def n (getLink root (opp dir))) (def bal (- (* 2 dir) 1)) (when (= (get n "Balance") (- bal)) (do (def root (assoc root "Balance" 0)) (def n (assoc n "Balance" 0)) (throw (ex-info "return" {:v {"node" (single root dir) "done" false}})))) (when (= (get n "Balance") bal) (do (adjustBalance root (opp dir) (- bal)) (throw (ex-info "return" {:v {"node" (double root dir) "done" false}})))) (def root (assoc root "Balance" (- bal))) (def n (assoc n "Balance" bal)) (throw (ex-info "return" {:v {"node" (single root dir) "done" true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn removeR [root data]
  (try (do (when (= root nil) (throw (ex-info "return" {:v {"node" nil "done" false}}))) (def node root) (when (= (int (get node "Data")) data) (do (when (= (getLink node 0) nil) (throw (ex-info "return" {:v {"node" (getLink node 1) "done" false}}))) (when (= (getLink node 1) nil) (throw (ex-info "return" {:v {"node" (getLink node 0) "done" false}}))) (def heir (getLink node 0)) (while (not= (getLink heir 1) nil) (def heir (getLink heir 1))) (def node (assoc node "Data" (get heir "Data"))) (def data (int (get heir "Data"))))) (def dir 0) (when (< (int (get node "Data")) data) (def dir 1)) (def r (removeR (getLink node dir) data)) (setLink node dir (get r "node")) (when (get r "done") (throw (ex-info "return" {:v {"node" node "done" true}}))) (def node (assoc node "Balance" (- (+ (int (get node "Balance")) 1) (* 2 dir)))) (when (or (= (get node "Balance") 1) (= (get node "Balance") (- 1))) (throw (ex-info "return" {:v {"node" node "done" true}}))) (if (= (get node "Balance") 0) {"node" node "done" false} (removeBalance node dir))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn Remove [tree data]
  (try (do (def r (removeR tree data)) (throw (ex-info "return" {:v (get r "node")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indentStr [n]
  (try (do (def s "") (def i 0) (while (< i n) (do (def s (str s " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dumpNode [node indent comma]
  (do (def sp (indentStr indent)) (if (= node nil) (do (def line (str sp "null")) (when comma (def line (str line ","))) (println line)) (do (println (str sp "{")) (println (str (str (str (indentStr (+ indent 3)) "\"Data\": ") (str (get node "Data"))) ",")) (println (str (str (str (indentStr (+ indent 3)) "\"Balance\": ") (str (get node "Balance"))) ",")) (println (str (indentStr (+ indent 3)) "\"Link\": [")) (dumpNode (getLink node 0) (+ indent 6) true) (dumpNode (getLink node 1) (+ indent 6) false) (println (str (indentStr (+ indent 3)) "]")) (def end (str sp "}")) (when comma (def end (str end ","))) (println end)))))

(defn dump [node indent]
  (dumpNode node indent false))

(defn main []
  (do (def tree nil) (println "Empty tree:") (dump tree 0) (println "") (println "Insert test:") (def tree (Insert tree 3)) (def tree (Insert tree 1)) (def tree (Insert tree 4)) (def tree (Insert tree 1)) (def tree (Insert tree 5)) (dump tree 0) (println "") (println "Remove test:") (def tree (Remove tree 3)) (def tree (Remove tree 1)) (def t tree) (def t (assoc t "Balance" 0)) (def tree t) (dump tree 0)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
