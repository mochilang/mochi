(ns main (:refer-clojure :exclude [split rstripEmpty spaces pad newFormatter printFmt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare split rstripEmpty spaces pad newFormatter printFmt)

(defn split [s sep]
  (try (do (def parts []) (def cur "") (def i 0) (while (< i (count s)) (if (and (and (> (count sep) 0) (<= (+ i (count sep)) (count s))) (= (subs s i (+ i (count sep))) sep)) (do (def parts (conj parts cur)) (def cur "") (def i (+ i (count sep)))) (do (def cur (str cur (subs s i (+ i 1)))) (def i (+ i 1))))) (def parts (conj parts cur)) (throw (ex-info "return" {:v parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rstripEmpty [words]
  (try (do (def n (count words)) (while (and (> n 0) (= (nth words (- n 1)) "")) (def n (- n 1))) (throw (ex-info "return" {:v (subvec words 0 n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn spaces [n]
  (try (do (def out "") (def i 0) (while (< i n) (do (def out (str out " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [word width align]
  (try (do (def diff (- width (count word))) (when (= align 0) (throw (ex-info "return" {:v (str word (spaces diff))}))) (when (= align 2) (throw (ex-info "return" {:v (str (spaces diff) word)}))) (def left (int (/ diff 2))) (def right (- diff left)) (throw (ex-info "return" {:v (str (str (spaces left) word) (spaces right))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newFormatter [text]
  (try (do (def lines (split text "\n")) (def fmtLines []) (def width []) (def i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< i (count lines))) (cond (= (count (nth lines i)) 0) (do (def i (+ i 1)) (recur true)) :else (do (def words (rstripEmpty (split (nth lines i) "$"))) (def fmtLines (conj fmtLines words)) (def j 0) (while (< j (count words)) (do (def wlen (count (nth words j))) (if (= j (count width)) (def width (conj width wlen)) (when (> wlen (nth width j)) (def width (assoc width j wlen)))) (def j (+ j 1)))) (def i (+ i 1)) (recur while_flag_1))))) (throw (ex-info "return" {:v {"text" fmtLines "width" width}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn printFmt [f align]
  (do (def lines (get f "text")) (def width (get f "width")) (def i 0) (while (< i (count lines)) (do (def words (nth lines i)) (def line "") (def j 0) (while (< j (count words)) (do (def line (str (str line (pad (nth words j) (nth width j) align)) " ")) (def j (+ j 1)))) (println line) (def i (+ i 1)))) (println "")))

(def text (str (str (str (str (str "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") "column$are$separated$by$at$least$one$space.\n") "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") "justified,$right$justified,$or$center$justified$within$its$column."))

(def f (newFormatter text))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (printFmt f 0)
      (printFmt f 1)
      (printFmt f 2)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
