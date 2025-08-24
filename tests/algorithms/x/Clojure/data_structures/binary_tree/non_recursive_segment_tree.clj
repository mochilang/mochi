(ns main (:refer-clojure :exclude [build update query add min_int max_int]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare build update query add min_int max_int)

(declare _read_file)

(def ^:dynamic build_i nil)

(def ^:dynamic build_n nil)

(def ^:dynamic build_st nil)

(def ^:dynamic query_has nil)

(def ^:dynamic query_l nil)

(def ^:dynamic query_r nil)

(def ^:dynamic query_res nil)

(def ^:dynamic update_idx nil)

(def ^:dynamic update_st nil)

(defn build [build_arr build_combine]
  (binding [build_i nil build_n nil build_st nil] (try (do (set! build_n (count build_arr)) (set! build_st []) (set! build_i 0) (while (< build_i (* 2 build_n)) (do (set! build_st (conj build_st 0)) (set! build_i (+ build_i 1)))) (set! build_i 0) (while (< build_i build_n) (do (set! build_st (assoc build_st (+ build_n build_i) (nth build_arr build_i))) (set! build_i (+ build_i 1)))) (set! build_i (- build_n 1)) (while (> build_i 0) (do (set! build_st (assoc build_st build_i (build_combine (nth build_st (* build_i 2)) (nth build_st (+ (* build_i 2) 1))))) (set! build_i (- build_i 1)))) (throw (ex-info "return" {:v build_st}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_st_p update_n update_combine update_p update_v]
  (binding [update_st update_st_p update_idx nil] (do (try (do (set! update_idx (+ update_p update_n)) (set! update_st (assoc update_st update_idx update_v)) (while (> update_idx 1) (do (set! update_idx (long (quot update_idx 2))) (set! update_st (assoc update_st update_idx (update_combine (nth update_st (* update_idx 2)) (nth update_st (+ (* update_idx 2) 1)))))))) (finally (alter-var-root (var update_st) (constantly update_st)))) update_st)))

(defn query [query_st query_n query_combine query_left query_right]
  (binding [query_has nil query_l nil query_r nil query_res nil] (try (do (set! query_l (+ query_left query_n)) (set! query_r (+ query_right query_n)) (set! query_res 0) (set! query_has false) (while (<= query_l query_r) (do (when (= (mod query_l 2) 1) (do (if (not query_has) (do (set! query_res (nth query_st query_l)) (set! query_has true)) (set! query_res (query_combine query_res (nth query_st query_l)))) (set! query_l (+ query_l 1)))) (when (= (mod query_r 2) 0) (do (if (not query_has) (do (set! query_res (nth query_st query_r)) (set! query_has true)) (set! query_res (query_combine query_res (nth query_st query_r)))) (set! query_r (- query_r 1)))) (set! query_l (long (quot query_l 2))) (set! query_r (long (quot query_r 2))))) (throw (ex-info "return" {:v query_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add [add_a add_b]
  (try (throw (ex-info "return" {:v (+ add_a add_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) (throw (ex-info "return" {:v min_int_a})) (throw (ex-info "return" {:v min_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max_int [max_int_a max_int_b]
  (try (if (> max_int_a max_int_b) (throw (ex-info "return" {:v max_int_a})) (throw (ex-info "return" {:v max_int_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_arr1 nil)

(def ^:dynamic main_st1 nil)

(def ^:dynamic main_arr2 nil)

(def ^:dynamic main_st2 nil)

(def ^:dynamic main_arr3 nil)

(def ^:dynamic main_st3 nil)

(def ^:dynamic main_arr4 nil)

(def ^:dynamic main_n4 nil)

(def ^:dynamic main_st4 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_arr1) (constantly [1 2 3]))
      (alter-var-root (var main_st1) (constantly (build main_arr1 add)))
      (println (mochi_str (query main_st1 (count main_arr1) add 0 2)))
      (alter-var-root (var main_arr2) (constantly [3 1 2]))
      (alter-var-root (var main_st2) (constantly (build main_arr2 min_int)))
      (println (mochi_str (query main_st2 (count main_arr2) min_int 0 2)))
      (alter-var-root (var main_arr3) (constantly [2 3 1]))
      (alter-var-root (var main_st3) (constantly (build main_arr3 max_int)))
      (println (mochi_str (query main_st3 (count main_arr3) max_int 0 2)))
      (alter-var-root (var main_arr4) (constantly [1 5 7 (- 1) 6]))
      (alter-var-root (var main_n4) (constantly (count main_arr4)))
      (alter-var-root (var main_st4) (constantly (build main_arr4 add)))
      (let [__res (update main_st4 main_n4 add 1 (- 1))] (do (alter-var-root (var main_st4) (constantly update_st)) __res))
      (let [__res (update main_st4 main_n4 add 2 3)] (do (alter-var-root (var main_st4) (constantly update_st)) __res))
      (println (mochi_str (query main_st4 main_n4 add 1 2)))
      (println (mochi_str (query main_st4 main_n4 add 1 1)))
      (let [__res (update main_st4 main_n4 add 4 1)] (do (alter-var-root (var main_st4) (constantly update_st)) __res))
      (println (mochi_str (query main_st4 main_n4 add 3 4)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
