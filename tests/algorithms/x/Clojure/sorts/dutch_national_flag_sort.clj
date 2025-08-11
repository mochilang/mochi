(ns main (:refer-clojure :exclude [dutch_national_flag_sort]))

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
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare dutch_national_flag_sort)

(def ^:dynamic dutch_national_flag_sort_a nil)

(def ^:dynamic dutch_national_flag_sort_high nil)

(def ^:dynamic dutch_national_flag_sort_low nil)

(def ^:dynamic dutch_national_flag_sort_mid nil)

(def ^:dynamic dutch_national_flag_sort_tmp nil)

(def ^:dynamic dutch_national_flag_sort_tmp2 nil)

(def ^:dynamic dutch_national_flag_sort_v nil)

(defn dutch_national_flag_sort [dutch_national_flag_sort_seq]
  (binding [dutch_national_flag_sort_a nil dutch_national_flag_sort_high nil dutch_national_flag_sort_low nil dutch_national_flag_sort_mid nil dutch_national_flag_sort_tmp nil dutch_national_flag_sort_tmp2 nil dutch_national_flag_sort_v nil] (try (do (set! dutch_national_flag_sort_a dutch_national_flag_sort_seq) (set! dutch_national_flag_sort_low 0) (set! dutch_national_flag_sort_mid 0) (set! dutch_national_flag_sort_high (- (count dutch_national_flag_sort_a) 1)) (while (<= dutch_national_flag_sort_mid dutch_national_flag_sort_high) (do (set! dutch_national_flag_sort_v (nth dutch_national_flag_sort_a dutch_national_flag_sort_mid)) (if (= dutch_national_flag_sort_v 0) (do (set! dutch_national_flag_sort_tmp (nth dutch_national_flag_sort_a dutch_national_flag_sort_low)) (set! dutch_national_flag_sort_a (assoc dutch_national_flag_sort_a dutch_national_flag_sort_low dutch_national_flag_sort_v)) (set! dutch_national_flag_sort_a (assoc dutch_national_flag_sort_a dutch_national_flag_sort_mid dutch_national_flag_sort_tmp)) (set! dutch_national_flag_sort_low (+ dutch_national_flag_sort_low 1)) (set! dutch_national_flag_sort_mid (+ dutch_national_flag_sort_mid 1))) (if (= dutch_national_flag_sort_v 1) (set! dutch_national_flag_sort_mid (+ dutch_national_flag_sort_mid 1)) (if (= dutch_national_flag_sort_v 2) (do (set! dutch_national_flag_sort_tmp2 (nth dutch_national_flag_sort_a dutch_national_flag_sort_high)) (set! dutch_national_flag_sort_a (assoc dutch_national_flag_sort_a dutch_national_flag_sort_high dutch_national_flag_sort_v)) (set! dutch_national_flag_sort_a (assoc dutch_national_flag_sort_a dutch_national_flag_sort_mid dutch_national_flag_sort_tmp2)) (set! dutch_national_flag_sort_high (- dutch_national_flag_sort_high 1))) (throw (Exception. "The elements inside the sequence must contains only (0, 1, 2) values"))))))) (throw (ex-info "return" {:v dutch_national_flag_sort_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (dutch_national_flag_sort []))
      (println (dutch_national_flag_sort [0]))
      (println (dutch_national_flag_sort [2 1 0 0 1 2]))
      (println (dutch_national_flag_sort [0 1 1 0 1 2 1 2 0 0 0 1]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
