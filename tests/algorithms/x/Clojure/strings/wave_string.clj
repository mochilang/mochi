(ns main (:refer-clojure :exclude [index_of is_alpha to_upper wave]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare index_of is_alpha to_upper wave)

(def ^:dynamic index_of_i nil)

(def ^:dynamic to_upper_idx nil)

(def ^:dynamic wave_ch nil)

(def ^:dynamic wave_i nil)

(def ^:dynamic wave_prefix nil)

(def ^:dynamic wave_result nil)

(def ^:dynamic wave_suffix nil)

(def ^:dynamic main_lowercase "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_s index_of_c]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (min (+ index_of_i 1) (count index_of_s))) index_of_c) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_alpha [is_alpha_c]
  (try (throw (ex-info "return" {:v (or (>= (index_of main_lowercase is_alpha_c) 0) (>= (index_of main_uppercase is_alpha_c) 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_upper [to_upper_c]
  (binding [to_upper_idx nil] (try (do (set! to_upper_idx (index_of main_lowercase to_upper_c)) (if (>= to_upper_idx 0) (subs main_uppercase to_upper_idx (min (+ to_upper_idx 1) (count main_uppercase))) to_upper_c)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn wave [wave_txt]
  (binding [wave_ch nil wave_i nil wave_prefix nil wave_result nil wave_suffix nil] (try (do (set! wave_result []) (set! wave_i 0) (while (< wave_i (count wave_txt)) (do (set! wave_ch (subs wave_txt wave_i (min (+ wave_i 1) (count wave_txt)))) (when (is_alpha wave_ch) (do (set! wave_prefix (subs wave_txt 0 (min wave_i (count wave_txt)))) (set! wave_suffix (subs wave_txt (+ wave_i 1) (min (count wave_txt) (count wave_txt)))) (set! wave_result (conj wave_result (str (str wave_prefix (to_upper wave_ch)) wave_suffix))))) (set! wave_i (+ wave_i 1)))) (throw (ex-info "return" {:v wave_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (wave "cat")))
      (println (str (wave "one")))
      (println (str (wave "book")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
