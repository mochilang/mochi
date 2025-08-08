(ns main (:refer-clojure :exclude [ord sdbm]))

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

(declare ord sdbm)

(def ^:dynamic ord_i nil)

(def ^:dynamic sdbm_code nil)

(def ^:dynamic sdbm_hash_value nil)

(def ^:dynamic sdbm_i nil)

(def ^:dynamic main_ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii)) (do (when (= (subs main_ascii ord_i (min (+ ord_i 1) (count main_ascii))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sdbm [sdbm_plain_text]
  (binding [sdbm_code nil sdbm_hash_value nil sdbm_i nil] (try (do (set! sdbm_hash_value 0) (set! sdbm_i 0) (while (< sdbm_i (count sdbm_plain_text)) (do (set! sdbm_code (ord (subs sdbm_plain_text sdbm_i (min (+ sdbm_i 1) (count sdbm_plain_text))))) (set! sdbm_hash_value (+ (* sdbm_hash_value 65599) sdbm_code)) (set! sdbm_i (+ sdbm_i 1)))) (throw (ex-info "return" {:v sdbm_hash_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (sdbm "Algorithms")))
      (println (str (sdbm "scramble bits")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
