(ns main (:refer-clojure :exclude [index_of dencrypt main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare index_of dencrypt main)

(def ^:dynamic dencrypt_ch nil)

(def ^:dynamic dencrypt_i nil)

(def ^:dynamic dencrypt_idx_l nil)

(def ^:dynamic dencrypt_idx_u nil)

(def ^:dynamic dencrypt_new_idx nil)

(def ^:dynamic dencrypt_out nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic main_msg nil)

(def ^:dynamic main_s nil)

(def ^:dynamic main_uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_lowercase "abcdefghijklmnopqrstuvwxyz")

(defn index_of [index_of_s index_of_c]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (min (+ index_of_i 1) (count index_of_s))) index_of_c) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dencrypt [dencrypt_s dencrypt_n]
  (binding [dencrypt_ch nil dencrypt_i nil dencrypt_idx_l nil dencrypt_idx_u nil dencrypt_new_idx nil dencrypt_out nil] (try (do (set! dencrypt_out "") (set! dencrypt_i 0) (while (< dencrypt_i (count dencrypt_s)) (do (set! dencrypt_ch (subs dencrypt_s dencrypt_i (min (+ dencrypt_i 1) (count dencrypt_s)))) (set! dencrypt_idx_u (index_of main_uppercase dencrypt_ch)) (if (>= dencrypt_idx_u 0) (do (set! dencrypt_new_idx (mod (+ dencrypt_idx_u dencrypt_n) 26)) (set! dencrypt_out (str dencrypt_out (subs main_uppercase dencrypt_new_idx (min (+ dencrypt_new_idx 1) (count main_uppercase)))))) (do (set! dencrypt_idx_l (index_of main_lowercase dencrypt_ch)) (if (>= dencrypt_idx_l 0) (do (set! dencrypt_new_idx (mod (+ dencrypt_idx_l dencrypt_n) 26)) (set! dencrypt_out (str dencrypt_out (subs main_lowercase dencrypt_new_idx (min (+ dencrypt_new_idx 1) (count main_lowercase)))))) (set! dencrypt_out (str dencrypt_out dencrypt_ch))))) (set! dencrypt_i (+ dencrypt_i 1)))) (throw (ex-info "return" {:v dencrypt_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_msg nil main_s nil] (do (set! main_msg "My secret bank account number is 173-52946 so don't tell anyone!!") (set! main_s (dencrypt main_msg 13)) (println main_s) (println (str (= (dencrypt main_s 13) main_msg))))))

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
