(ns main (:refer-clojure :exclude [indexOf rotate scrambleLeft scrambleRight chao main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf rotate scrambleLeft scrambleRight chao main)

(declare chao_ch chao_i chao_idx chao_left chao_out chao_right indexOf_i main_cipher main_plain)

(defn indexOf [indexOf_s indexOf_ch]
  (try (do (def indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i 1)) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rotate [rotate_s rotate_n]
  (try (throw (ex-info "return" {:v (str (subs rotate_s rotate_n (count rotate_s)) (subs rotate_s 0 rotate_n))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn scrambleLeft [scrambleLeft_s]
  (try (throw (ex-info "return" {:v (str (str (str (subs scrambleLeft_s 0 1) (subs scrambleLeft_s 2 14)) (subs scrambleLeft_s 1 2)) (subs scrambleLeft_s 14 (count scrambleLeft_s)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn scrambleRight [scrambleRight_s]
  (try (throw (ex-info "return" {:v (str (str (str (str (subs scrambleRight_s 1 3) (subs scrambleRight_s 4 15)) (subs scrambleRight_s 3 4)) (subs scrambleRight_s 15 (count scrambleRight_s))) (subs scrambleRight_s 0 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chao [chao_text chao_encode]
  (try (do (def chao_left "HXUCZVAMDSLKPEFJRIGTWOBNYQ") (def chao_right "PTLNBQDEOYSFAVZKGJRIHWXUMC") (def chao_out "") (def chao_i 0) (while (< chao_i (count chao_text)) (do (def chao_ch (subs chao_text chao_i (+ chao_i 1))) (def chao_idx 0) (if chao_encode (do (def chao_idx (indexOf chao_right chao_ch)) (def chao_out (+ chao_out (subvec chao_left chao_idx (+ chao_idx 1))))) (do (def chao_idx (indexOf chao_left chao_ch)) (def chao_out (+ chao_out (subvec chao_right chao_idx (+ chao_idx 1)))))) (def chao_left (rotate chao_left chao_idx)) (def chao_right (rotate chao_right chao_idx)) (def chao_left (scrambleLeft chao_left)) (def chao_right (scrambleRight chao_right)) (def chao_i (+ chao_i 1)))) (throw (ex-info "return" {:v chao_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_plain "WELLDONEISBETTERTHANWELLSAID") (def main_cipher (chao main_plain true)) (println main_plain) (println main_cipher) (println (chao main_cipher false))))

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
