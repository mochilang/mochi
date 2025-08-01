(ns main (:refer-clojure :exclude [primeFactors commatize indexOf pad10 trimRightStr main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare primeFactors commatize indexOf pad10 trimRightStr main)

(declare commatize_c commatize_i commatize_out commatize_s count_v indexOf_i main_factors main_fs main_includesAll main_k main_line main_prev main_res main_s pad10_str primeFactors_factors primeFactors_p primeFactors_x trimRightStr_end)

(defn primeFactors [primeFactors_n]
  (try (do (def primeFactors_factors []) (def primeFactors_x primeFactors_n) (while (= (mod primeFactors_x 2) 0) (do (def primeFactors_factors (conj primeFactors_factors 2)) (def primeFactors_x (int (/ primeFactors_x 2))))) (def primeFactors_p 3) (while (<= (* primeFactors_p primeFactors_p) primeFactors_x) (do (while (= (mod primeFactors_x primeFactors_p) 0) (do (def primeFactors_factors (conj primeFactors_factors primeFactors_p)) (def primeFactors_x (int (/ primeFactors_x primeFactors_p))))) (def primeFactors_p (+ primeFactors_p 2)))) (when (> primeFactors_x 1) (def primeFactors_factors (conj primeFactors_factors primeFactors_x))) (throw (ex-info "return" {:v primeFactors_factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn commatize [commatize_n]
  (try (do (def commatize_s (str commatize_n)) (def commatize_out "") (def commatize_i (- (count commatize_s) 1)) (def commatize_c 0) (while (>= commatize_i 0) (do (def commatize_out (str (subs commatize_s commatize_i (+ commatize_i 1)) commatize_out)) (def commatize_c (+ commatize_c 1)) (when (and (= (mod commatize_c 3) 0) (> commatize_i 0)) (def commatize_out (str "," commatize_out))) (def commatize_i (- commatize_i 1)))) (throw (ex-info "return" {:v commatize_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [indexOf_s indexOf_sub]
  (try (do (def indexOf_i 0) (while (<= (+ indexOf_i (count indexOf_sub)) (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i (count indexOf_sub))) indexOf_sub) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad10 [pad10_s]
  (try (do (def pad10_str pad10_s) (while (< (count pad10_str) 10) (def pad10_str (str " " pad10_str))) (throw (ex-info "return" {:v pad10_str}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn trimRightStr [trimRightStr_s]
  (try (do (def trimRightStr_end (count trimRightStr_s)) (while (and (> trimRightStr_end 0) (= (subs trimRightStr_s (- trimRightStr_end 1) trimRightStr_end) " ")) (def trimRightStr_end (- trimRightStr_end 1))) (throw (ex-info "return" {:v (subs trimRightStr_s 0 trimRightStr_end)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_res []) (def count_v 0) (def main_k (* 11 11)) (loop [while_flag_1 true] (when (and while_flag_1 (< count_v 20)) (cond (or (or (= (mod main_k 3) 0) (= (mod main_k 5) 0)) (= (mod main_k 7) 0)) (do (def main_k (+ main_k 2)) (recur true)) :else (do (def main_factors (primeFactors main_k)) (when (> (count main_factors) 1) (do (def main_s (str main_k)) (def main_includesAll true) (def main_prev (- 1)) (loop [f_seq main_factors] (when (seq f_seq) (let [f (first f_seq)] (cond (= f main_prev) (recur (rest f_seq)) (= (indexOf main_s main_fs) (- 1)) (do (def main_includesAll false) (recur nil)) :else (do (def main_fs (str f)) (def main_prev f) (recur (rest f_seq))))))) (when main_includesAll (do (def main_res (conj main_res main_k)) (def count_v (+ count_v 1)))))) (def main_k (+ main_k 2)) (recur while_flag_1))))) (def main_line "") (doseq [e (subvec main_res 0 10)] (def main_line (str (str main_line (pad10 (commatize e))) " "))) (println (trimRightStr main_line)) (def main_line "") (doseq [e (subvec main_res 10 20)] (def main_line (str (str main_line (pad10 (commatize e))) " "))) (println (trimRightStr main_line))))

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
