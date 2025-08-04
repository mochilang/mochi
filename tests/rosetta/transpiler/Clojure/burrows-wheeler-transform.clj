(ns main (:refer-clojure :exclude [contains sortStrings bwt ibwt makePrintable main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare contains sortStrings bwt ibwt makePrintable main)

(declare bwt_i bwt_last bwt_le bwt_rot bwt_table contains_i ibwt_i ibwt_le ibwt_n ibwt_table main_enc main_etx main_examples main_r main_res main_stx makePrintable_ch makePrintable_i makePrintable_out sortStrings_arr sortStrings_i sortStrings_j sortStrings_n sortStrings_tmp)

(def main_stx "\u0002")

(def main_etx "\u0003")

(defn contains [contains_s contains_ch]
  (try (do (def contains_i 0) (while (< contains_i (count contains_s)) (do (when (= (subs contains_s contains_i (+ contains_i 1)) contains_ch) (throw (ex-info "return" {:v true}))) (def contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortStrings [sortStrings_xs]
  (try (do (def sortStrings_arr sortStrings_xs) (def sortStrings_n (count sortStrings_arr)) (def sortStrings_i 0) (while (< sortStrings_i sortStrings_n) (do (def sortStrings_j 0) (while (< sortStrings_j (- sortStrings_n 1)) (do (when (> (compare (nth sortStrings_arr sortStrings_j) (nth sortStrings_arr (+ sortStrings_j 1))) 0) (do (def sortStrings_tmp (nth sortStrings_arr sortStrings_j)) (def sortStrings_arr (assoc sortStrings_arr sortStrings_j (nth sortStrings_arr (+ sortStrings_j 1)))) (def sortStrings_arr (assoc sortStrings_arr (+ sortStrings_j 1) sortStrings_tmp)))) (def sortStrings_j (+ sortStrings_j 1)))) (def sortStrings_i (+ sortStrings_i 1)))) (throw (ex-info "return" {:v sortStrings_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bwt [bwt_s_p]
  (try (do (def bwt_s bwt_s_p) (when (or (contains bwt_s main_stx) (contains bwt_s main_etx)) (throw (ex-info "return" {:v {"err" true "res" ""}}))) (def bwt_s (str (str main_stx bwt_s) main_etx)) (def bwt_le (count bwt_s)) (def bwt_table []) (def bwt_i 0) (while (< bwt_i bwt_le) (do (def bwt_rot (str (subs bwt_s bwt_i bwt_le) (subs bwt_s 0 bwt_i))) (def bwt_table (conj bwt_table bwt_rot)) (def bwt_i (+ bwt_i 1)))) (def bwt_table (sortStrings bwt_table)) (def bwt_last "") (def bwt_i 0) (while (< bwt_i bwt_le) (do (def bwt_last (str bwt_last (subs (nth bwt_table bwt_i) (- bwt_le 1) bwt_le))) (def bwt_i (+ bwt_i 1)))) (throw (ex-info "return" {:v {"err" false "res" bwt_last}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ibwt [ibwt_r]
  (try (do (def ibwt_le (count ibwt_r)) (def ibwt_table []) (def ibwt_i 0) (while (< ibwt_i ibwt_le) (do (def ibwt_table (conj ibwt_table "")) (def ibwt_i (+ ibwt_i 1)))) (def ibwt_n 0) (while (< ibwt_n ibwt_le) (do (def ibwt_i 0) (while (< ibwt_i ibwt_le) (do (def ibwt_table (assoc ibwt_table ibwt_i (str (subs ibwt_r ibwt_i (+ ibwt_i 1)) (nth ibwt_table ibwt_i)))) (def ibwt_i (+ ibwt_i 1)))) (def ibwt_table (sortStrings ibwt_table)) (def ibwt_n (+ ibwt_n 1)))) (def ibwt_i 0) (while (< ibwt_i ibwt_le) (do (when (= (subs (nth ibwt_table ibwt_i) (- ibwt_le 1) ibwt_le) main_etx) (throw (ex-info "return" {:v (subs (nth ibwt_table ibwt_i) 1 (- ibwt_le 1))}))) (def ibwt_i (+ ibwt_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn makePrintable [makePrintable_s]
  (try (do (def makePrintable_out "") (def makePrintable_i 0) (while (< makePrintable_i (count makePrintable_s)) (do (def makePrintable_ch (subs makePrintable_s makePrintable_i (+ makePrintable_i 1))) (if (= makePrintable_ch main_stx) (def makePrintable_out (str makePrintable_out "^")) (if (= makePrintable_ch main_etx) (def makePrintable_out (str makePrintable_out "|")) (def makePrintable_out (str makePrintable_out makePrintable_ch)))) (def makePrintable_i (+ makePrintable_i 1)))) (throw (ex-info "return" {:v makePrintable_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_examples ["banana" "appellee" "dogwood" "TO BE OR NOT TO BE OR WANT TO BE OR NOT?" "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES" "\u0002ABC\u0003"]) (doseq [t main_examples] (do (println (makePrintable t)) (def main_res (bwt t)) (if (get main_res "err") (do (println " --> ERROR: String can't contain STX or ETX") (println " -->")) (do (def main_enc (str (get main_res "res"))) (println (str " --> " (makePrintable main_enc))) (def main_r (ibwt main_enc)) (println (str " --> " main_r)))) (println "")))))

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
