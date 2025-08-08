(ns main (:refer-clojure :exclude [index_of remove_item stable_matching]))

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

(declare index_of remove_item stable_matching)

(def ^:dynamic index_of_i nil)

(def ^:dynamic remove_item_i nil)

(def ^:dynamic remove_item_removed nil)

(def ^:dynamic remove_item_res nil)

(def ^:dynamic stable_matching_donor nil)

(def ^:dynamic stable_matching_donor_preference nil)

(def ^:dynamic stable_matching_donor_record nil)

(def ^:dynamic stable_matching_i nil)

(def ^:dynamic stable_matching_n nil)

(def ^:dynamic stable_matching_new_index nil)

(def ^:dynamic stable_matching_num_donations nil)

(def ^:dynamic stable_matching_prev_donor nil)

(def ^:dynamic stable_matching_prev_index nil)

(def ^:dynamic stable_matching_rec_preference nil)

(def ^:dynamic stable_matching_rec_record nil)

(def ^:dynamic stable_matching_recipient nil)

(def ^:dynamic stable_matching_unmatched nil)

(defn index_of [index_of_xs index_of_x]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_x) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_item [remove_item_xs remove_item_x]
  (binding [remove_item_i nil remove_item_removed nil remove_item_res nil] (try (do (set! remove_item_res []) (set! remove_item_removed false) (set! remove_item_i 0) (while (< remove_item_i (count remove_item_xs)) (do (if (and (not remove_item_removed) (= (nth remove_item_xs remove_item_i) remove_item_x)) (set! remove_item_removed true) (set! remove_item_res (conj remove_item_res (nth remove_item_xs remove_item_i)))) (set! remove_item_i (+ remove_item_i 1)))) (throw (ex-info "return" {:v remove_item_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stable_matching [stable_matching_donor_pref stable_matching_recipient_pref]
  (binding [stable_matching_donor nil stable_matching_donor_preference nil stable_matching_donor_record nil stable_matching_i nil stable_matching_n nil stable_matching_new_index nil stable_matching_num_donations nil stable_matching_prev_donor nil stable_matching_prev_index nil stable_matching_rec_preference nil stable_matching_rec_record nil stable_matching_recipient nil stable_matching_unmatched nil] (try (do (when (not= (count stable_matching_donor_pref) (count stable_matching_recipient_pref)) (throw (Exception. "unequal groups"))) (set! stable_matching_n (count stable_matching_donor_pref)) (set! stable_matching_unmatched []) (set! stable_matching_i 0) (while (< stable_matching_i stable_matching_n) (do (set! stable_matching_unmatched (conj stable_matching_unmatched stable_matching_i)) (set! stable_matching_i (+ stable_matching_i 1)))) (set! stable_matching_donor_record []) (set! stable_matching_i 0) (while (< stable_matching_i stable_matching_n) (do (set! stable_matching_donor_record (conj stable_matching_donor_record (- 1))) (set! stable_matching_i (+ stable_matching_i 1)))) (set! stable_matching_rec_record []) (set! stable_matching_i 0) (while (< stable_matching_i stable_matching_n) (do (set! stable_matching_rec_record (conj stable_matching_rec_record (- 1))) (set! stable_matching_i (+ stable_matching_i 1)))) (set! stable_matching_num_donations []) (set! stable_matching_i 0) (while (< stable_matching_i stable_matching_n) (do (set! stable_matching_num_donations (conj stable_matching_num_donations 0)) (set! stable_matching_i (+ stable_matching_i 1)))) (while (> (count stable_matching_unmatched) 0) (do (set! stable_matching_donor (nth stable_matching_unmatched 0)) (set! stable_matching_donor_preference (nth stable_matching_donor_pref stable_matching_donor)) (set! stable_matching_recipient (nth stable_matching_donor_preference (nth stable_matching_num_donations stable_matching_donor))) (set! stable_matching_num_donations (assoc stable_matching_num_donations stable_matching_donor (+ (nth stable_matching_num_donations stable_matching_donor) 1))) (set! stable_matching_rec_preference (nth stable_matching_recipient_pref stable_matching_recipient)) (set! stable_matching_prev_donor (nth stable_matching_rec_record stable_matching_recipient)) (if (not= stable_matching_prev_donor (- 0 1)) (do (set! stable_matching_prev_index (index_of stable_matching_rec_preference stable_matching_prev_donor)) (set! stable_matching_new_index (index_of stable_matching_rec_preference stable_matching_donor)) (when (> stable_matching_prev_index stable_matching_new_index) (do (set! stable_matching_rec_record (assoc stable_matching_rec_record stable_matching_recipient stable_matching_donor)) (set! stable_matching_donor_record (assoc stable_matching_donor_record stable_matching_donor stable_matching_recipient)) (set! stable_matching_unmatched (conj stable_matching_unmatched stable_matching_prev_donor)) (set! stable_matching_unmatched (remove_item stable_matching_unmatched stable_matching_donor))))) (do (set! stable_matching_rec_record (assoc stable_matching_rec_record stable_matching_recipient stable_matching_donor)) (set! stable_matching_donor_record (assoc stable_matching_donor_record stable_matching_donor stable_matching_recipient)) (set! stable_matching_unmatched (remove_item stable_matching_unmatched stable_matching_donor)))))) (throw (ex-info "return" {:v stable_matching_donor_record}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_donor_pref [[0 1 3 2] [0 2 3 1] [1 0 2 3] [0 3 1 2]])

(def ^:dynamic main_recipient_pref [[3 1 2 0] [3 1 0 2] [0 3 1 2] [1 0 3 2]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (stable_matching main_donor_pref main_recipient_pref)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
