(ns main (:refer-clojure :exclude [pow2 lshift rshift NewWriter writeBitsLSB writeBitsMSB WriteBits CloseWriter toBinary bytesToBits ExampleWriter_WriteBits]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow2 lshift rshift NewWriter writeBitsLSB writeBitsMSB WriteBits CloseWriter toBinary bytesToBits ExampleWriter_WriteBits)

(declare ExampleWriter_WriteBits_bw bytesToBits_i bytesToBits_out pow2_i pow2_v toBinary_b toBinary_i toBinary_val writeBitsLSB_b writeBitsMSB_b)

(defn pow2 [pow2_n]
  (try (do (def pow2_v 1) (def pow2_i 0) (while (< pow2_i pow2_n) (do (def pow2_v (* pow2_v 2)) (def pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lshift [lshift_x lshift_n]
  (try (throw (ex-info "return" {:v (* lshift_x (pow2 lshift_n))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rshift [rshift_x rshift_n]
  (try (throw (ex-info "return" {:v (/ rshift_x (pow2 rshift_n))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn NewWriter [NewWriter_order]
  (try (throw (ex-info "return" {:v {:order NewWriter_order :bits 0 :nbits 0 :data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn writeBitsLSB [writeBitsLSB_w_p writeBitsLSB_c writeBitsLSB_width]
  (try (do (def writeBitsLSB_w writeBitsLSB_w_p) (def writeBitsLSB_w (assoc writeBitsLSB_w :bits (+ (:bits writeBitsLSB_w) (lshift writeBitsLSB_c (:nbits writeBitsLSB_w))))) (def writeBitsLSB_w (assoc writeBitsLSB_w :nbits (+ (:nbits writeBitsLSB_w) writeBitsLSB_width))) (while (>= (:nbits writeBitsLSB_w) 8) (do (def writeBitsLSB_b (mod (:bits writeBitsLSB_w) 256)) (def writeBitsLSB_w (assoc writeBitsLSB_w :data (conj (:data writeBitsLSB_w) writeBitsLSB_b))) (def writeBitsLSB_w (assoc writeBitsLSB_w :bits (rshift (:bits writeBitsLSB_w) 8))) (def writeBitsLSB_w (assoc writeBitsLSB_w :nbits (- (:nbits writeBitsLSB_w) 8))))) (throw (ex-info "return" {:v writeBitsLSB_w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn writeBitsMSB [writeBitsMSB_w_p writeBitsMSB_c writeBitsMSB_width]
  (try (do (def writeBitsMSB_w writeBitsMSB_w_p) (def writeBitsMSB_w (assoc writeBitsMSB_w :bits (+ (:bits writeBitsMSB_w) (lshift writeBitsMSB_c (- (- 32 writeBitsMSB_width) (:nbits writeBitsMSB_w)))))) (def writeBitsMSB_w (assoc writeBitsMSB_w :nbits (+ (:nbits writeBitsMSB_w) writeBitsMSB_width))) (while (>= (:nbits writeBitsMSB_w) 8) (do (def writeBitsMSB_b (mod (rshift (:bits writeBitsMSB_w) 24) 256)) (def writeBitsMSB_w (assoc writeBitsMSB_w :data (conj (:data writeBitsMSB_w) writeBitsMSB_b))) (def writeBitsMSB_w (assoc writeBitsMSB_w :bits (* (mod (:bits writeBitsMSB_w) (pow2 24)) 256))) (def writeBitsMSB_w (assoc writeBitsMSB_w :nbits (- (:nbits writeBitsMSB_w) 8))))) (throw (ex-info "return" {:v writeBitsMSB_w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn WriteBits [WriteBits_w WriteBits_c WriteBits_width]
  (try (if (= (:order WriteBits_w) "LSB") (writeBitsLSB WriteBits_w WriteBits_c WriteBits_width) (writeBitsMSB WriteBits_w WriteBits_c WriteBits_width)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn CloseWriter [CloseWriter_w_p]
  (try (do (def CloseWriter_w CloseWriter_w_p) (when (> (:nbits CloseWriter_w) 0) (do (when (= (:order CloseWriter_w) "MSB") (def CloseWriter_w (assoc CloseWriter_w :bits (rshift (:bits CloseWriter_w) 24)))) (def CloseWriter_w (assoc CloseWriter_w :data (conj (:data CloseWriter_w) (mod (:bits CloseWriter_w) 256)))))) (def CloseWriter_w (assoc CloseWriter_w :bits 0)) (def CloseWriter_w (assoc CloseWriter_w :nbits 0)) (throw (ex-info "return" {:v CloseWriter_w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toBinary [toBinary_n toBinary_bits]
  (try (do (def toBinary_b "") (def toBinary_val toBinary_n) (def toBinary_i 0) (while (< toBinary_i toBinary_bits) (do (def toBinary_b (str (str (mod toBinary_val 2)) toBinary_b)) (def toBinary_val (quot toBinary_val 2)) (def toBinary_i (+ toBinary_i 1)))) (throw (ex-info "return" {:v toBinary_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bytesToBits [bytesToBits_bs]
  (try (do (def bytesToBits_out "[") (def bytesToBits_i 0) (while (< bytesToBits_i (count bytesToBits_bs)) (do (def bytesToBits_out (str bytesToBits_out (toBinary (nth bytesToBits_bs bytesToBits_i) 8))) (when (< (+ bytesToBits_i 1) (count bytesToBits_bs)) (def bytesToBits_out (str bytesToBits_out " "))) (def bytesToBits_i (+ bytesToBits_i 1)))) (def bytesToBits_out (str bytesToBits_out "]")) (throw (ex-info "return" {:v bytesToBits_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ExampleWriter_WriteBits []
  (do (def ExampleWriter_WriteBits_bw (NewWriter "MSB")) (def ExampleWriter_WriteBits_bw (WriteBits ExampleWriter_WriteBits_bw 15 4)) (def ExampleWriter_WriteBits_bw (WriteBits ExampleWriter_WriteBits_bw 0 1)) (def ExampleWriter_WriteBits_bw (WriteBits ExampleWriter_WriteBits_bw 19 5)) (def ExampleWriter_WriteBits_bw (CloseWriter ExampleWriter_WriteBits_bw)) (println (bytesToBits (:data ExampleWriter_WriteBits_bw)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (ExampleWriter_WriteBits)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
