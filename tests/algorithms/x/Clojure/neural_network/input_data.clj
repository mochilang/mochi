(ns main (:refer-clojure :exclude [dense_to_one_hot new_dataset next_batch read_data_sets main]))

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

(declare dense_to_one_hot new_dataset next_batch read_data_sets main)

(def ^:dynamic dense_to_one_hot_i nil)

(def ^:dynamic dense_to_one_hot_j nil)

(def ^:dynamic dense_to_one_hot_result nil)

(def ^:dynamic dense_to_one_hot_row nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_ds nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_test_images nil)

(def ^:dynamic main_test_labels_raw nil)

(def ^:dynamic main_train_images nil)

(def ^:dynamic main_train_labels_raw nil)

(def ^:dynamic next_batch_batch_images nil)

(def ^:dynamic next_batch_batch_labels nil)

(def ^:dynamic next_batch_end nil)

(def ^:dynamic next_batch_images_new nil)

(def ^:dynamic next_batch_images_rest nil)

(def ^:dynamic next_batch_labels_new nil)

(def ^:dynamic next_batch_labels_rest nil)

(def ^:dynamic next_batch_new_ds nil)

(def ^:dynamic next_batch_new_index nil)

(def ^:dynamic next_batch_start nil)

(def ^:dynamic read_data_sets_test_labels nil)

(def ^:dynamic read_data_sets_testset nil)

(def ^:dynamic read_data_sets_train nil)

(def ^:dynamic read_data_sets_train_images_rest nil)

(def ^:dynamic read_data_sets_train_labels nil)

(def ^:dynamic read_data_sets_train_labels_rest nil)

(def ^:dynamic read_data_sets_validation nil)

(def ^:dynamic read_data_sets_validation_images nil)

(def ^:dynamic read_data_sets_validation_labels nil)

(def ^:dynamic rest_v nil)

(defn dense_to_one_hot [dense_to_one_hot_labels dense_to_one_hot_num_classes]
  (binding [dense_to_one_hot_i nil dense_to_one_hot_j nil dense_to_one_hot_result nil dense_to_one_hot_row nil] (try (do (set! dense_to_one_hot_result []) (set! dense_to_one_hot_i 0) (while (< dense_to_one_hot_i (count dense_to_one_hot_labels)) (do (set! dense_to_one_hot_row []) (set! dense_to_one_hot_j 0) (while (< dense_to_one_hot_j dense_to_one_hot_num_classes) (do (if (= dense_to_one_hot_j (nth dense_to_one_hot_labels dense_to_one_hot_i)) (set! dense_to_one_hot_row (conj dense_to_one_hot_row 1)) (set! dense_to_one_hot_row (conj dense_to_one_hot_row 0))) (set! dense_to_one_hot_j (+ dense_to_one_hot_j 1)))) (set! dense_to_one_hot_result (conj dense_to_one_hot_result dense_to_one_hot_row)) (set! dense_to_one_hot_i (+ dense_to_one_hot_i 1)))) (throw (ex-info "return" {:v dense_to_one_hot_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_dataset [new_dataset_images new_dataset_labels]
  (try (throw (ex-info "return" {:v {:epochs_completed 0 :images new_dataset_images :index_in_epoch 0 :labels new_dataset_labels :num_examples (count new_dataset_images)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn next_batch [next_batch_ds next_batch_batch_size]
  (binding [next_batch_batch_images nil next_batch_batch_labels nil next_batch_end nil next_batch_images_new nil next_batch_images_rest nil next_batch_labels_new nil next_batch_labels_rest nil next_batch_new_ds nil next_batch_new_index nil next_batch_start nil rest_v nil] (try (do (set! next_batch_start (:index_in_epoch next_batch_ds)) (if (> (+ next_batch_start next_batch_batch_size) (:num_examples next_batch_ds)) (do (set! rest_v (- (:num_examples next_batch_ds) next_batch_start)) (set! next_batch_images_rest (subvec (:images next_batch_ds) next_batch_start (:num_examples next_batch_ds))) (set! next_batch_labels_rest (subvec (:labels next_batch_ds) next_batch_start (:num_examples next_batch_ds))) (set! next_batch_new_index (- next_batch_batch_size rest_v)) (set! next_batch_images_new (subvec (:images next_batch_ds) 0 next_batch_new_index)) (set! next_batch_labels_new (subvec (:labels next_batch_ds) 0 next_batch_new_index)) (set! next_batch_batch_images (concat next_batch_images_rest next_batch_images_new)) (set! next_batch_batch_labels (concat next_batch_labels_rest next_batch_labels_new)) (set! next_batch_new_ds {:epochs_completed (+ (:epochs_completed next_batch_ds) 1) :images (:images next_batch_ds) :index_in_epoch next_batch_new_index :labels (:labels next_batch_ds) :num_examples (:num_examples next_batch_ds)}) (throw (ex-info "return" {:v {:dataset next_batch_new_ds :images next_batch_batch_images :labels next_batch_batch_labels}}))) (do (set! next_batch_end (+ next_batch_start next_batch_batch_size)) (set! next_batch_batch_images (subvec (:images next_batch_ds) next_batch_start next_batch_end)) (set! next_batch_batch_labels (subvec (:labels next_batch_ds) next_batch_start next_batch_end)) (set! next_batch_new_ds {:epochs_completed (:epochs_completed next_batch_ds) :images (:images next_batch_ds) :index_in_epoch next_batch_end :labels (:labels next_batch_ds) :num_examples (:num_examples next_batch_ds)}) (throw (ex-info "return" {:v {:dataset next_batch_new_ds :images next_batch_batch_images :labels next_batch_batch_labels}}))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn read_data_sets [read_data_sets_train_images read_data_sets_train_labels_raw read_data_sets_test_images read_data_sets_test_labels_raw read_data_sets_validation_size read_data_sets_num_classes]
  (binding [read_data_sets_test_labels nil read_data_sets_testset nil read_data_sets_train nil read_data_sets_train_images_rest nil read_data_sets_train_labels nil read_data_sets_train_labels_rest nil read_data_sets_validation nil read_data_sets_validation_images nil read_data_sets_validation_labels nil] (try (do (set! read_data_sets_train_labels (dense_to_one_hot read_data_sets_train_labels_raw read_data_sets_num_classes)) (set! read_data_sets_test_labels (dense_to_one_hot read_data_sets_test_labels_raw read_data_sets_num_classes)) (set! read_data_sets_validation_images (subvec read_data_sets_train_images 0 read_data_sets_validation_size)) (set! read_data_sets_validation_labels (subvec read_data_sets_train_labels 0 read_data_sets_validation_size)) (set! read_data_sets_train_images_rest (subvec read_data_sets_train_images read_data_sets_validation_size (count read_data_sets_train_images))) (set! read_data_sets_train_labels_rest (subvec read_data_sets_train_labels read_data_sets_validation_size (count read_data_sets_train_labels))) (set! read_data_sets_train (new_dataset read_data_sets_train_images_rest read_data_sets_train_labels_rest)) (set! read_data_sets_validation (new_dataset read_data_sets_validation_images read_data_sets_validation_labels)) (set! read_data_sets_testset (new_dataset read_data_sets_test_images read_data_sets_test_labels)) (throw (ex-info "return" {:v {:test_ds read_data_sets_testset :train read_data_sets_train :validation read_data_sets_validation}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_ds nil main_res nil main_test_images nil main_test_labels_raw nil main_train_images nil main_train_labels_raw nil] (do (set! main_train_images [[0 1] [1 2] [2 3] [3 4] [4 5]]) (set! main_train_labels_raw [0 1 2 3 4]) (set! main_test_images [[5 6] [6 7]]) (set! main_test_labels_raw [5 6]) (set! main_data (read_data_sets main_train_images main_train_labels_raw main_test_images main_test_labels_raw 2 10)) (set! main_ds (:train main_data)) (set! main_res (next_batch main_ds 2)) (set! main_ds (:dataset main_res)) (println (str (:images main_res))) (println (str (:labels main_res))) (set! main_res (next_batch main_ds 2)) (set! main_ds (:dataset main_res)) (println (str (:images main_res))) (println (str (:labels main_res))) (set! main_res (next_batch main_ds 2)) (set! main_ds (:dataset main_res)) (println (str (:images main_res))) (println (str (:labels main_res))))))

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
