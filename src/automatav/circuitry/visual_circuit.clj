(ns automatav.circuitry.visual-circuit
  (:use [automatav.circuitry.subtractive-circuit :only [subtractive-circuit]]
        [automatav.circuitry.lstm-circuit :only [lstm-circuit blank-lstm]]
        [incanter core])
  (:import javax.imageio.ImageIO java.awt.image.BufferedImage java.io.File)
  (:require clojure.contrib.math))


;;;;;; LSTM Matrix

;;;; Mutable reference to LSTM matrix

(def lstm-matrix
  (ref {}))


;;;; Create LSTM Matrix

(defn create-lstm-matrix [width]
  (dorun
    (map (fn f [x]
           (dosync 
             (alter lstm-matrix 
               assoc x 
                 (reduce #(assoc %1 %2 (blank-lstm)) {} (range 0 (- width 1))))))
         (range 0 (- width 1)))))


;;;;;; Junctioning between Subtractve & LSTM Matricies

;;;; Junction refocusing

;; Individual-level refocusing & application

(defn simulate-junctioning [lvl column in1 in2]
  (let [output     (subtractive-circuit [in1 in2])
        lstm-row   (get @lstm-matrix lvl)
        indiv-lstm (get lstm-row column)
        mem-state  (indiv-lstm [output 1 0 0])]
  (dosync 
    (alter lstm-matrix 
      assoc lvl (assoc lstm-row column mem-state)))
  output))


;; Row-level refocusing (Caller must trampoline this fn)

(defn apply-junction [inputs lvl]
  (let [next-inputs  (doall (next inputs))
        next-outputs (doall (map (partial simulate-junctioning lvl) 
                            (range 0 Double/POSITIVE_INFINITY) inputs next-inputs))]
  (if next-inputs
    (recur next-outputs (+ 1 lvl)))))


;;;;;; LSTM value retrieval

(defn accumulate-row-outputs [row]
  (let [sorted-keys (sort (keys row))]
  (map #((get row %) [0 0 0 1]) sorted-keys)))

(defn accumulate-lstm-outputs []
  (let [sorted-keys (sort (keys @lstm-matrix))]
  (map #(accumulate-row-outputs (get @lstm-matrix %)) sorted-keys)))
 

;;;;;; Initial visual processing

;;;; Image loading

(defn load-image-array [path]
  (ImageIO/read (File. path)))


;;;; Image slicing

(defn slice-image [direction rc-num image]
  (when (= direction :row)
    (map #(.getRGB image % rc-num) (range 0 (.getWidth image)))))


;;;; RGB normalization

(defn std-normalize [pixel]
  (let [b1 (bit-and pixel 0xFF)
        b2 (bit-and (bit-shift-right pixel 16) 0xFF)
        b3 (bit-and (bit-shift-right pixel 8) 0xFF)]
  (reduce #(Math/abs (+ %1 %2)) 0 [b1 b2 b3])))

(defn std-slice-normalize [slice]
  (map std-normalize slice))


;;;;;; Image processing

;;;; Noisy single slice projection 

(defn noisy-slice-projection [path]
  (let [image      (load-image-array path)
        img-slice  (slice-image :row 250 image)
        nrml-slice (std-slice-normalize img-slice)]
  (create-lstm-matrix (.getWidth image))
  (apply-junction (doall nrml-slice) 0)
  (accumulate-lstm-outputs)))


;;;; delta 'topography' of a single slice

(defn get-row-deltas [row]
  (/ (count (filter #(> % 0) row)) (count row)))

(defn slice-delta-analysis [slice]
  (map get-row-deltas slice))


