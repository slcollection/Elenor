(ns automatav.circuitry.imaginative-circuit
  (use [incanter core processing charts]
       [automatav.circuitry.visual-circuit]))


;;;;;; Visualizations/Sketches

;;;; Single slice processing sketch

(defn imagine-slice [slice]
  (sketch
 
    (setup []
      (doto this
        (size 750 750)
        (background 255)))

    (draw []
      (dorun (map (fn f [y row]
                     (dorun (map (fn g [x z]
                                   (doto this
                                     (stroke (* z 10))  ; Amplify contrast in automata img
                                     (point (+ x (* 0.5 y)) y)))
                                 (range 0 Double/POSITIVE_INFINITY) row)))
                  (range 0 Double/POSITIVE_INFINITY) slice)))))


;;;; Single slice delta analysis visualization

(defn imagine-delta-analysis [data]
  (xy-plot (range 0 (count data)) data))

(defn imagine-delta-analysis2 [data]
  (xy-plot (range 0 (count (first data))) (first data)))


;;;;;; Setup

(defn view-slice [sch]
  (view sch :size [1000 750]))


;;;;;; Routines

(defn view-noisy-single-projection []
  (view-slice (imagine-slice (noisy-slice-projection "/home/providence/etc/img_proc/test.jpg"))))

(defn view-delta-analysis []
  (view (imagine-delta-analysis (slice-delta-analysis (noisy-slice-projection "/home/providence/etc/img_proc/test.jpg")))))

(defn view-delta-analysis2 []
  (view (imagine-delta-analysis2 (noisy-slice-projection "/home/providence/etc/img_proc/test.jpg"))))


