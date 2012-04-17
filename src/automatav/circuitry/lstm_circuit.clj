(ns automatav.circuitry.lstm-circuit)


;;;;;; LSTM circuit

;;;; Internal operator fns

;; First row

(defn row1-operator [input]
  input)


;; Second row

(defn n5-operator [inputs]
  (if (== (second inputs) 1)
    (first inputs)
    -1))

(defn n6-operator [inputs]
  (if (== (first inputs) 1)
    0
    (second inputs)))


;; Third row

(defn n7-operator [inputs]
  (if (> (first inputs) -1)
    (first inputs)
    (second inputs)))


;; Fourth row

(defn n8-operator [inputs]
  (if (> (second inputs) 0)
    (first inputs)
    -1))


;;;; Mapping schemes

;; First row

(defn n1-mapping-scheme [output]
  {:5 output})

(defn n2-mapping-scheme [output]
  {:5 output})

(defn n3-mapping-scheme [output]
  {:6 output})

(defn n4-mapping-scheme [output]
  {:8 output})


;; Second row

(defn n5-mapping-scheme [output]
  {:7 output})

(defn n6-mapping-scheme [output]
  {:7 output})


;; Third row

(defn n7-mapping-scheme [output]
  {:6 output :8 output})


;;;; Neuron definitions

(defn neuron1 [input]
  ((comp n1-mapping-scheme row1-operator) input))

(defn neuron2 [input]
  ((comp n2-mapping-scheme row1-operator) input))

(defn neuron3 [input]
  ((comp n3-mapping-scheme row1-operator) input))

(defn neuron4 [input]
  ((comp n4-mapping-scheme row1-operator) input))

(defn neuron5 [inputs]
  ((comp n5-mapping-scheme n5-operator) inputs))

(defn neuron6 [inputs]
  ((comp n6-mapping-scheme n6-operator) inputs))

(defn neuron7 [inputs]
  ((comp n7-mapping-scheme n7-operator) inputs))

(defn neuron8 [inputs]
  (n8-operator inputs))


;; Circuit abstraction

(defn lstm-circuit [recur-inputs inputs]

  ; Fire first row, bind outputs
  (let [output-tbl1 (neuron1 (nth inputs 0))
        output-tbl2 (neuron2 (nth inputs 1))
        output-tbl3 (neuron3 (nth inputs 2))
        output-tbl4 (neuron4 (nth inputs 3))

  ; Fire second row, bind outputs
        output-tbl5 (neuron5 [(:5 output-tbl1) (:5 output-tbl2)])
        output-tbl6 (neuron6 [(:6 output-tbl3) (:6 recur-inputs)])

  ; Fire third row, bind outputs
        output-tbl7 (neuron7 [(:7 output-tbl5) (:7 output-tbl6)])

  ; Fire fourth row, bind outputs
        output      (neuron8 [(:8 output-tbl7) (:8 output-tbl4)])]
  
  ; Determine whether to output or recur
  (if (not (== -1 output))
    output
    (partial lstm-circuit {:6 (:6 output-tbl7)}))))

(defn blank-lstm []
  (partial lstm-circuit {:6 0}))


