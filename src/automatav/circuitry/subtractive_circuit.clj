(ns automatav.circuitry.subtractive-circuit)

;;;;;; SUBTRACTIVE CIRCUIT ;;;;;;;

;;;; Internal-Operator Fns

;; First Row

(defn row1-operator [input]
  [(- 0 input) input])


;; Second Row

(defn row2-operator [inputs]
  (reduce + 0 inputs))


;; Third Row

(defn row3-operator [inputs]
  (first (filter #(<= 0 %) inputs)))


;;;; Mapping-Schemes

;; First Row

(defn n1-mapping-scheme [outputs]
  {:3 (first outputs) :4 (second outputs)})

(defn n2-mapping-scheme [outputs]
  {:4 (first outputs) :3 (second outputs)})


;; Second Row

(defn row2-mapping-scheme [output]
  {:5 output})


;;;; Neuron Definitions

(defn neuron1 [inputs]
  ((comp n1-mapping-scheme row1-operator) inputs))

(defn neuron2 [inputs]
  ((comp n2-mapping-scheme row1-operator) inputs))

(defn neuron3 [inputs]
  ((comp row2-mapping-scheme row2-operator) inputs))

(defn neuron4 [inputs]
  ((comp row2-mapping-scheme row2-operator) inputs))

(defn neuron5 [inputs]
  (row3-operator inputs))


;;;; Circuit Abstraction

(defn subtractive-circuit [inputs]

  ; Fire first two neurons, bind outputs
  (let [output-tbl-a (neuron1 (first inputs))
        output-tbl-b (neuron2 (second inputs))

  ; Fire second two neurons, bind outputs
        output-tbl-c (neuron3 [(:3 output-tbl-a) (:3 output-tbl-b)])
        output-tbl-d (neuron4 [(:4 output-tbl-a) (:4 output-tbl-b)])]

  ; Fire final neuron, return output
  (neuron5 [(:5 output-tbl-c) (:5 output-tbl-d)])))


