(ns clojure-sudoku.core)

(defn parse-row
  "Parses a row of a sudoku definition"
  [sudoku-row]
  (map #(if (= \. %)
           (range 0 10)
           (Integer/parseInt (str %))) sudoku-row))

(defn parse-field
  "Parses a complete sudoku field"
  [field-definition]
  (map parse-row field-definition))

(defn transpose
  "Transposes a field"
  [field]
  (apply map vector field))

(defn field-visitor-recurse
  "Returns a new field with applied fn to all fields matching the predicate"
  [function predicate current-col current-row field]
  (let [field-visitor-col-recurse
              (defn field-visitor-col-recurse
                [row-num field-row]
                (loop [current-col 0
                       result []
                       field-row-rest field-row]
                  (let [is-last-col (= current-col (count field-row))
                        current-cell-contents (atom (first field-row-rest))]
                    (if is-last-col
                      result
                      (do (if (predicate current-cell-contents current-col row-num)
                            (swap! current-cell-contents #(function % current-col row-num)))
                        (recur (inc current-col)
                               (conj result @current-cell-contents)
                               (rest field-row-rest)))))))]

    (loop [current-row current-row
           result []
           field-rest field]
      (let [is-last-row (= current-row (count field))
            current-row-contents (atom (first field-rest))]
        (if is-last-row
          result
          (do
            (swap! current-row-contents #(field-visitor-col-recurse current-row %))
            (recur (inc current-row)
                   (conj result @current-row-contents)
                   (rest field-rest))))))))

(defn field-visitor
  "Returns a new field with applied fn to all fields matching the predicate"
  [fn predicate field]
  (field-visitor-recurse fn predicate 0 0 field))


(defn rownum-equals?
  [match-row _ x y]
  (= match-row y))

(defn colnum-equals?
  [match-col _ x y]
  (= match-col x))

(defn subfield-equals?
  [subfield-definition _ x y]
  (let [ [[top-left-x top-left-y] [bottom-right-x bottom-right-y]] subfield-definition]
    (and  (>= x top-left-x)
          (<  x bottom-right-x)
          (>= y top-left-y)
          (<  y bottom-right-y))))

(defn filter-row
  [predicate? row-num field]
  (remove nil?
    (reduce into
            (field-visitor (fn [contents _ y]
                            (if (and
                                 (= row-num y)
                                 (predicate? contents))
                              contents
                              nil))
                          (fn [_ _ _] true)
                          field))))

(defn map-row
  "Applies fn to sudoku row row-num in field"
  [function row-num field]
  (field-visitor function (partial rownum-equals? row-num) field))

(defn map-col
  "Applies fn to sudoku column col-num in field"
  [function col-num field]
  (field-visitor function (partial colnum-equals? col-num) field))

(defn map-subfield
  "Applies fn to a subfield of field"
  [function sub-field field]
  (field-visitor function (partial subfield-equals? sub-field) field))

(defn calculate-sudoku-subfield-of
  "Calculates the subfield a given point is in"
  [x y]
  (let [generic-raster (fn [inner-fn multiple number]
                           (* multiple (inner-fn (quot number multiple))))
        floor-three #(generic-raster identity 3 %)
        ceil-three #(generic-raster inc 3 %)

        lower-x (floor-three x)
        upper-x (ceil-three x)
        lower-y (floor-three y)
        upper-y (ceil-three y)]

        [[lower-x lower-y] [upper-x upper-y]]))

(defn contains-unresolved-fields?
  "Determines whether a sudoku field still contains unresolved fields"
  [sudoku-field]
  (not (empty?
      (filter
        #(seq (filter vector? %))
        sudoku-field))))

;;(defn reduce-field
;;   "Reduces the unresolved cells of a field that is not completely resolved to the only left logical solutions"
;;   [field]
;;   (let [reduce (fn [cell x y]
;;                  (if (seq cell)
;;                    (do
;;                      (let [allowed-numbers cell]
;;                        (map-row y )))))]
;;     (field-visitor reduce #(true) %)))


;; (def field-solved? (atom false))

;; (defn solve-sudoku
;;   "Tries to solve a sudoku field"
;;   [field]
;;   (while (not @field-solved?)
;;     (let [current-field (atom field)
;;           try-solve (fn [field])]
;;       (do
;;         (swap! current-field (field-visitor fn #(true) %))
;;         (swap! field-solved? #(contains-unresolved-fields? current-field))))))





