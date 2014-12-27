(ns clojure-sudoku.core)

(defn parse-row
  "Parses a row of a sudoku definition"
  [sudoku-row]
  (map #(if (= \. %)
           nil
           (Integer/parseInt (str %))) sudoku-row))

(defn parse-field
  "Parses a complete sudoku field"
  [field-definition]
  (map parse-row field-definition))

(defn transpose
  "Transposes a field"
  [field]
  (apply map vector field))

(defn field-visitor-col-recurse
  [fn predicate row-num field-row]
  (loop [current-col 0
         result []
         field-row-rest field-row]
    (let [is-last-col (= current-col (count field-row))
          current-cell-contents (atom (first field-row-rest))]
      (if is-last-col
        result
        (do (if (predicate current-col row-num)
              (swap! current-cell-contents #(fn %)))
          (recur (inc current-col)
                 (conj result @current-cell-contents)
                 (rest field-row-rest)))))))

(defn field-visitor-recurse
  "Returns a new field with applied fn to all fields matching the predicate"
  [fn predicate current-col current-row field]
  (loop [current-row current-row
         result []
         field-rest field]
    (let [is-last-row (= current-row (count field))
          current-row-contents (atom (first field-rest))]
      (if is-last-row
        result
        (do
          (swap! current-row-contents #(field-visitor-col-recurse fn predicate current-row %))
          (recur (inc current-row)
                 (conj result @current-row-contents)
                 (rest field-rest)))))))

(defn field-visitor
  "Returns a new field with applied fn to all fields matching the predicate"
  [fn predicate field]
  (field-visitor-recurse fn predicate 0 0 field))


(defn rownum-equals?
  [match-row x y]
  (= match-row y))

(defn colnum-equals?
  [match-col x y]
  (= match-col x))

(defn subfield-equals?
  [subfield-definition x y]
  (let [top-left     (first subfield-definition)
        bottom-right (nth subfield-definition 1)]
    (and  (>= x (first top-left))
          (<  x (first bottom-right))
          (>= y (nth top-left 1))
          (<  y (nth bottom-right 1))
          )))

(defn map-row
  "Applies fn to sudoku row row-num in field"
  [fn row-num field]
  (field-visitor fn (partial rownum-equals? row-num) field))

(defn map-col
  "Applies fn to sudoku column col-num in field"
  [fn col-num field]
  (field-visitor fn (partial colnum-equals? col-num) field))

(defn map-subfield
  "Applies fn to a subfield of field"
  [fn sub-field field]
  (field-visitor fn (partial subfield-equals? sub-field) field))
