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
  [fn predicate current-col current-row field]
  (let [field-visitor-col-recurse (defn field-visitor-col-recurse
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
                               (rest field-row-rest)))))))]

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
                   (rest field-rest))))))))

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
  (let [ [[top-left-x top-left-y] [bottom-right-x bottom-right-y]]     subfield-definition ]
    (and  (>= x top-left-x)
          (<  x bottom-right-x)
          (>= y top-left-y)
          (<  y bottom-right-y))))

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

(defn calculate-sudoku-subfield-of
  "Calculates the subfield a given point is in"
  [x y]
  (let [lower-x (* 3 (quot x 3))
        upper-x (* 3 (inc (quot x 3)))
        lower-y (* 3 (quot y 3))
        upper-y (* 3 (inc (quot y 3)))]
        [[lower-x lower-y] [upper-x upper-y]]))

(defn contains-unresolved-fields?
  "Determines whether a sudoku field still contains unresolved fields"
  [sudoku-field]
  (not (empty?
      (filter
        #(seq (filter vector? %))
        sudoku-field))))



