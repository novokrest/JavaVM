(ns hw1)

;; task 1
(defn my-twice

 "Call input function twice"

 [callee arg]

 (callee arg)
 (callee arg)
 )

;; test
(my-twice println "task 1")

;;task 2
(defn my-reader

 "Reads file, prints its content and returns it"

 [filename]

 (let [content (slurp filename)]
  (println content)
  content)
 )

;; test
(println (=
          13
          (count (my-reader "task2.txt"))))

;; task 3
(def cube-anonymous

 "Returns cubed value"

 (fn [a]
  (loop [i 0 pow 1]
   (if (= i 3) pow (recur (inc i) (* pow a))))))

;; test
(println (=
          27
          (cube-anonymous 3)))

;; task 4
(defn my-concat-reverse

 "Takes two seq, reverses each of them and returns their concatenation"

 [seq1 seq2]

 (concat (reverse seq1) (reverse seq2))
 )

;; test
(println (=
          [3 2 1 5 4 3]
          (my-concat-reverse [1 2 3] [3 4 5])))

/
;; task 5
(defn my-exists

 "Checks if there is given element in seq"

 [seq elem]

 (loop [[head & tail] seq]
  (if (= head nil)
   false
   (if (= head elem) true (recur tail))))
 )

;; test
(println (=
          true
          (my-exists [1 2 3] 1)))
(println (=
          false
          (my-exists [1 2 3] 5)))

;; task 6
(defn my-pairs

 "Takes two seq and returns all distinct pairs of elements"

 [seq1 seq2]

 (let [dseq1 (distinct seq1)
       dseq2 (distinct seq2)
       f (fn [elem] (map (fn [c] (seq [elem c])) dseq2))]

  (mapcat f dseq1)

  )
 )

;; test
(println (=
          [[1 11] [1 10] [2 11] [2 10] [3 11] [3 10]]
          (my-pairs [1 2 3 2] [11 10 11])))

;; task 7
(defn my-seq-repeat

 "Returns seq consiting of n elem"

 [elem n]

 (loop [i 0 res nil]
  (if (< i n)
   (recur (inc i) (cons elem res))
   res
   ))
 )

;; test
(println (=
          1000000
          (apply + (my-seq-repeat 1 1000000))))