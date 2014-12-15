(ns hw2)

;; =========
;; factorial

(defmulti my-factorial

          "Compute factorial of number. Dispatching based on number's value"

          (fn [num]
           (cond
            (< num 0) :negative
            (= num 0) :zero
            (> num 0) :positive)))

(defmethod my-factorial
 :zero [_]
 1)

(defmethod my-factorial
 :positive [num]
 (* num (my-factorial (- num 1))))

(defmethod my-factorial
 :negative [_]
 :undefined)


;; test factorial

(println (=
          120
          (my-factorial 5)))

(println (=
          :undefined
          (my-factorial -1)))


;; =============
;; bank transfer

(defn my-make-transfer [sum account1 account2]
 (dosync
  (alter account1 (fn [a b] (- a b)) sum)
  (alter account2 (fn [a b] (+ a b)) sum)))


;;test bank transfer

(println (=
          [5 15]
          (let [acc1 (ref 10)
                acc2 (ref 10)]
           (my-make-transfer 5 acc1 acc2)
           [(deref acc1) (deref acc2)]
           )
          ))


;; ======
;; macros

(defmacro my-or
 ([] nil)
 ([x] x)
 ([x & next] `(let [v# ~x] (if v# v# (my-or ~@next))))
 )

;; test

(println (=
          nil
          (my-or)
          ))
(println (=
          nil
          (my-or nil)
          ))
(println (=
          1
          (my-or nil nil nil 1 2 3)
          ))
(println (=
          false
          (my-or false)
          ))
(println (=
          []
          (my-or nil [])
          ))
(println (=
          1
          (my-or 1 nil 3 4 5)
          ))

(defmacro my-let

 [bindings & body]

  (if (or (not (vector? bindings)) (odd? (count bindings)))
    (throw (Exception. "Incorrect bindings format")))

  `((fn [~@(take-nth 2 bindings)] ~@body) ~@(take-nth 2 (rest bindings)))
 )

;; test

(my-let [a (- 1 1) b (+ 0 1)]
        (println (= a 0))
        (println (= b 1))
        )