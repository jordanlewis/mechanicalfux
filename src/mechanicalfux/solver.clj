(ns mechanicalfux.solver)

(defn get-interval [a b]
  (let [v (- a b)]
    (max v (- v))))

(defn as-intervals [a]
  (pop
    (reduce (fn [xs x]
              (let [l (last xs)]
                (conj (pop xs) (- x l) x)))
            [(first a)] (drop 1 a))))

(defn at-most-two-consecutive-leaps [intervals]
  (every?
    (fn [[a b c]]
      (if (and (> a 1) (> b 1)) (<= c 1) true))
    (partition 3 1 (map (fn [x] (max x (- x))) intervals))))

(defn no-consective-thirds [intervals]
  (not-any?
    (fn [[a b]]
      (or (= a b 2)
          (= a b -2)))
    (partition 2 1 intervals)))

(defn no-overly-long-runs [intervals]
  (not-any? #(> (max % (- %)) 5)
            (reductions
              (fn [acc interval]
                (cond (pos? interval) (if (>= acc 0) (inc acc) 0)
                      (neg? interval) (if (<= acc 0) (dec acc) 0)
                      :else 0))
              0 intervals)))

(defn dissonant? [interval]
  ;; no perfect or tritone fourths
  (contains? #{3} interval))

(defn no-dissonance [intervals]
  (not-any? dissonant? intervals))

(defmacro loudand
  ([] (prn "valid!") true)
  ([x] x)
  ([x & nxt]
   `(let [and# ~x]
      (if and# (loudand ~@nxt) (do (prn "failed " (second  (first '(~&form)))) and#)))))

(defn valid [cf b]
  (let [counterpoint-intervals (as-intervals b)
        intervals (map get-interval cf b)
        cfl (count cf)
        bl (count b)]
    (prn cf b bl cfl)
    (loudand
      (no-dissonance intervals)
      (contains? #{0 7 4} (first intervals)) ;; perfect start
      (or (< bl cfl) (contains? #{0 7} (last intervals))) ;; end
      (or (< bl (dec cfl)) (contains? #{2 5} (nth intervals (- cfl 2)))) ;; 3rd or 5th ending
      ;;(not-any? #(= 0 %) (take (- cfl 2) (rest intervals)))
      (> 10 (- (apply max b) (apply min b))) ;; at most a tenth range
      (no-overly-long-runs counterpoint-intervals)
      (no-consective-thirds counterpoint-intervals))))

;; return nil if no matches
(defn doit [cf x next-notes]
  (prn "counterpoint" x)
  ;; x is valid
  (if (= (count cf) (count x))
    (do "Success!!!" x)
    (let [result (->> next-notes
                      shuffle
                      (map (fn [n elt] (do (prn "guess" n elt) (conj x elt))) (range))
                      (filter #(valid cf %))
                      (map (fn [n cp] (do (prn "valid" n cp) (doit cf cp next-notes))) (range))
                      (filter identity)
                      first)]
      (prn "returning result: " result)
      result)))

(defn make-species-one-counterpoint [x]
  (doit x [] (range (apply min x) (+ 10 (apply max x)))))

(def cantus-firmus [0 2 1 0 3 2 4 3 2 1 0])

