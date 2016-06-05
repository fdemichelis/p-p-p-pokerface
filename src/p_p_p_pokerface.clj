(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))


(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))


(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))


(defn full-house? [hand]
  (= 2 (apply min (vals (frequencies (map rank hand))))))




(defn two-pairs? [hand]
  (let [rep (sort > (vals (frequencies (map rank hand))))]
    (if (or (= 4 (first rep)) (= 2 (second rep))) true false)))


(defn straight? [hand]
  (let [f (sort (keys (frequencies (map rank hand))))]
    (if (or (and (= (count f) 5)
                 (= (- (apply max f)
                       (apply min f)) 4))
            (= f '(2 3 4 5 14)))
      true false)))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{ [high-card? 0] [pair? 1]
                    [two-pairs? 2]  [three-of-a-kind? 3]
                    [straight? 4]   [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}]
    (apply max (map (fn [[f,v]] (if (f hand) v 0)) checkers))))


