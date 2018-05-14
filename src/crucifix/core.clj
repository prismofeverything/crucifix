(ns crucifix.core
  (:require
   [clojure.set :as set]
   [hiccup.core :as render]))

(def default-params
  {:box-width 20})

(defn vertical-line
  [lines width n]
  [:line
   {:x1 (* n width)
    :y1 0
    :x2 (* n width)
    :y2 (* width lines)
    :stroke "rgb(0,0,0)"
    :stroke-width 1}])

(defn horizontal-line
  [lines width n]
  [:line
   {:x1 0
    :y1 (* n width)
    :x2 (* width lines)
    :y2 (* n width)
    :stroke "rgb(0,0,0)"
    :stroke-width 1}])

(defn filled-box
  [width index n]
  [:rect
   {:width width
    :height width
    :x (* n width)
    :y (* index width)
    :fill "black"}])

(defn text-number
  [width [x y] number]
  [:text
   {:x (+ 2 (* x width))
    :y (+ 15 (* y width))}
   (inc number)])

(defn generate-grid
  [dimensions {:keys [box-width]}]
  (let [vertical (map (partial vertical-line dimensions box-width) (range (inc dimensions)))
        horizontal (map (partial horizontal-line dimensions box-width) (range (inc dimensions)))]
    (concat vertical horizontal)))

(defn generate-boxes
  [width rows]
  (mapcat
   (fn [row index]
     (map (partial filled-box width index) row))
   rows (range)))

(defn build-blocks
  [boxes]
  (apply
   set/union
   (map
    (fn [row index]
      (reduce conj #{} (map (fn [n] [n index]) row)))
    boxes (range))))

(defn find-numbers
  [boxes]
  (let [rows (count boxes)
        blocks (build-blocks boxes)]
    (println blocks)
    (mapcat
     (fn [y]
       (filter
        (fn [[x y]]
          (and
           (not (blocks [x y]))
           (or
            (zero? x)
            (zero? y)
            (blocks [(dec x) y])
            (blocks [x (dec y)]))))
        (map
         (fn [x]
           [x y])
         (range rows))))
     (range rows))))

(defn generate-numbers
  [width boxes]
  (let [numbers (find-numbers boxes)]
    (map
     (partial text-number width)
     numbers (range))))

(defn generate-layout
  [boxes letters params]
  (let [grid (generate-grid (count boxes) params)
        width (:box-width params)
        box (generate-boxes width boxes)
        numbers (generate-numbers width boxes)]
    [:html
     [:body
      [:svg
       {:height 1000
        :width 1000}
       (concat
        grid
        box
        numbers)]]]))

(defn render-layout
  [boxes letters params]
  (let [layout (generate-layout boxes letters params)]
    (spit "crossword.html" (render/html layout))))

(defn random-layout
  [dimensions p]
  (map
   (fn [y]
     (random-sample p (range dimensions)))
   (range dimensions)))

(defn -main
  [& args]
  ())
