;
; see https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
; or Google/YouTube on Conway's Game of Life
;
; A simple game to practice a little bit of Clojure/JVM
;
; The game in short:
;   A visualisation to what happens if you have in your universe some space and
;   energy therein (eg. particles, molecule, atoms, amino-acids, proteins, cells..).
;   This energy will begin to interact and produce outcomes of unexpected complexity,
;   even at low energy densities and few natural laws.
;

(ns game-of-life.core
  (:require [clojure.string    :as str]
            [lanterna.screen   :as scn]))


(def scr (scn/get-screen :swing))


(def empty-space   " ")
(def energy-object "@")


(defn sample-container-row [cells probability]
  (into [] (take cells (repeatedly
    #(if (> (rand) probability)
        energy-object
        empty-space)))))

(defn sample-container [height width energy-object-probability]
  "creates a sample contariner with an initial test culture"
  (into [] (for [n (range height)]
    (sample-container-row width energy-object-probability))))


(defn is-populated [culture x y]
  (try
    (if (= energy-object (get (get culture x) y))
      1
      0)
    (catch Exception e ; eg. index out of bounds
      0)))

(defn count-immediate-neighbours [culture x y]
  "counts all populated cells that are sideways or adjacent to this cell"
  (let [search-range (range -1 2)]
    (reduce + (flatten
      (for [find-horizontally search-range]
        (for [find-vertically search-range]

          (if (and (= find-horizontally 0)
                   (= find-vertically   0))
            0 ;exclude self
            (is-populated culture
                          (+ x find-horizontally)
                          (+ y find-vertically)))))))))

(defn scan-neighbours [culture]
  "creates a population density chart about the test culture"
  (into [] (for [row (range (count culture))]
    (into [] (for [col (range (count (get culture row)))]
      (count-immediate-neighbours culture row col))))))


(defn evolve [culture]
  "creates a new population / sample container based on game rules"
  (let [density-chart (scan-neighbours culture)]
    (into [] (for [x (range (count culture))]
      (into [] (for [y (range (count (get culture x)))]

        (let [this       (get (get culture x) y)
              neighbours (get (get density-chart x) y)]

;        original rules
;          existing survives when 2-3 neighbours
;          empty space spawns when 3 neighbours
;
          (if (= this energy-object)
            (cond
              (or (> neighbours 3)
                  (< neighbours 2)) empty-space
              :else                 energy-object)

            (cond
              (or (= neighbours 3)) energy-object
              :else                 empty-space)))))))))



(defn draw-screen [culture]
  (loop [[row & remain] culture]
    (scn/put-string scr 0
                        (- (count culture) (count remain))
                        (str/join row))

    (if (< 0 (count remain))
      (recur remain)))

  (scn/redraw scr))



(defn -main [& args]
  (let [args        (set args)
        culture     (sample-container 60 230 (/ 900 1000))]

    (scn/start scr)
    (Thread/sleep 1000)
    (draw-screen culture)

    (loop [[now & future] [culture]]

      (draw-screen now)
      (Thread/sleep 200)

      (recur (conj future (evolve now))))
    ))
