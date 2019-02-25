(ns canvas.hexes
  (:require 
    [canvas.model :refer [appstate]]
    [canvas.htmlcolours :refer [col_to_rgb]]))
    
(def ^:const sqrt3 (.sqrt js/Math 3))
(def ^:const axial_directions [ [1 0] [0 1] [-1 1] [-1 0] [0 -1] [1 -1] ])
(def ^:const cube_directions [ [1 -1 0] [1 0 -1] [0 1 -1] [-1 1 0] [-1 0 1] [0 -1 1] ])
(def ^:const cube_diagonals [ [2 -1 -1] [1 1 -2] [-1 2 -1] [-2 1 1] [-1 -1 2] [1 -2 1] ])
(def ^:const doublewidth_directions [[2 0] [1 -1] [-1 -1] [-2 0] [-1 1] [1 1]])

(defn- hex_add [ a b ]
  (map #(+ (nth a %) (nth b %)) (range 0 2)))
(defn- cube_add [ a b ]
  (map #(+ (nth a %) (nth b %)) (range 0 3)))

(defn- hex_neighbour [ hex dir ]
  (hex_add hex (nth axial_directions dir)))
(defn- cube_neighbour [ cube dir ]
  (cube_add cube (nth cube_directions dir)))
(defn- cube_diagonal_neighbour [ cube dir ]
  (cube_add cube (cube_diagonals dir)))
(defn- doublewidth_neighbor [ hex direction ]
  (let [b (nth doublewidth_directions direction)]
    (hex_add hex b)))

(defn- lerp [ a b t ]
  (-> b (- a) (* t) (+ a)))
(defn- cube_lerp [ a b t ]
  (map #(lerp (nth a %) (nth b %) t) (range 3)))
    
; Conversion fns
(defn- axial_to_cube [ hex ]
  [(nth hex 0) (reduce - 0 hex) (nth hex 1)])
  
(defn- cube_to_axial [ cube ]
  [(nth cube 0) (nth cube 2)])
  
(defn- cube_to_doublewidth [ cube ]
  (let [[x y z] cube]
    [(-> x (* 2) (+ z)) z]))
    
(defn- doublewidth_to_cube [ hex ]
  (let [[col row] hex
        x (/ (- col row) 2)
        z row
        y (-> 0 (- x) (- z))]
    [x y z]))

(defn- cube_to_doubleheight [ cube ]
  (let [[x y z] cube]
    [x (-> z (* 2) (+ x))]))
    
  
(defn- doubleheight_to_cube [ hex ]
  (let [x  (nth hex 0)
        z  (/ (- (nth hex 1) (nth hex 0)) 2)
        y  (reduce - 0 [x z])]
    [x y z]))
  
(defn- cube_round [cube]
  (let [rx (Math/round (nth cube 0))
        ry (Math/round (nth cube 1)) 
        rz (Math/round (nth cube 2))
        dx (-> rx (- (nth cube 0)) Math/abs)
        dy (-> ry (- (nth cube 1)) Math/abs)
        dz (-> rz (- (nth cube 2)) Math/abs)]
    (if (and (> dx dy) (> dx dz))
        [(reduce - [0 ry rz]) ry rz]
        (if (> dy dz)
            [rx (reduce - [0 rx rz]) rz]
            [rx ry (reduce - [0 rx ry])]))))
            
(defn cube_distance [ a b ]
   (apply Math/max
    (map 
      #(-> (nth a %) (- (nth b %)) Math/abs) 
      (range 0 3))))

(defn- hex_corner [ center size corner adj ]
  (let [angle_deg (-> 60 (* corner) (- adj))
        angle_rad (-> Math/PI (/ 180) (* angle_deg))]
    [(+ (nth center 0) (* size (Math/cos angle_rad)))
     (+ (nth center 1) (* size (Math/sin angle_rad)))]))     
(defn- pointy_hex_corner [ center size corner ] (hex_corner center size corner 30))
(defn- flat_hex_corner [ center size corner ] (hex_corner center size corner 0))
     
;; PIXEL Calcs

; My Calculation:
;(defn- mid_pointy_hex [center hex size] ;hex = [q r]
;  (let [w (-> (.sqrt js/Math 3) (* size)) h (* size 2)
;        x (first center)                  y (second center)
;        q (first hex)                     r (second hex)]
;    [(+ x (+ (* q w) (* r (/ w 2))))
;     (+ y            (* r (* h (/ 3 4))))]))
     
(defn- pointy_hex_to_pixel [ center hex size ] 
  (let [sqrt3 (.sqrt js/Math 3)
        x     (first center)
        y     (second center)
        q     (first hex)
        r     (second hex)]
    [(+ x (* size (+ (* sqrt3 q) (* r (/ sqrt3 2))))) 
     (+ y (* size                (* r (/ 3 2))))])) 
     
(defn- pixel_to_pointy_hex [ pos center size ]
  (let [px (- (first pos) (first  center))
        py (- (second pos) (second center))
        pq (/ (- (* (/ sqrt3 3) px) (/ py 3)) size)
        pr (/                  (* (/ 2 3) py) size)]
    (-> [pq pr] axial_to_cube cube_round cube_to_axial)))
    
(defn- flat_hex_to_pixel [ center hex size ] 
  (let [sqrt3 (.sqrt js/Math 3)
        x     (first center)
        y     (second center)
        q     (first hex)
        r     (second hex)]
    [(+ x (* size                (* q (/ 3 2))))
     (+ y (* size (+ (* sqrt3 r) (* q (/ sqrt3 2)))))]))

(defn- pixel_to_flat_hex [ pos center size ]
  (let [px (- (first pos) (first  center))
        py (- (second pos) (second center))
        pq (/ (* (/ 2 3) px)                  size)
        pr (/ (- (* (/ sqrt3 3) py) (/ px 3)) size)]
    (-> [pq pr] axial_to_cube cube_round cube_to_axial)))
    
(defn- doublewidth_to_pixel [ orig hex size ]
  (let [[px py]   orig
        [col row] hex]
    [(-> (/ sqrt3 2) (* col) (* size) (+ px))
     (-> (/ 3 2)     (* row) (* size) (+ py))]))
     
(defn- pixel_to_doublewidth [ pos orig size ]
  (-> (pixel_to_pointy_hex pos orig size)
      (axial_to_cube)
      (cube_to_doublewidth)))
      
(defn- doubleheight_to_pixel [ orig hex size ]
  (let [[px py]   orig 
        [col row] hex]
    [(-> (/ 3 2)     (* col) (* size) (+ px))
     (-> (/ sqrt3 2) (* row) (* size) (+ py))]))  

(defn- pixel_to_doubleheight [ pos orig size ]
  (-> (pixel_to_flat_hex pos orig size)
      (axial_to_cube)
      (cube_to_doubleheight)))
    
(defn- cube_range [ center r ]
  nil); (map (fn [[x y]] (if (> r (Math/abs (+ x y))) [x y]) [x (range (- 1 r) r) y (range (- 1 r) r)])))

;; FILL Functions

(defn- fill_hex_axes [ ctx hex cube ]
  (set! (.-fillStyle ctx) 
    (if (= (:pixeltohex @appstate) hex)
        (col_to_rgb :lightyellow)
        (if (= (-> @appstate :pixeltohex first) (first hex))
            (col_to_rgb :lightgreen 0.5)
            (if (= (-> @appstate :pixeltohex second) (second hex))
                (col_to_rgb :lightblue 0.5)
                (if (= (-> @appstate :origincube) cube)
                    "rgb(255,255,255)"
                    "rgba(0,0,0,0)")))))
    (.fill ctx))
    
(defn- fill_text [ ctx center txt ]
  (set! (.-textAlign ctx) "center")
  (set! (.-font      ctx) "14px Sans-Serif")
  (set! (.-fillStyle ctx) "rgb(0,0,0)")
  (let [[x y] center] (.fillText ctx txt x y)))
    
(defn- fill_hex_distance [ ctx cube ]    
  (set! (.-fillStyle ctx)
    (if (= (:cubedistance @appstate) (cube_distance (-> @appstate :origincube) cube))
        (col_to_rgb :lightcyan)
        (if (= cube (-> @appstate :origincube))
            "rgb(255,255,255)"
            "rgba(0,0,0,0)")))
  (.fill ctx))
  
(defn- cube_linedraw [ a b ]
  (let [N  (cube_distance a b)]
    (map #(cube_round (cube_lerp a b (-> 1 (/ N) (* %)))) (range (inc N))))) ; (/ 0 N) = #div0, (-> 1 (/ N) (* 0)) = 0
  
(defn- fill_shape_line [ ctx hex ]
  (set! (.-fillStyle ctx)
    (if (= hex (-> @appstate :origincube cube_to_axial))
        "rgb(255,255,255)"
        (if (some #(= hex (cube_to_axial %)) (-> @appstate :line))
            (col_to_rgb :lavender)
            "rgba(0,0,0,0)")))
  (.fill ctx))
  
  
(defn- fill_shape_range [ ctx hex ]
  (set! (.-fillstyle ctx)
    (if (= hex (-> @appstate :origincube cube_to_axial))
        "rgb(255,255,255)"
        (if (some #(= hex (cube_to_axial %)) (-> @appstate :range))
            (col_to_rgb :turquoise)
            "rgba(0,0,0,0)")))
  (.fill ctx))
  
;function doublewidth_neighbor(hex, direction):
;    var dir = doublewidth_directions[direction]
;    return DoubledCoord(hex.col + dir.col, hex.col + dir.col)

(defn- get_cube_neighbours [ cube ]
 (map #(cube_neighbour cube %) (range 6)))
 
(defn hex_reachable [ startcube distance blocked ]
  (set
    (reduce concat
      (take (inc distance)
        (iterate 
          (fn [x]
            (clojure.set/difference
              (->> x
                   (map #(get_cube_neighbours %))
                   (reduce concat)
                   set)
              (-> @appstate :blocked set)))
          #{(seq startcube)})))))
        
;  function hex_reachable(start, movement):
;    var visited = set() # set of hexes
;    add start to visited
;    var fringes = [] # array of arrays of hexes
;    fringes.append([start])
;
;    for each 1 < k ≤ movement:
;        fringes.append([])
;        for each hex in fringes[k-1]:
;            for each 0 ≤ dir < 6:
;                var neighbor = hex_neighbor(hex, dir)
;                if neighbor not in visited and not blocked:
;                    add neighbor to visited
;                    fringes[k].append(neighbor)
;
;    return visited
   
(defn type_to_cube [ type hex ]
  (case type
    [:pointy :double] (doublewidth_to_cube hex)
    [:flat   :double] (doubleheight_to_cube hex)
    (axial_to_cube hex)))
    
(defn hex_to_pixel [ type hex center size ]
  (let [pixel_fn (case type 
                      [:pointy :axial] pointy_hex_to_pixel
                      [:flat   :axial] flat_hex_to_pixel
                      [:pointy :double] doublewidth_to_pixel
                      doubleheight_to_pixel)]
    (pixel_fn center hex size)))
    
(defn pixel_to_hex [ type pos center size ]
  (let [pixel_fn (case type 
                      [:pointy :axial] pixel_to_pointy_hex
                      [:flat   :axial] pixel_to_flat_hex
                      [:pointy :double] pixel_to_doublewidth
                      pixel_to_doubleheight)]
    (pixel_fn pos center size)))
    

(defn draw_hex [ type ctx hex center size ]
  (let [axial     (-> (type_to_cube type hex) cube_to_axial)
        orig      (hex_to_pixel type hex center size)
        corner_fn (if (-> type first (= :pointy)) pointy_hex_corner flat_hex_corner)]
    (.beginPath ctx)
    (let [[x y] (corner_fn orig size 0)]
      (.moveTo ctx x y))
    (doseq [[x y] (map #(corner_fn orig size %) (range 1 6))]
      (.lineTo ctx x y))
    (.closePath ctx)

    (set! (.-fillStyle ctx)
      (if (= [0 0] axial) ; center
          "#FFFFFF"
          (if (some #(= (axial_to_cube axial) %) (-> @appstate :blocked))
              (set! (.-fillstyle ctx) (col_to_rgb :peru))
              (if (some #(= (axial_to_cube axial) %) (-> @appstate :reachable))
                  (col_to_rgb :yellow)
                  (col_to_rgb :lightcyan)))))
    
    (.fill ctx)
    
    (set! (.-strokeStyle ctx) (col_to_rgb :lightslategray)) 
    (set! (.-lineWidth ctx) "1")  
    (.stroke ctx)
    
    (if (some #(= axial (cube_to_axial %)) (:blocked @appstate))
        nil
        (fill_text ctx (hex_to_pixel type hex center size) (cube_distance (-> @appstate :origincube ) (type_to_cube type hex))))
    
    
    ;(fill_hex_blocked )
    
    ;(fill_hex_axes ctx hex (type_to_cube type hex)) ; axial and double
    ;(fill_hex_distance ctx (doublewidth_to_cube hex))
    ;(fill_shape_line ctx hex)
    ;(fill_shape_range ctx hex)
    ;(fill_hex_neighbors ctx (-> @appstate :origincube cube_to_doublewidth) hex)
    
    
))