(ns canvas.core
  (:require 
    [canvas.hexes :as hex]
    [canvas.model :refer [appstate]]
    [reagent.core :as r]))

(def ^:const hexsize 25)
(def ^:const axcenter [400 250])
(def ^:const dbcenter [20 20])

(defonce board {
  :range 6                  ; axial / cube
  :cols 8 :rows 6 :offset 2 ; rectangular
  })

(enable-console-print!) 

(defn set_reachable []
  (swap! appstate assoc :reachable (hex/hex_reachable [0 0 0] (:range @appstate) (:blocked @appstate)))
)

(defn- mouseclick [ evt ]
  (let [cube (:pixeltocube @appstate)]
    ;(swap! appstate assoc :origincube (hex/type_to_cube (-> @appstate :type vals) (-> @appstate :pixeltohex))
    ;                      :cubedistance 0 
    ;                      :line []
    ;                      :cube_range [])
    (if (not= cube [0 0 0])
        (if (some #(= cube %) (:blocked @appstate))
            (swap! appstate update :blocked disj cube)
            (swap! appstate update :blocked conj cube)))
    (set_reachable)
  ))
                          
(defn- mousemove [ evt ]
  (let [target (.-target evt)
        de     (.-documentElement js/document)
        mx     (-> (.-clientX evt) (- (.-offsetLeft target)) (+ (.-scrollLeft de)))
        my     (-> (.-clientY evt) (- (.-offsetTop  target)) (+ (.-scrollTop  de)))
        hex    (hex/pixel_to_hex (-> @appstate :type vals) [mx my] (-> @appstate :center) (-> @appstate :hexsize))]
    ;(prn pixel_to_hex_fn)
    (if (not= (-> @appstate :pixeltohex) hex)
        (swap! appstate assoc :pixeltohex   hex
                              :pixeltocube  (hex/type_to_cube (-> @appstate :type vals) hex)))))
 ;                             :cubedistance (-> @appstate :origincube (hex/cube_distance (convert_fn hex)))))))
 ;                             :line         (-> @appstate :origincube (hex/cube_linedraw (hex/axial_to_cube hex)))
 ;                             :range        (cube_range (-> @appstate :origincube)  (-> @appstate :origincube (hex/cube_distance (hex/axial_to_cube hex))))
 ;                             ))))
                   
    
            
; Linear interpolation for drawing lines between points
(defn- lerp [ a b t ]
  (-> b (- a) (* t) (+ a)))
(defn- vec_lerp [ a b t ]
  (map #(lerp (nth a %) (nth b %) t) (-> a count range)))
(defn vec_linedraw [ a b N ]
  (map #(vec_lerp a b (-> 1 (/ N) (* %))) (range (inc N))))

(defn- draw_grid_line [ ctx ]
  (let [a  (hex/pointy_hex_to_pixel (-> @appstate :center) (-> @appstate :origincube hex/cube_to_axial) (-> @appstate :hexsize))
        b  (hex/pointy_hex_to_pixel (-> @appstate :center) (-> @appstate :pixeltohex) (-> @appstate :hexsize))
        N  (hex/cube_distance (-> @appstate :pixeltohex hex/axial_to_cube) (-> @appstate :origincube))]
    (set! (.-lineWidth ctx) (/ (-> @appstate :hexsize) 5))
    (set! (.-strokeStyle ctx) "rgba(112,128,144,0.5)")  ;lightslategray
    (.beginPath ctx)
    (let [[x y] a] (.moveTo ctx x y))
    (let [[x y] b] (.lineTo ctx x y))
    (.stroke ctx)
    
    (set! (.-strokeStyle ctx) "#FFFFFF")
    (set! (.-lineWidth ctx) "1")
    (set! (.-fillStyle ctx) "rgba(124,0,124,0.8)")
    (doseq [[x y] (vec_linedraw a b N)]
        (.beginPath ctx)
        (.arc ctx x y (/ (-> @appstate :hexsize) 4) 0 (* 2 Math/PI))
        (.fill ctx)
        (.stroke ctx))))
        
;; Grids
      
(defn- draw_grid_axial [ ctx orig hexsize dist ]
  (doseq [q (range (- 1 dist) dist) r (range (- 1 dist) dist)]
    (if (> dist (Math/abs (+ q r)))
        (hex/draw_hex [(-> @appstate :type :grid) (-> @appstate :type :coord)] ctx [q r] orig hexsize))))
  
(defn- clip_grid_pair [ ctx orig hexsize c r o ]
  (.beginPath ctx)
  (let [[x y] orig] (.moveTo ctx x y))
  (doseq [hex [[(* c 2) 0] [(* c 2) r] [(* (+ c o) 2) r] [(* (+ c o) 2) (* r 2)] [(* o 2) (* r 2)] [(* o 2) r] [0 r]]]
    (let [[x y] (hex/doublewidth_to_pixel orig hex hexsize)]
      (.lineTo ctx x y)))
  (.closePath ctx)
  (.stroke ctx)
  (.clip ctx))
          
(defn- draw_grid_doublewidth [ ctx orig hexsize c r o ]
  (clip_grid_pair ctx orig hexsize c r o)
  (doseq [col (range (-> c (+ o) (* 2) inc))
          row (range (-> r (* 2) inc))]
    (if (= 0 (-> col (+ row) (mod 2)))
      (hex/draw_hex [(-> @appstate :type :grid) (-> @appstate :type :coord)] ctx [col row] orig hexsize))))
      
(defn- draw_grid_doubleheight [ ctx orig hexsize c r o ]
  (clip_grid_pair ctx orig hexsize c r o)
  (doseq [col (range (-> c (+ o) inc))
          row (range (-> r (* 2) (* 2) inc))]
    (if (= 0 (-> col (+ row) (mod 2)))
      (hex/draw_hex [(-> @appstate :type :grid) (-> @appstate :type :coord)] ctx [col row] orig hexsize))))
      
                  
; Main drawing functions
                    
(defn draw_page [ canvas ]
  (let [ctx (.getContext canvas "2d")
        w (.-clientWidth canvas) 
        h (.-clientHeight canvas)]
    (.clearRect ctx 0 0 w h)
    
    (.save ctx)
    (if (= :double (-> @appstate :type :coord))
      (if (= :pointy (-> @appstate :type :grid))
          (apply draw_grid_doublewidth ctx (-> @appstate :center) (-> @appstate :hexsize) (-> board (dissoc :range) vals))
          (apply draw_grid_doubleheight ctx (-> @appstate :center) (-> @appstate :hexsize) (-> board (dissoc :range) vals)))
      (draw_grid_axial ctx (-> @appstate :center) (-> @appstate :hexsize) (:range board)))
    (.restore ctx)))
    
(defn canvasclass [ ]
  (let [dom-node (r/atom nil)]
    (r/create-class
     {:component-did-update
        (fn [ this ]
          (draw_page (.getElementById js/document "drawing")))
      :component-did-mount
        (fn [ this ]
          (reset! dom-node (r/dom-node this)))
      :reagent-render
        (fn [ ]
          @appstate
          [:div.container.my-1
            [:div.my-2
              [:div.btn-group.mr-2
                [:button.btn.btn-outline-primary
                  {:on-click #(swap! appstate assoc-in [:type :grid]
                    (if (= :pointy (-> @appstate :type :grid))
                        :flat
                        :pointy))}
                  (if (= :pointy (-> @appstate :type :grid)) "pointy" "flat")]
                [:button.btn.btn-outline-primary
                  {:on-click 
                    (fn [] 
                      (swap! appstate assoc-in [:type :coord] (if (= :axial (-> @appstate :type :coord)) :double :axial))
                      (swap! appstate assoc :center (if (= :axial (-> @appstate :type :coord)) axcenter dbcenter)))}
                  (if (= :axial (-> @appstate :type :coord)) "axial" "double")]]
              [:input.mr-2 {:type "range" :min 0 :max 11 :value (:range @appstate)
                       :on-change (fn [e]
                                    (swap! appstate assoc :range (-> e .-target .-value js/parseInt))
                                    (set_reachable))}]
              [:span (str "Range: " (:range @appstate))]]
            [:canvas#drawing.border (if-let [node @dom-node] 
             {:width "1000px" 
              :height "500px"
              :on-click      mouseclick
              :on-mouse-move mousemove
              :on-mouse-out  #(swap! appstate dissoc :mx :my)})]
            [:div (str @appstate)]
            ])})))
            
(swap! appstate assoc :type {:grid :pointy :coord :axial}
                      :center axcenter
                      :hexsize 25)
(swap! appstate assoc :blocked  #{[0 -2 2] [1 -3 2] [-3 1 2] [-1 0 1] [2 -3 1] [2 -2 0] [2 -1 -1] [-4 1 3] [1 0 -1] [-1 -1 2] [0 2 -2] [-1 2 -1] [-2 1 1] [1 2 -3] [-5 1 4]}
                      :range 2)
(set_reachable)

; Nightvault Board - OOB calcs
; set out of bounds
(def cells 
  (for [x (range (-> board :cols (+ (:offset board) inc (* 2))))
        y (range (-> board :rows (* 2) inc))]
          [x y]))
(swap! appstate assoc :oob (->> cells 
                                (map 
                                  (fn [[x y]] 
                                    (if (or (= 0 x) 
                                            (= 0 y)
                                            (= (* (:rows board) 2) y)
                                            (and (<= x (-> board :offset (* 2))) (> y (-> board :rows dec)))
                                            (= x (-> board :cols (+ (:offset board)) (* 2)))
                                            (and (>= x (* (:cols board) 2)) (<= y (:rows board)))
                                            )
                                        [x y] 
                                        nil)))
                                (remove nil?)
                                (concat [[5 3] [7 3]])))
            
(r/render-component [canvasclass] (.getElementById js/document "app"))