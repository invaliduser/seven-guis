(ns seven-guis.circle
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(def canvas-el (atom nil))
(def ctx (atom nil))

(def actions (r/atom []))
(def undo-stack (r/atom '()))

(defn circle-action [[x y r]]
  {:circle [x y r (random-uuid)]})
(defn resize-action [[id r]]
  {:resize [id r]})

(def circles (r/atom {}))
(def filled-circle (atom nil))
(defn clear-circles! [](reset! circles {}))

(def current-mouse-position (r/atom []))
(def popup (r/atom nil))
(def resizing? (r/atom nil))

(defn submit-action! [action]
  (do
    (reset! undo-stack '())
    (swap! actions conj action)))

(defn execute-action! [action]
  (if-let [data (:circle action)]
    (swap! circles assoc (last data) data))
  (if-let [[id r] (:resize action)]
    (swap! circles assoc-in [id 2] r)))

(defn play-actions! []
  (doseq [action @actions]
    (execute-action! action)))


(defn mouse-position [e]
  (let [rect  (.getBoundingClientRect @canvas-el)
        scale-x (/ (.-width @canvas-el) (.-width rect))
        scale-y (/ (.-height @canvas-el) (.-height rect))]
    {:x (* scale-x (- (.-clientX e) (.-left rect)))
     :y (* scale-y (- (.-clientY e) (.-top rect)))}))


(defn ** [n] (* n n))
(defn distance [[x y] [ox oy]]
  (.sqrt js/Math  (+ (** (- x ox))
                     (** (- y oy)))))


(defn nearest-circle [[x y]]
  (let [reducer (fn [[lowest record-distance :as current] [_ [cx cy _ _ :as considered]]]
                  (let [considered-distance (distance [x y][cx cy])]
                        (if (or (nil? current)
                            (> record-distance
                               considered-distance))
                      [considered considered-distance]
                      current)))
        [result dist] (reduce reducer nil @circles)]
    result))


(defn between? [n [low high]];all-exclusive
  (and (> high n)
       (< low n)))

(defn is-inside?
  ([[x y r id] [cx cy]]
   (> r
      (distance [x y] [cx cy]))))

(defn is-inside-rect? [[[x y width height]] [cx cy]]
  (and (between? cx [x (+ x width)])
       (between? cy [y (+ y height)])))

(defn clear-canvas! []
  (.clearRect @ctx 0 0
              (.-width @canvas-el)
              (.-height @canvas-el)))

(defn draw-circle [[x y r]]
  #_(set! (.-strokeStyle @ctx) "rgb(200,0,0)" )
  (.beginPath @ctx)
  (.arc @ctx x y r 0 (* 2 (.-PI js/Math)))
  (.stroke @ctx))

(defn draw-filled-circle [[x y r & stuff]]
  (set! (.-fillStyle @ctx) "rgb(128,128,128)" )
  (.beginPath @ctx)
  (.arc @ctx x y r 0 (* 2 (.-PI js/Math)))
  (.fill @ctx))

(defn render-circles! []
  (doseq [[k [x y r id]] @circles]
    (draw-circle [x y r])))
(defn redraw! []
  (clear-canvas!)
  (render-circles!))

(defn close-out-resizing! []
  (let [id (@resizing? 3)
        new-radius (get-in @circles [id 2])
        action (resize-action [id new-radius])]
    (submit-action! action)
    (execute-action! action)
    (redraw!)))

(defn handle-click [e]
  (let [{:keys [x y]}(mouse-position e)
        action  (circle-action [x y 10])]
    (if (or @popup @resizing?)
      (do (reset! popup nil)
          (close-out-resizing!)
          (reset! resizing? nil))
    (do
      (submit-action! action)
      (execute-action! action)
      (draw-circle [x y 10])))))

(defn ensure-filled! [[x y r id :as nearest]]
  (let [[fx fy fr fid] @filled-circle]
    (cond
      (nil? @filled-circle)
      (do (draw-filled-circle nearest)
          (reset! filled-circle nearest))

      (= id fid)
      (do) ;-nothing

      (not= fid fid)
      (do
        (redraw!)
        (draw-filled-circle nearest)))))

(defn un-fill! []
  (let [[fx fy fr fid] @filled-circle]
    (cond
      (nil? @filled-circle)
      (do)

      :else
      (do (reset! filled-circle nil)
          (redraw!)))))

(defn resize-dialog []
  (if-let [[x y r id] @resizing?]
    (let  [rect (.getBoundingClientRect @canvas-el)
           c-width (.-width @canvas-el)
           c-height (.-height @canvas-el)
           scale-x (/ (.-width rect) c-width)
           scale-y (/ (.-height rect) c-height)]

      [:div {:style {:display (if x "inline flow" "none")
                     :position "relative"
                     :width (* 1.6 c-width)
                     :left (* .1 c-width)
                     :border "black solid 1px"
                     :height (* .65 c-height)
                     :top (- c-height)}}
       [:div [:p (str "Adjust diameter of circle at (" (Math/round x) "," (Math/round y) "), current radius:" (get-in @circles [id 2]))]]
       [:div [:input {:type "range"
                      :on-change #(do
                                    (swap! circles assoc-in [id 2] (.. % -target -value))
                                    (redraw!))
                      :value (get-in @circles [id 2])
                      :min 1 :max 100 :step 1}]]])))

(defn handle-mouse-move [e]
  (let [{:keys [x y]}(mouse-position e)]
    (reset! current-mouse-position [x y])
    (let [nearest (nearest-circle [x y])
          inside? (is-inside? nearest [x y])]
      (if inside?
        (ensure-filled! nearest)
        (un-fill!)))))

(defn handle-right-click [e]
  (do
    (.preventDefault e))
  (let [{:keys [x y]} (mouse-position e)
        nearest (nearest-circle [x y])
        inside? (is-inside? nearest [x y])]
    (if inside?
      (reset! popup nearest))))

(defn popup-comp []
  (if-let [[x y r id] @popup]
    (let [rect (.getBoundingClientRect @canvas-el)
          scale-x (/ (.-width rect)(.-width @canvas-el))
          scale-y (/ (.-height rect) (.-height @canvas-el))]
      
      [:span {:style {:border "black solid 1px"
                      :visibility (if x "visible" "hidden")
                      :display (if x "inline flow" "none")
                      :background-color "grey"
                      :padding "4px"
                      :position "relative"
                      :left  (* x scale-x)
                      :top (* scale-y (- y (.-height @canvas-el)))}
              :on-click #(do (reset! resizing? @popup)
                             (reset! popup nil))}
       "Adjust diameter"])))

(def e-listener (atom nil ))
(defn canvas-class []
  (r/create-class
   {:component-did-mount (fn [this]
                           (reset! canvas-el (rd/dom-node this))
                           (reset! ctx (.getContext @canvas-el "2d"))
                           (if-not @e-listener
                             (.addEventListener @canvas-el
                                                "contextmenu" handle-right-click)))

    :reagent-render (fn []
                      [:canvas
                       {:on-click handle-click
                        :on-mouse-move handle-mouse-move
                        :style {:width "100%" :border "black solid 1px"}}])}))

(defn undo! []
  (swap! undo-stack conj (peek @actions))
  (swap! actions pop)
  (clear-circles!)
  (play-actions!)
  (redraw!))

(defn redo! []
  (swap! actions conj (peek @undo-stack))
  (swap! undo-stack pop)
  (clear-circles!)
  (play-actions!)
  (redraw!))


(defn circle []
  [:div {:style {:width "50%"}}
   [:div {:style {:display "flex" :justify-content "center"} }
    [:button {:on-click undo!
              :disabled (or
                         (not (seq @actions))
                         @popup @resizing?)} "Undo"]
    [:button {:on-click redo!
              :disabled (or
                         (not (seq @undo-stack))
                         @popup @resizing?)} "Redo"]]
   [:div
    [canvas-class]
    [popup-comp]
    [resize-dialog]]])
