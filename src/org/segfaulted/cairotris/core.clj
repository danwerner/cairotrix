(ns org.segfaulted.cairotris.core
  "Cairotris - a Pajitnovian falling blocks game built on top of Clojure, GTK+ and Cairo"
  (:gen-class :name org.segfaulted.cairotris.core :main true )
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [org.gnome.gtk DrawingArea Gtk VBox Widget$ExposeEvent
                          Widget$KeyPressEvent Widget$KeyReleaseEvent
                          Window Window$DeleteEvent ]
           [org.gnome.gdk Event EventExpose Keyval]
           [org.freedesktop.cairo Context]))

(defonce gtk-init (Gtk/init (make-array String 0)))

(def MAX-FPS 10)
(def MIN-MILLIS-PER-FRAME (/ 1000 MAX-FPS))
(def TURN-TIME 1000)

(def WORLD-HEIGHT 15) ;blocks
(def WORLD-WIDTH 9)
(def BLOCK-SIZE 40) ;px
(def STARTING-POS [(dec (quot WORLD-WIDTH 2)) 0])

(def WORLD-WIN-HEIGHT (* WORLD-WIDTH BLOCK-SIZE)) ;px
(def WORLD-WIN-WIDTH (* WORLD-HEIGHT BLOCK-SIZE))

(def BACKGROUND-COLOR [0.0 0.0 0.0])

(def *running* (atom true))
(def *keys* (atom #{}))

(def COLORS
  {:black [0.0 0.0 0.0]
   :white [1.0 1.0 1.0]
   :red   [1.0 0.0 0.0]
   :green [0.0 1.0 0.0]
   :blue  [0.0 0.0 1.0]})

(def TETRACOLORS (dissoc COLORS :black))

;;; Util

(defn set-source-rgb [cr [r g b]]
  (.setSource cr r g b))

(defmacro with-context [cr & body]
  `(let [cr# ~cr]
     (try
       (.save cr#)
       (doto cr#
         ~@body)
       (finally
         (.restore cr#)))))

(defn indexed [coll]
  (map-indexed vector coll))

(defn map-values [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(defn current-millis []
  (System/currentTimeMillis))

(defn debug [obj & args]
  (assert (or (instance? String obj) (not args)))
  (println (apply format (str obj) args))
  obj)

;;; Game Logic

(defn parse-tetra [tetra]
  (let [lines  (->> tetra str/split-lines (map str/trim))
        parsed (for [[y line] (indexed lines)
                    [x chr] (indexed line)]
                 (case chr
                   \X [x y]
                   \_ nil))]
    (set (keep identity parsed))))
  
(def TETRAS-RAW {
  :I "XXXX"
  :J "X__
      XXX"
  :L "__X
      XXX"
  :O "XX
      XX"
  :S "_XX
      XX_"
  :T "_X_
      XXX"
  :Z "XX_
      _XX"
})

(def TETRAS (map-values parse-tetra TETRAS-RAW))

(defn random-tetra []
  {:form (rand-nth (keys TETRAS))
   :color (rand-nth (keys TETRACOLORS))})

(defn make-game []
  {:turn 0
   :blocks #{}
   :current nil
   :next (random-tetra)
   :position nil})

(defn abs-tetra-block-coords [tetra position]
  (let [rel-coords (-> tetra :form TETRAS)
        [tx ty]    position]
    (for [[x y] rel-coords]
      [(+ x tx) (+ y ty)])))

(defn tetra->blocks [tetra position]
  (let [color (:color tetra)
        abs-coords (abs-tetra-block-coords tetra position)]
    (for [[x y] abs-coords]
      {:x x, :y y, :color color})))

(defn next-position [position]
  (let [[x y] position]
    [x (inc y)]))

(defn coords-overlap? [coords1 coords2]
  (set/intersection (set coords1) (set coords2)))

(defn tetra-hits-bottom? [tetra-block-coords]
  (some (fn [[_ y]] (>= y WORLD-HEIGHT)) tetra-block-coords))


(defn anchor-current-tetra [game]
  (let [{:keys [blocks current position]} game]
    (assoc game :blocks (set/union blocks (tetra->blocks current position)))))

(defn drop-next-tetra [game]
  (assoc game
    :current (:next game)
    :next (random-tetra)
    :position STARTING-POS))

(defn next-game-turn [game]
  (let [{:keys [blocks current position]} game
        world-block-coords     (map (juxt :x :y) blocks)
        old-tetra-block-coords (abs-tetra-block-coords current position)
        new-position           (next-position position)
        new-tetra-block-coords (abs-tetra-block-coords current new-position)
        collision?             (coords-overlap? new-tetra-block-coords
                                              world-block-coords)
        hits-bottom?           (tetra-hits-bottom? new-tetra-block-coords)]
    (if (or (seq collision?) hits-bottom?)
      ;; TODO: play sound here? or in the game loop => separation of concerns?
      (-> game anchor-current-tetra drop-next-tetra)
      (assoc game :position new-position))))


;;; Drawing

(defn draw-bg [cr]
  (doto cr
    (set-source-rgb BACKGROUND-COLOR)
    (.paint)))

(defn draw-block [cr block]
  (let [{:keys [x y color]} block]
    (doto cr
      (set-source-rgb (COLORS color))
      (.rectangle (* x BLOCK-SIZE) (* y BLOCK-SIZE)
                  BLOCK-SIZE        BLOCK-SIZE)
      (.fill)
      (set-source-rgb BACKGROUND-COLOR)
      (.stroke))))

(defn draw-blocks [cr blocks]
  (doseq [block blocks]
    (draw-block cr block)))

(defn draw-tetra [cr tetra position]
    ;; Turning the current tetra into blocks with absolute world
    ;; coordinates allows us to use draw-blocks unchanged.
    (draw-blocks cr (tetra->blocks tetra position)))
  

(defn draw-all [^DrawingArea worldarea game]
  (let [target  (.getWindow worldarea)
        cr      (Context. target)
        {:keys [blocks current position]} game]
    (doto cr
      (draw-bg)
      (draw-blocks blocks)
      (draw-tetra current position))))

(defn redraw-all [^DrawingArea worldarea]
  (let [window (.getWindow worldarea)]
    (.queueDrawArea worldarea 0 0 (.getWidth window) (.getHeight window))))


;;; Main

(defn game-loop [^DrawingArea worldarea game-ref]
  (let [game-start (current-millis)]
    (loop [game @game-ref]
      (let [turn-start (current-millis)
            turn       (quot (- turn-start game-start) TURN-TIME)
            new-turn?  (< (:turn game) turn)
            game       (assoc game :turn turn)
            ;; TODO: check for input
            game       (if new-turn? (next-game-turn game) game)
            turn-time  (- (current-millis) turn-start)
            wait-time  (max 0 (- MIN-MILLIS-PER-FRAME turn-time))]

        ;; Expose events render the current state of the world.
        (reset! game-ref game)
        (redraw-all worldarea)

        (Thread/sleep wait-time)
        (if @*running*
          (recur game))))))

(defn -main []
  (let [win        (Window.)
        box        (VBox. false 1)
        worldarea  (DrawingArea.)
        game-ref   (atom (drop-next-tetra (make-game)))]
    (.connect win
      (reify Window$DeleteEvent
        (onDeleteEvent [_ source event]
          (reset! *running* false)
          (Gtk/mainQuit)
          false)))
    (.connect win
      (reify Widget$KeyPressEvent
        (onKeyPressEvent [_ source event]
          (swap! *keys* conj (.getKeyval event))
          false)))
    (.connect win
      (reify Widget$KeyReleaseEvent
        (onKeyReleaseEvent [_ source event]
          (swap! *keys* disj (.getKeyval event))
          false)))
    (.connect worldarea
      (reify Widget$ExposeEvent
        (onExposeEvent [_ source event]
          (draw-all source @game-ref)
          false)))
    (doto box
      (.add worldarea))
    (doto win
      (.add box)
      (.setTitle "Cairotris")
      (.setDefaultSize WORLD-WIN-HEIGHT WORLD-WIN-WIDTH)
      (.showAll))
    (future (game-loop worldarea game-ref))
    (Gtk/main)
    ;; I don't use agents, so why is this necessary?
    (shutdown-agents)))
