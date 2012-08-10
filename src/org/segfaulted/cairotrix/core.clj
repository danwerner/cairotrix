(ns org.segfaulted.cairotrix.core
  "Cairotrix - a Pajitnovian falling blocks game built on top of Clojure, GTK+ and Cairo"
  (:gen-class :name org.segfaulted.cairotrix.core :main true )
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [org.gnome.gtk DrawingArea Gtk VBox Widget$Draw
                          Widget$KeyPressEvent Widget$KeyReleaseEvent
                          Window Window$DeleteEvent ]
           [org.gnome.gdk Keyval]
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

;; Variable
(def RUNNING (atom true))
(def KEYS (atom #{}))

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
  {:shape (rand-nth (keys TETRAS))
   :color (rand-nth (keys TETRACOLORS))})

(defn drop-next-tetra [game]
  (assoc game
    :current (:next game)
    :next (random-tetra)
    :position STARTING-POS))

(defn make-game []
  (->
    {:turn 0
     :state "play"
     :game-start (current-millis)
     :blocks #{}
     :current nil
     :next (random-tetra)
     :position nil}
    (drop-next-tetra)))

(defn abs-tetra-block-coords [tetra position]
  (let [rel-coords (-> tetra :shape TETRAS)
        [tx ty]    position]
    (for [[x y] rel-coords]
      [(+ x tx) (+ y ty)])))

(defn tetra->blocks [tetra position]
  (let [color (:color tetra)
        abs-coords (abs-tetra-block-coords tetra position)]
    (for [[x y] abs-coords]
      {:x x, :y y, :color color})))

(defn blocks->coords [blocks]
  (map (juxt :x :y) blocks))

(defn next-position [position direction]
  (let [[x y] position
        [dx dy] direction]
    [(+ x dx) (+ y dy)]))

(defn coords-overlap? [coords1 coords2]
  (seq (set/intersection (set coords1) (set coords2))))

(defn tetra-hits-border? [tetra-block-coords]
  (some (fn [[x y]]
          (not (and (< y WORLD-HEIGHT)
                    (<= 0 x (dec WORLD-WIDTH)))))
        tetra-block-coords))


(defn anchor-current-tetra [game]
  (let [{:keys [blocks current position]} game]
    (assoc game :blocks
      (set/union blocks (tetra->blocks current position)))))

(defn move-current-tetra [game direction]
  (let [{:keys [blocks current position]} game
        world-block-coords     (blocks->coords blocks)
        old-tetra-block-coords (abs-tetra-block-coords current position)
        new-position           (next-position position direction)
        new-tetra-block-coords (abs-tetra-block-coords current new-position)
        collision?             (coords-overlap? new-tetra-block-coords
                                                world-block-coords)
        hits-border?           (tetra-hits-border? new-tetra-block-coords)]
    (if (or collision? hits-border?)
      (if (= direction [0 1])
        ;; Tetra has hit something on its way down
        (-> game anchor-current-tetra drop-next-tetra)
        ;; Tetra has hit something left/right, don't move
        game)
      ;; Path is clear, move the tetra
      (assoc game :position new-position))))


;;; Keyboard Input

(def KEY-DIRECTIONS
  {Keyval/Down  [0  1]
   Keyval/Left  [-1 0]
   Keyval/Right [ 1 0]})

(defn parse-keyboard-directions [keys]
  (filter identity (map KEY-DIRECTIONS keys)))


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


(defn draw-all [^DrawingArea worldarea cr game]
  (let [{:keys [blocks current position state]} game]
    (doto cr
      (draw-bg)
      (draw-blocks blocks)
      (draw-tetra current position))
    (when (= state "game-over")
      ;; TODO: Draw "Game Over" text
      )))

(defn redraw-all [^DrawingArea worldarea]
  (let [window (.getWindow worldarea)]
    (.queueDrawArea worldarea 0 0 (.getWidth window) (.getHeight window))))


;;; Main

(defn play-state [^DrawingArea worldarea game turn-start]
      (let [turn       (quot (- turn-start (:game-start game))
                             TURN-TIME)
            new-turn?  (< (:turn game) turn)
            game       (assoc game :turn turn)

            ;; Directions
            directions (parse-keyboard-directions @KEYS)
            directions (if new-turn?
                         (cons [0 1] directions)
                         directions)
            ;; Move
            game       (if directions
                         (reduce move-current-tetra game directions)
                         game)]

        (let [{:keys [blocks current position]} game
              tetra-block-coords (abs-tetra-block-coords current position)
              world-block-coords (blocks->coords blocks)
              collision? (coords-overlap? tetra-block-coords
                                          world-block-coords)]
          (if collision?
            (assoc game :state "game-over")
            game))))


(defn game-loop [^DrawingArea worldarea game-ref]
  (loop [game @game-ref]
    (let [state (:state game)
          turn-start (current-millis)
          game (case state
                 ("game-over" "new")
                   (make-game)
                 "play"
                   (play-state worldarea game turn-start))
          turn-time  (- (current-millis) turn-start)
          wait-time  (max 0 (- MIN-MILLIS-PER-FRAME turn-time))]

      ;; Draw signals render the current state of the world.
      (reset! game-ref game)
      (redraw-all worldarea)
      (Thread/sleep wait-time)

      (if (@KEYS Keyval/Escape)
        (reset! RUNNING false))

      (if @RUNNING
        (recur game)
        (Gtk/mainQuit)))))


(defn -main []
  (let [win        (Window.)
        box        (VBox. false 1)
        worldarea  (DrawingArea.)
        game-ref   (atom {:state "new"})]

    (.connect win
      (reify Window$DeleteEvent
        (onDeleteEvent [_ source event]
          (reset! RUNNING false)
          false)))
    (.connect win
      (reify Widget$KeyPressEvent
        (onKeyPressEvent [_ source event]
          (swap! KEYS conj (.getKeyval event))
          false)))
    (.connect win
      (reify Widget$KeyReleaseEvent
        (onKeyReleaseEvent [_ source event]
          (swap! KEYS disj (.getKeyval event))
          false)))
    (.connect worldarea
      (reify Widget$Draw
        (onDraw [_ source cr]
          (draw-all source cr @game-ref)
          false)))
    (doto box
      (.add worldarea))
    (doto win
      (.add box)
      (.setTitle "Cairotrix")
      (.setDefaultSize WORLD-WIN-HEIGHT WORLD-WIN-WIDTH)
      (.showAll))

    (future (Gtk/main))
    (game-loop worldarea game-ref)

    ;; I don't use agents, so why is this necessary?
    (shutdown-agents)
    (println "Quit.")))
