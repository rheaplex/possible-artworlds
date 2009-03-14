
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aesthetics
;; Use some meta-programming to make declaring aesthetic types simpler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +aesthetic-properties+ 
  '(circle triangle square
    red yellow blue
    portrait landscape abstract
    grid perspective hierachical)
  "The properties of an aesthetic.")

(defparameter +number-of-aesthetic-properties+ 
  (length +aesthetic-properties+))

(defmacro define-aesthetic-values-class () 
  `(defclass <aesthetic-values> ()
     (,(dolist (property (+aesthetic-properties+))
	       `(,property :reader ,property-value :initarg :,property)))
     ((:documentation "Aesthetic measures."))))

;; Can the code just be inlined?
(define-aesthetic-values-class)

(defmacro define-aesthetic-values-subclass (name min-value max-value docstring)
  `(defclass ,name (<aesthetic-values>)
     ()
     ((:default-initargs (dolist (property (+aesthetic-properties+))
			   `(:,property (random-range ,min-value ,max-value))))
      (:docstring ,docstring))))

(defparameter aesthetic-strength-start-min -2.0
  "")

(defparameter aesthetic-strength-start-max 2.0
  "")

(defparameter aesthetic-strength-min -10.0
  "")

(defparameter aesthetic-strength-max 10.0
  "")

(define-aesthetic-values-subclass <aesthetic-strengths>
    aesthetic-strength-start-min aesthetic-strength-start-max 
    "Aesthetic strengths.")

(defparameter aesthetic-weight-min -1.0
  "")

(defparameter aesthetic-weight-max 1.0
  "")

(define-aesthetic-values-subclass <aesthetic-values>
    aesthetic-weight-min aesthetic-weight-max 
    "Aesthetic weights.")

(defmethod evaluate-aesthetic ((values <aesthetic-strengths>) 
			       (weights <aesthetic-weights>))
  "Score a concrete instance of an aesthetic according to an abstract one."
  (/ (+ (* (circle-value values) (circle-weight weights)) 
	(* (triangle-value values) (triangle-weight weights)) 
	(* (square-value values) (square-weight weights)) 
	(* (red-value values) (red-weight weights))
	(* (yellow-value values) (yellow-weight weights)) 
	(* (blue-value values) (blue-weight weights)) 
	(* (body-value values) (body-weight weights)) 
	(* (landscape-value values) (landscape-weight weights)) 
	(* (abstract-value values) (abstract-weight weights)) 
	(* (grid-value values) (grid-weight weights)) 
	(* (perspective-value values) (perspective-weight weights)) 
	(* (hierarchical-value values) (hierarchical-weight weights)))
     +number-of-aesthetic-properties+))

;; (defmethod describe-aesthetic-opinion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artworld players.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <person> ()
  ((id :accessor identifier 
       :initval (let ((id 0)) (lamda () 1+ id)))
   (aesthetic :accessor aesthetic-vision
	      :initval (make-instance 'aesthetic-weights))
   (artworks :accessor artworks
	     :initval (make-vector)))
  ((:documentation "A person. Artworks mean different things in subclasses.")))

(defmacro deftrivialsubclass (sub super docstring)
  `(defclass ,sub (,super) () (:documentation ,docstring)))

(deftrivialsubclass <artist> <person> "")

(deftrivialsubclass <critic> <person> "")

(deftrivialsubclass <collector> <person> "")

(defun make-artist ()
  (let (make-instance 'artist)))

(defclass <artwork> ()
  ((aesthetic :initarg :aesthetic 
	      :reader artwork-aesthetic)
   (artist :initarg :artist 
	   :reader artwork-artist)
   (critic :accessor atwork-critic)
   (collector :accessor artwork-collector)
   (created :accessor artwork-created
	:init-value *current-iteration*))
  ((:documentation "An artwork.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artworld population(s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +artists+ (make-vector)
  "Make artworks in their studio and send their favourites to the gallery.")

(defvar +critics+ (make-vector)
  "Evaluate artworks in the gallery and reccomend favourites to collectors.")

(defvar +collectors+ (make-vector)
  "Buy artworks reccomended by critics from galleries pay artists.")

(defvar +gallery+ (make-vector)
  "Artworks contributed by artists and visited by critics.")

(defparameter +start-num-artists+ 10)
(defparameter +max-num-artists+ 25)
(defparameter +min-num-artists+ 5)

(defparameter +start-num-critics+ 5)
(defparameter +max-num-critics+ 12)
(defparameter +min-num-critics+ 1)

(defparameter +start-num-collectors+ 12)
(defparameter +max-num-critics+ 30)
(defparameter +min-num-critics+ 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-artworld ()
  ""
  ;; artists
  ;; critics
  ;; collectors
  nil)

(defun update-artworld ()
  ""
  ;; Artists
  ;; Gallery
  ;; Critics
  ;; Collector
  nil)

(defparameter +artworld-iteration-delay+ 2
  "The amount of time to pause between each iteration, in seconds.")

(defvar *current-iteration* 0
  "The current iteration/time.")

(defun run-artworld (iterations)
  "Set up the artworld then iterate as many times as required."
  (format t "Setting up artworld.~%")
  (setup-artworld)
  (format t "Set up artworld.~%")
  (dotimes (*current-iteration* iterations)
    (format t "Iteration %i:~%" i)
    (update-artworld)
    (sleep (+artworld-iteration-delay+))))





