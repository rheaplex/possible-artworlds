
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes
;;
;; Lots of metaprogramming to create classes where plists would probably do.
;; The metaprogramming is to stay flexible, the classes are for type safety.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-keyword (name) 
  "Turn a word into a keyword."
  (values (intern (string-upcase (string name)) "KEYWORD")))

(defun make-reader-symbol (slot)
  "Make a symbol of the form slot-value."
  (values (intern (string-upcase (format nil "~s-value" slot)))))

(defmacro deftrivialsubclass (sub super docstring)
  "Define a trivial subclass of a class."
  `(defclass ,sub (,super) () (:documentation ,docstring)))

(defun random-range (min max)
  "Generate a number between min & max, assumes max is greater than min."
  (+ min
     (random (- max min))))

(defun random-sign (val)
  "Return the value with its sign reversed 50% of the time."
  (if (< 0.5 (random 1.0)) 
      val
      (* val -1.0)))

(defun next-value (current min-val max-val min-delta max-delta)
  "Generate a value between min and max varying from current by the delta."
  (let* ((delta (random-sign (random-range min-delta max-delta)))
	 (next (+ current delta)))
    ;; Constrain to range min-val..max-val
    (max min-val (min next max-val))))

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

(defun define-aesthetic-property-slot (property)
  `(,property :reader ,(make-reader-symbol property)
	      :initarg ,(make-keyword property)))

(defmacro define-aesthetic-values-class () 
  `(defclass <aesthetic-values> ()
     (,@(map 'list #'(lambda (property) 
		       (define-aesthetic-property-slot property))
	     +aesthetic-properties+))
     (:documentation "Aesthetic measures.")))

(define-aesthetic-values-class)

(defmacro define-aesthetic-values-subclass (name minval maxval docstring)
  `(defclass ,name (<aesthetic-values>)
     ()
     (:default-initargs ,@(loop for property in +aesthetic-properties+
			     collect (make-keyword property) 
			     collect  `(random-range ,minval ,maxval)))
     (:documentation ,docstring)))

(defparameter aesthetic-strength-start-min -2.0
  "")

(defparameter aesthetic-strength-start-max 2.0
  "")

(defparameter aesthetic-strength-min -10.0
  "")

(defparameter aesthetic-strength-max 10.0
  "")

(define-aesthetic-values-subclass '<aesthetic-strengths>
    aesthetic-strength-start-min aesthetic-strength-start-max 
    "Aesthetic strengths.")

(defparameter aesthetic-weight-min -1.0
  "")

(defparameter aesthetic-weight-max 1.0
  "")

(define-aesthetic-values-subclass <aesthetic-values>
    aesthetic-weight-min aesthetic-weight-max 
    "Aesthetic weights.")

(defmacro sum-aesthetic-properties (values weights)
  "Create code to sum weight * value for all aesthetic property slots."
  `(+ ,@(map 'list #'(lambda (property)
		       `(* (,(make-reader-symbol property) ,values) 
			   (,(make-reader-symbol property) ,weights)))
	     +aesthetic-properties+)))

(defmethod evaluate-aesthetic ((values <aesthetic-strengths>) 
			       (weights <aesthetic-weights>))
  "Score a concrete instance of an aesthetic according to an abstract one."
  (/ (sum-aesthetic-properties values weights)
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





