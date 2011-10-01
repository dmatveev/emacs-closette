;; The job of the 'defclass macro (macro-expansion layer) is to parse the class
;; definition and convert it into a call to 'ensure-class (glue-layer)

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  `(ensure-class ',name
     :direct-superclasses ,(canonicalize-direct-superclasses direct-superclasses)
     :direct-slots ,(canonicalize-direct-slots direct-slots)
     ,@(canonicalize-defclass-options options)))


;; Create and initialize a new class metaobject (glue layer)

(defun ensure-class (name &rest all-keys)
  (if (find-class name nil)
      (error "Can't redefine the class named %s." name)
    (let ((class (apply 'make-instance 'standard-class :name name all-keys)))
      (setf (find-class name) class)
      class)))


;; Mantain the global mapping from class names to class metaobjects.
;; TODO The original AMOP implementation uses (let) scoping here to make
;; the hashtable local to the 'find-class and '(setf find-class).

(defvar *closette-class-table* (make-hash-table :test 'eq))

(defun find-class (symbol &optional no-errorp)
  (let ((class (gethash symbol *closette-class-table* nil)))
    (if (and (null class) (not no-errorp))
        (error "No class named %s." symbol)
      class)))

(defun put-class (symbol new-value)
  (setf (gethash symbol *closette-class-table*) new-value))

(defun priv-put-class (symbol value)
  (puthash symbol value *closette-class-table*))

(defsetf find-class priv-put-class)




(defclass standard-class ()
  ((name :initarg :name
         :accessor class-name)
   (direct-superclasses :initarg :direct-superclasses
                        :accessor class-direct-superclasses)
   (direct-slots :accessor class-direct-slots)
   (class-precedence-list :accessor class-precedence-list)
   (effective-slots :accessor class-slots)
   (direct-subclasses :initform ()
                      :accessor class-direct-subclasses)
   (direct-methods :initform ()
                   :accessor class-direct-methods)))