(defpackage :cl-gimei
  (:use :cl)
  (:export
   :kanji
   :hiragana
   :katakana
   :first-name
   :last-name
   :make-male
   :make-female
   :make-name
   :malep
   :femalep))
(in-package :cl-gimei)

(defparameter *data-directory* (asdf:system-relative-pathname :cl-gimei #p"data/"))
(defparameter *names-pathname* (merge-pathnames "names.yml" *data-directory*))
(defparameter *addresses-pathname* (merge-pathnames "addresses.yml" *data-directory*))

(defvar *name-table*)

(defgeneric kanji (name))
(defgeneric hiragana (name))
(defgeneric katakana (name))

(defstruct (item (:constructor make-item (kanji hiragana katakana))) kanji hiragana katakana)

(defun list-to-item (list)
  (apply #'make-item list))

(defmethod kanji ((name item))
  (item-kanji name))

(defmethod hiragana ((name item))
  (item-hiragana name))

(defmethod katakana ((name item))
  (item-katakana name))

(defstruct name-table
  last-name
  male
  female)

(defclass name ()
  ((first-name :initarg :first-name :reader first-name)
   (last-name :initarg :last-name :reader last-name)))

(defclass male (name)
  ()
  (:default-initargs
   :first-name (alexandria:random-elt (name-table-male *name-table*))
   :last-name (alexandria:random-elt (name-table-last-name *name-table*))))

(defclass female (name)
  ()
  (:default-initargs
   :first-name (alexandria:random-elt (name-table-female *name-table*))
   :last-name (alexandria:random-elt (name-table-last-name *name-table*))))

(defun malep (name) (typep name 'male))
(defun femalep (name) (typep name 'female))

(defun make-male ()
  (make-instance 'male))

(defun make-female ()
  (make-instance 'female))

(defun make-name ()
  (if (zerop (random 2))
      (make-male)
      (make-female)))

(defmethod kanji ((name name))
  (concatenate 'string (kanji (last-name name)) " " (kanji (first-name name))))

(defmethod hiragana ((name name))
  (concatenate 'string (hiragana (last-name name)) " " (hiragana (first-name name))))

(defmethod katakana ((name name))
  (concatenate 'string (katakana (last-name name)) " " (katakana (first-name name))))


(defun load-name-table ()
  (let* ((table (yaml:parse *names-pathname*))
         (first-name (gethash "first_name" table))
         (last-name (gethash "last_name" table)))
    (make-name-table :last-name (coerce (mapcar #'list-to-item last-name) 'simple-vector)
                     :female (coerce (mapcar #'list-to-item (gethash "female" first-name)) 'simple-vector)
                     :male (coerce (mapcar #'list-to-item (gethash "male" first-name)) 'simple-vector))))

(unless (boundp '*name-table*)
  (setf *name-table* (load-name-table)))
