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
   :femalep
   :make-address
   :prefecture
   :city
   :town))
(in-package :cl-gimei)

(defparameter *data-directory* (asdf:system-relative-pathname :cl-gimei #p"data/"))
(defparameter *names-pathname* (merge-pathnames "names.yml" *data-directory*))
(defparameter *addresses-pathname* (merge-pathnames "addresses.yml" *data-directory*))

(defgeneric kanji (name))
(defgeneric hiragana (name))
(defgeneric katakana (name))

(defstruct (item (:constructor make-item (kanji hiragana katakana))) kanji hiragana katakana)

(defun list-to-item (list)
  (apply #'make-item list))

(defun convert-items (items)
  (coerce (mapcar #'list-to-item items) 'simple-vector))

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

(defun load-name-table ()
  (let* ((table (yaml:parse *names-pathname*))
         (first-name (gethash "first_name" table))
         (last-name (gethash "last_name" table)))
    (make-name-table :last-name (convert-items last-name)
                     :female (convert-items (gethash "female" first-name))
                     :male (convert-items (gethash "male" first-name)))))

(defvar *name-table* (load-name-table))

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


(defstruct address-table
  prefecture
  city
  town)

(defun load-address-table ()
  (let* ((table (gethash "addresses" (yaml:parse *addresses-pathname*)))
         (prefecture (gethash "prefecture" table))
         (city (gethash "city" table))
         (town (gethash "town" table)))
    (make-address-table :prefecture (convert-items prefecture)
                        :city (convert-items city)
                        :town (convert-items town))))

(defvar *address-table* (load-address-table))

(defclass address ()
  ((prefecture
    :initform (alexandria:random-elt (address-table-prefecture *address-table*))
    :reader prefecture)
   (city
    :initform (alexandria:random-elt (address-table-city *address-table*))
    :reader city)
   (town
    :initform (alexandria:random-elt (address-table-town *address-table*))
    :reader town)))

(defun make-address ()
  (make-instance 'address))

(defmethod kanji ((address address))
  (concatenate 'string
               (kanji (prefecture address))
               (kanji (city address))
               (kanji (town address))))

(defmethod hiragana ((address address))
  (concatenate 'string
               (hiragana (prefecture address))
               (hiragana (city address))
               (hiragana (town address))))

(defmethod katakana ((address address))
  (concatenate 'string
               (katakana (prefecture address))
               (katakana (city address))
               (katakana (town address))))
