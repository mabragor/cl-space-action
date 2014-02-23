;;;; cl-space-action.asd

(asdf:defsystem #:cl-space-action
  :serial t
  :description "A game where 2 spaceships fight with each other."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-gtk2-gtk)
  :components ((:file "package")
               (:file "cl-space-action")))

