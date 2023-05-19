#lang racket/base

(provide (struct-out event-struct)
         (struct-out column-struct))

(struct event-struct (name columns row-gen) #:transparent)

(struct column-struct
  (template-name
   internal-name
   druid-type
   is-primary-timestamp)
  #:prefab)
