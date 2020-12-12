(defsystem "adventofcode-2020"
           :description "Advent of Code 2020 in LISP"
           :author "Franck YVONNET"
           :serial t
           :depends-on (:aoc-coord
                        :aoc-misc
                        :fset
                        :cl-ppcre
                        :functional-queue
                        :trivia)
           :components ((:file "day01")
                        (:file "day02")
                        (:file "day03")
                        (:file "day04")
                        (:file "day05")
                        (:file "day06")
                        (:file "day07")
                        (:file "day08")
                        (:file "day09")
                        (:file "day10")
                        (:file "day11")
                        (:file "day12")))
