(defsystem "adventofcode-2020"
           :description "Advent of Code 2020 in LISP"
           :author "Franck YVONNET"
           :serial t
           :depends-on (:aoc-coord
                        :aoc-misc
                        :alexandria
                        :fset
                        :cl-ppcre
                        :functional-queue
                        :serapeum
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
                        (:file "day12")
                        (:file "day13")
                        (:file "day14")
                        (:file "day15")
                        (:file "day16")
                        (:file "day17")
                        (:file "day18")
                        (:file "day19")
                        (:file "day20")
                        (:file "day21")
                        (:file "day22")
                        (:file "day24")))
