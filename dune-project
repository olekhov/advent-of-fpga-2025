(lang dune 3.20)

(name advent_of_fpga)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Vasilii Olekhov <olekhov@gmail.com>")

(maintainers "Vasilii Olekhov <olekhov@gmail.com>")

(license LICENSE)

(documentation https://https://github.com/olekhov/advent-of-fpga-2025/README)

(package
 (name advent_of_fpga)
 (synopsis "Exploring FPGA for fun")
 (description "FPGA solutions for Advent of Code 2025")
 (depends ocaml)
 (tags
  ("learning" "fpga" "aoc" "advent of code"t)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
