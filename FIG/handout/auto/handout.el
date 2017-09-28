(TeX-add-style-hook
 "handout"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "english") ("geometry" "top=0.8in" "bottom=1in" "left=1.05in" "right=1.10in" "headheight=0in" "headsep=3in")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "lineno"
    "amsfonts"
    "babel"
    "amssymb"
    "latexsym"
    "amsthm"
    "amsmath"
    "verbatim"
    "geometry"
    "graphicx")
   (TeX-add-symbols
    '("f" 1)
    '("comt" 1)
    "R"
    "Q"
    "N"
    "Z"
    "Ps"
    "ba"
    "spc")
   (LaTeX-add-amsthm-newtheorems
    "thm"
    "lem"
    "cor"
    "prop"
    "crit"
    "alg"
    "defn"
    "conj"
    "exmp"
    "exmps"
    "prob"
    "rem"
    "note"
    "claim"
    "summ"
    "case"
    "ack"))
 :latex)

