(TeX-add-style-hook
 "lectio_7"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("beamer" "xcolor={usenames,dvipsnames,svgnames}" "compress")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontspec" "no-math")))
   (TeX-run-style-hooks
    "latex2e"
    "../clips-listings"
    "../colors"
    "beamer"
    "beamer10"
    "booktabs"
    "dcolumn"
    "colortbl"
    "ifxetex"
    "amsmath"
    "biblatex"
    "fontspec")
   (TeX-add-symbols
    "small")))

