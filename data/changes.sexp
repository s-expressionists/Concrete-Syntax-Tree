(:changes
 (:release "0.3" "2025-06-13"
  (:item
   (:paragraph
    "A" "malformed" (:symbol "LOOP") "in" "the" "internal" "macro"
    (:symbol "concrete-syntax-tree:with-bounded-recursion") "has" "been"
    "fixed" "." "Most" "implementations" "accepted" "the" "malformed" "loop"
    "and" "evaluated" "it" "with" "the" "intended" "semantics" "but" "Clasp"
    "is" "more" "strict" "and" "therefore" "required" "this" "fix" "."))
  (:item
   (:paragraph
    "The" "manual" "has" "been" "converted" "from" "LaTeX" "to" "texinfo"
    "."))
  (:item
   (:paragraph
    "An" "automatically" "generated" "Changelog" "section" "has" "been" "added"
    "to" "manual" ".")))

 (:release "0.2" "2025-06-07"
  (:item
   (:paragraph
    "concrete-syntax-tree" "now" "uses" "the" "fiveam" "system" "for"
    "its" "unit" "tests" "."))
  (:item
   (:paragraph
    (:symbol "concrete-syntax-tree:cst-from-expression") "can" "now" "handle"
    "arbitrary" "nesting" "depths" "and" "list" "lengths" "."))
  (:item
   (:paragraph
    (:symbol "concrete-syntax-tree:reconstruct") "can" "now" "handle"
    "arbitrary" "nesting" "depths" "and" "list" "lengths" "."))
  (:item
   (:paragraph
    (:symbol "concrete-syntax-tree:cst-from-expression") "now" "creates"
    "distinct" "CSTs" "for" "certain" "atoms" "that" "occur" "multiple" "times"
    "in" "the" "source" "expression" "," "namely" "numbers" "," "characters"
    "and" "symbols" "." "For" "other" "atoms" "such" "as" "pathnames" "or"
    "instances" "of" "standard" "classes" "," (:symbol "eq") "occurrences" "in"
    "the" "source" "expression" "are" "represented" "by" "multiple"
    "occurrences" "of" "a" "single" "CST" ".")
   (:paragraph
    "For" "example" "," "the" "source" "expression"
    (:tt "(1 1 :foo :foo #1=#P\"foo\" #1#)")
    "was"  "represented" "before" "this" "change" "as" "a" "CST" "with" "the"
    "following" "properties")
   (:code :common-lisp "(eq (cst:first result) (cst:second result))
(eq (cst:third result) (cst:fourth result))
(eq (cst:fifth result) (cst:sixth result))")
   (:paragraph
    "but" "is" "now" "represented" "as" "a" "CST" "with" "the" "following"
    "properties")
   (:code :common-lisp "(not (eq (cst:first result) (cst:second result)))
(not (eq (cst:third result) (cst:fourth result)))
(eq (cst:fourth result) (cst:sixth result))"))
  (:item
   (:paragraph
    (:symbol "concrete-syntax-tree:cst-from-expression") "and"
    (:symbol "concrete-syntax-tree:reconstruct") "are" "now" "slightly" "more"
    "efficient" ".")))

 (:release "0.1" "2023-03-16" ; Commit 3729172
  (:item
   (:paragraph
    "Initial" "version" "with" "CST" "classes" ","
    (:symbol "concrete-syntax-tree:cst-from-expression") ","
    (:symbol "concrete-syntax-tree:reconstruct") "," "lambda" "list" "parsing"
    "," "and" "a" "LaTeX-based" "manual" "."))))
