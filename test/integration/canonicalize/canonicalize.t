Test that a nonsense argument is caught
  $ stanc --canonicalize dummy
  stanc: not found
  [127]

Test capitalization - this should fail due to the lack of model_name, not the canonicalizer
  $ stanc --canonicalize DEPRECATIONS,parentheses,bRaCeS
  stanc: not found
  [127]
