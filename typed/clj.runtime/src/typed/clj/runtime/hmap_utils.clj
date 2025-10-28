(ns typed.clj.runtime.hmap-utils
  "Utilities for HMap validation shared across projects.")

(defn valid-hmap-key-value?
  "Returns true if value is valid for use as an HMap key.
  Valid keys exclude NaN and regex patterns."
  [val]
  (keyword? val))
