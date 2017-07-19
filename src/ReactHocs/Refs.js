"use strict"

exports.unsafeSetProp = function(key) {
  return function(val) {
    return function(target) {
      return function() {
        target[key] = val
        return {}
      }
    }
  }
}
