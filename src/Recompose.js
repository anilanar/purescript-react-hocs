"use strict"

exports.setDisplayName = function(displayName) {
  return function(cls) {
    cls.displayName = displayName
    return cls
  }
}
