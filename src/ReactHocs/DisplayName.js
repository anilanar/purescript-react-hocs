"use strict"

exports.getDisplayName = function(cls) {
  return cls.displayName
}

exports.mapDisplayName = function(f) {
  return function(cls) {
    cls.displayName = f(cls.displayName)
    return cls
  }
}

exports.setDisplayName = function(displayName) {
  return function(cls) {
    cls.displayName = displayName
    return cls
  }
}
