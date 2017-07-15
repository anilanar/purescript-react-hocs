"use strict"

exports.writeIsMountedImpl = function(this_, isMounted) {
  this_.isMounted__ = isMounted
  return {}
}

exports.readIsMountedImpl = function(this_) {
  return !!this_.isMounted__
}
