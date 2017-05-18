"use strict"

var React = require("react")
var createReactClass = require("create-react-class")
var PropTypes = require("prop-types")

exports.withContext = function(BaseClass) {
  return function(ctx) {
    var cls = createReactClass({
      displayName: "WithContext",
      getChildContext: function() {
        return { ctx: ctx }
      },
      render: function() {
        return React.createElement(BaseClass, this.props)
      }
    })
    cls.childContextTypes = {
      ctx: PropTypes.any,
    }
    return cls
  }
}

exports.getFromContext_ = function(setCtx) {
  return function(f) {
    return function(BaseClass) {
      var GetContext = function(props, context) {
        var newProps = setCtx(f(context.ctx))(props)
        return React.createElement(BaseClass, newProps)
      }
      GetContext.displayName = "GetContext"
      GetContext.contextTypes = {
        ctx: PropTypes.any
      }
      return GetContext
    }
  }
}

exports.accessContext = function(BaseClass) {
  BaseClass.contextTypes = {
    ctx: PropTypes.any
  }
  return BaseClass
}

exports.readContext = function(p) {
  return function(_this) {
    return function() {
      return _this.context.ctx
    }
  }
}
