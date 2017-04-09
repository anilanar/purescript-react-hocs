"use strict"

var React = require("react")
var PropTypes = require("prop-types")

exports.withContext = function(BaseClass) {
  return function(ctx) {
    var cls = React.createClass({
      displayName: "WithContext",
      getChildContext: function() {
        return { context: { ctx: ctx } }
      },
      render: function() {
        return React.createElement(BaseClass, this.props)
      }
    })
    cls.childContextTypes = {
      context: PropTypes.object,
    }
    return cls
  }
}

exports.getFromContext_ = function(setCtx) {
  return function(f) {
    return function(BaseClass) {
      var GetContext = function(props, context) {
        var newProps = setCtx(f(context.context.ctx))(props)
        return React.createElement(BaseClass, newProps)
      }
      GetContext.displayName = "GetContext"
      GetContext.contextTypes = {
        context: PropTypes.object
      }
      return GetContext
    }
  }
}
