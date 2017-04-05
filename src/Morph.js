"use strict"

var React = require("react")
var PropTypes = React.PropTypes

exports.withContext = function(BaseClass) {
  return function(ctx) {
    return React.createClass({
      displayName: "WithContext",
      getChildContext: function() {
        return { context: { ctx: ctx } }
      },
      render: function() {
        return React.createElement(BaseClass, this.props)
      }
    })
  }
}

exports.getFromContext_ = function(setCtx) {
  return function(f) {
    return function(BaseClass) {
      var GetContext = function(props, context) {
        return React.createElement(BaseClass, setCtx(props)(f(context.ctx)))
      }
      GetContext.displayName = "GetContext"
      GetContext.contextTypes = {
        context: PropTypes.object
      }
      return GetContext
    }
  }
}
