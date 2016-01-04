"use strict";


var _ = module.exports = {
  add: function (v1, v2) {
    return {
      x: v1.x + v2.x,
      y: v1.y + v2.y
    };
  },

  subtract: function (v1, v2) {
    return {
      x: v1.x - v2.x,
      y: v1.y - v2.y
    };
  },

  dot: function (v1, v2) {
    return v1.x * v2.x + v1.y * v2.y;
  },

  // perpendicular dot product
  // http://mathworld.wolfram.com/PerpDotProduct.html
  perpDot: function (v1, v2) {
    return v1.x * v2.y - v1.y * v2.x;
  },

  dilate: function (scale, v) {
    return {
      x: scale * v.x,
      y: scale * v.y
    };
  },

  length: function (v) {
    return Math.sqrt(v.x * v.x + v.y * v.y);
  },

  project: function (v1, v2) {
    return _.dot(v1, v2) / _.length(v2);
  },

  distance: function (v1, v2) {
    return _.length(_.subtract(v1, v2));
  },

  bounds: function (margin, v) {
    return {
      left: v.x - margin,
      top: v.y - margin,
      right: v.x + margin,
      bottom: v.y + margin
    };
  }
};