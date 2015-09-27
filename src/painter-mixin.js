"use strict";

/* global Path2D */

var defs = require("./defs");
var iid = require("./image-id");


module.exports = {
  paintTileBorders: function (c, easedZoomLevel) {
    if (!this.tileBordersPath) {
      var path = new Path2D();
      var lmx = defs.tileXCount * defs.imageSize;
      var lmy = defs.tileYCount * defs.imageSize;
      path.moveTo(0, 0);
      path.lineTo(0, lmy);
      for (var lx = 1; lx <= defs.tileXCount; lx++) {
        var ldx = lx * defs.imageSize;
        path.moveTo(ldx, 0);
        path.lineTo(ldx, lmy);
      }
      path.moveTo(0, 0);
      path.lineTo(lmx, 0);
      for (var ly = 1; ly <= defs.tileYCount; ly++) {
        var ldy = ly * defs.imageSize;
        path.moveTo(0, ldy);
        path.lineTo(lmx, ldy);
      }
      this.tileBordersPath = path;
    }
    c.lineWidth = easedZoomLevel / window.devicePixelRatio;
    c.strokeStyle = defs.borderColor;
    c.stroke(this.tileBordersPath);
  },

  paintTileLabels: function (c, easedZoomLevel) {
    var easedTextMargin = 4 * Math.sqrt(easedZoomLevel);
    c.fillStyle = defs.labelColor;
    c.font = 32 + "px " + defs.labelFont;
    c.textAlign = "left";
    c.textBaseline = "top";
    for (var lx = this.firstVisibleLocalX; lx <= this.lastVisibleLocalX; lx++) {
      var tx  = defs.localToTileX(lx);
      var ldx = lx * defs.imageSize;
      for (var ly = this.firstVisibleLocalY; ly <= this.lastVisibleLocalY; ly++) {
        var ty  = defs.localToTileY(ly);
        var ldy = ly * defs.imageSize;
        c.fillText(tx + "," + ty, ldx + easedTextMargin, ldy);
      }
    }
  },

  paintTileContents: function (c, easedZoomPower) {
    var zoomPower  = Math.round(easedZoomPower);
    var groupCount = Math.pow(2, zoomPower);
    var groupSize  = defs.imageSize * groupCount;
    var firstVisibleGroupX = Math.floor(this.firstVisibleLocalX / groupCount) * groupCount;
    var lastVisibleGroupX  = Math.floor(this.lastVisibleLocalX / groupCount) * groupCount;
    var firstVisibleGroupY = Math.floor(this.firstVisibleLocalY / groupCount) * groupCount;
    var lastVisibleGroupY  = Math.floor(this.lastVisibleLocalY / groupCount) * groupCount;
    for (var gx = firstVisibleGroupX; gx <= lastVisibleGroupX; gx += groupCount) {
      var gdx = gx * defs.imageSize;
      for (var gy = firstVisibleGroupY; gy <= lastVisibleGroupY; gy += groupCount) {
        var gdy = gy * defs.imageSize;
        var groupId = iid.fromLocal(gx, gy, zoomPower);
        var canvas = this.getRenderedGroup(groupId);
        if (canvas) {
          c.drawImage(canvas, gdx, gdy, groupSize, groupSize);
        }
      }
    }
  },

  paintNow: function () {
    var width  = window.devicePixelRatio * this.state.clientWidth;
    var height = window.devicePixelRatio * this.state.clientHeight;
    var canvas = this.canvas;
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width  = width;
      canvas.height = height;
    }
    var easedZoomPower = this.getEasedZoomPower();
    var easedZoomLevel = Math.pow(2, easedZoomPower);
    var easedImageSize = defs.imageSize / easedZoomLevel;
    var scrollLeft = Math.floor(this.getEasedAttentionLeft() * defs.tileXCount * easedImageSize - this.state.clientWidth / 2);
    var scrollTop  = Math.floor(this.getEasedAttentionTop() * defs.tileYCount * easedImageSize - this.state.clientHeight / 2);
    var c = canvas.getContext("2d", {
        alpha: false
      });
    c.setTransform(window.devicePixelRatio, 0, 0, window.devicePixelRatio, 0, 0);
    c.save();
    c.fillStyle = defs.backgroundColor;
    c.fillRect(0, 0, this.state.clientWidth, this.state.clientHeight);
    c.translate(-scrollLeft, -scrollTop);
    c.translate(0.5 / window.devicePixelRatio, 0.5 / window.devicePixelRatio);
    c.scale(1 / easedZoomLevel, 1 / easedZoomLevel);
    if (easedZoomPower < 3) {
      c.globalAlpha = 1 - (easedZoomPower - 2);
      this.paintTileLabels(c, easedZoomLevel);
      c.globalAlpha = 1;
    }
    this.paintTileBorders(c, easedZoomLevel);
    c.restore();
    c.translate(-scrollLeft, -scrollTop);
    c.scale(1 / easedZoomLevel, 1 / easedZoomLevel);
    this.paintTileContents(c, easedZoomPower);
    if (this.state.invertColor) {
      c.restore();
      c.globalCompositeOperation = "difference";
      c.fillStyle = "#fff";
      c.fillRect(0, 0, this.state.clientWidth, this.state.clientHeight);
      c.globalCompositeOperation = "source-over";
    }
    this.pendingPaint = false;
  },

  paint: function () {
    if (!this.pendingPaint) {
      this.pendingPaint = true;
      window.requestAnimationFrame(this.paintNow);
    }
  }
};
