"use strict";

var defs = require("./defs");


var columnCount = 24;
var columnWidth = 15;
var columnsPerGroup = 1;
var columnsPerLabel = 2;
var rowCount = 10;
var rowHeight = 30;
var rowsPerGroup = 1;
var marginSize = 20;
var paddingSize = 2;

var boxWidth = columnCount * columnWidth;
var boxHeight = rowCount * rowHeight;


function makeDefaultColumnLabel(x) {
  return (
    (x === 0 || x === 24) ? "midnight" :
    (x === 12) ? "noon" :
    (x % 12) + (x < 12 ? "am" : "pm"));
}

function makeDefaultRowLabel(y) {
  return (y * 100 / 10) + "%";
}


module.exports = {
  paintTIFace: function (c) {
    c.lineWidth = 1 / window.devicePixelRatio;
    c.beginPath();
    c.moveTo(0, 0);
    c.lineTo(boxWidth + 2 * paddingSize, 0);
    c.lineTo(boxWidth + 2 * paddingSize, boxHeight + 2 * paddingSize);
    c.lineTo(0, boxHeight + 2 * paddingSize);
    c.lineTo(0, 0);
    c.globalAlpha = 0.75;
    c.fill();
    c.globalAlpha = 1;
    c.stroke();
  },

  // TODO: Refactor
  getMeanColor: function (z) {
    var l = 10 + z * 80;
    return "hsl(0, 0%, " + l + "%)";
  },

  // TODO: Refactor
  paintTIGlobalMeans: function (c) {
    c.lineWidth = 1 / window.devicePixelRatio;
    var l = this.loadedTileCount / defs.maxTileCount;
    for (var x = 0; x < columnCount; x++) {
      var k = this.getRenderedImageCount(x, this.floorZoomPower) / defs.maxTileCount;
      var z = this.globalMeanTravelTimes[x] / this.maxGlobalMeanTravelTime;
      var h = Math.floor(z * boxHeight);
      var style = this.getMeanColor(z);
      c.fillStyle = style;
      var h1 = Math.floor(h * l);
      var h2 = Math.floor(h * k);
      c.globalAlpha = 0.5;
      c.fillRect(x * columnWidth, boxHeight - h1, columnWidth, h1);
      c.globalAlpha = 1;
      c.fillRect(x * columnWidth, boxHeight - h2, columnWidth, h2);
    }
  },

  paintTIGrid: function (c) {
    c.lineWidth = 0.5 / window.devicePixelRatio;
    c.beginPath();
    c.moveTo(0, 0);
    c.lineTo(boxWidth, 0);
    c.lineTo(boxWidth, boxHeight);
    c.lineTo(0, boxHeight);
    c.lineTo(0, 0);
    for (var x = 1; x < columnCount; x++) {
      if (x % columnsPerGroup === 0) {
        var w = x * columnWidth;
        c.moveTo(w, 0);
        c.lineTo(w, boxHeight);
      }
    }
    for (var y = 1; y < rowCount; y++) {
      if (y % rowsPerGroup === 0) {
        var h = y * rowHeight;
        c.moveTo(0, h);
        c.lineTo(boxWidth, h);
      }
    }
    c.globalAlpha = 0.5;
    c.stroke();
    c.globalAlpha = 1;
  },

  paintTICurrentTime: function (c) {
    c.lineWidth = 1 / window.devicePixelRatio;
    c.strokeRect(this.floorTimeValue * columnWidth, 0, columnWidth, boxHeight);
    var w = Math.floor(this.easedTimeValue * columnWidth * 2) / 2;
    c.beginPath();
    c.moveTo(w, 0);
    c.lineTo(w, boxHeight);
    c.lineTo(w, 0);
    var z = this.globalMeanTravelTimes[this.floorTimeValue] / this.maxGlobalMeanTravelTime;
    var h = Math.floor(z * boxHeight);
    c.moveTo(0, boxHeight - h);
    c.lineTo(boxWidth, boxHeight - h);
    c.lineTo(0, boxHeight - h);
    c.globalAlpha = 0.75;
    c.setLineDash([2, 4]);
    c.stroke();
    c.setLineDash([]);
    c.globalAlpha = 1;
  },

  // TODO: Refactor
  paintLabel: function (c, label, x, y) {
    c.strokeText(label, x, y);
    c.fillText(label, x, y);
  },

  paintTILabels: function (c) {
    c.lineWidth = 4 / window.devicePixelRatio;
    c.font = 6 + "px " + defs.labelFont;
    c.textAlign = "center";
    c.textBaseline = "top";
    for (var x = 0; x <= columnCount; x++) {
      if (x % columnsPerLabel === 0) {
        this.paintLabel(c, makeDefaultColumnLabel(x), x * columnWidth, boxHeight + 4);
      }
    }
    c.textAlign = "right";
    c.textBaseline = "middle";
    for (var y = 0; y <= rowCount; y++) {
      if (y % rowsPerGroup === 0) {
        this.paintLabel(c, makeDefaultRowLabel(y), -4, (rowCount - y) * rowHeight);
      }
    }
    c.font = 9 + "px " + defs.labelFont;
    c.textAlign = "center";
    c.textBaseline = "bottom";
    this.paintLabel(c, makeDefaultColumnLabel(this.floorTimeValue) + "—" + makeDefaultColumnLabel(this.floorTimeValue + 1), (this.floorTimeValue + 0.5) * columnWidth, -4);
    c.textAlign = "left";
    c.textBaseline = "middle";
    var z = this.globalMeanTravelTimes[this.floorTimeValue] / this.maxGlobalMeanTravelTime;
    var h = Math.floor(z * boxHeight);
    this.paintLabel(c, Math.round(h * 100 / 10 / rowHeight) + "%", boxWidth + 4, boxHeight - h);
  },

  paintTimeInspector: function (c) {
    c.save();
    c.fillStyle = defs.backgroundColor;
    c.strokeStyle = defs.borderColor;
    c.translate(marginSize, this.state.clientHeight - boxHeight - 2 * paddingSize - marginSize);
    this.paintTIFace(c);
    c.translate(paddingSize, paddingSize);
    this.paintTIGlobalMeans(c);
    c.fillStyle = defs.inverseBackgroundColor;
    c.strokeStyle = defs.inverseBackgroundColor;
    this.paintTIGrid(c);
    this.paintTICurrentTime(c);
    c.fillStyle = defs.inverseBackgroundColor;
    c.strokeStyle = defs.backgroundColor;
    this.paintTILabels(c);
    c.restore();
  }
};
