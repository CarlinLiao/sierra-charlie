"use strict";

var http = require("http-request-wrapper");
var simplify = require("simplify-js");

var origin;
var queuedTileIds = [];
var pendingTileId;
var loadedTileIds = {};

function getTileUrl(tileId) {
  return (
    origin + "/json/tile-" + tileId + (
      process.env.NODE_ENV === "production" ?
        ".json.gz" :
        ".json"));
}

function queueTileToLoad(tileId) {
  if (tileId !== pendingTileId && !(tileId in loadedTileIds)) {
    queuedTileIds.push(tileId);
  }
}

function queueTilesToLoad(tileIds) {
  for (var i = 0; i < tileIds.length; i++) {
    queueTileToLoad(tileIds[i]);
  }
}

function loadNextTile() {
  if (!pendingTileId) {
    while (queuedTileIds.length) {
      var tileId = queuedTileIds.pop();
      if (!(tileId in loadedTileIds)) {
        pendingTileId = tileId;
        break;
      }
    }
    if (pendingTileId) {
      http.getJsonResource(getTileUrl(pendingTileId), function (res, err) {
          if (!err || err.type === "clientError") {
            loadedTileIds[pendingTileId] = true;
            res = res || {};
            var roadLinks = [];
            if (res.roadLinks) {
              for (var i = 0; i < res.roadLinks.length; i++) {
                var roadLink = res.roadLinks[i];
                if (roadLink.ps.length > 1) {
                  roadLinks.push({
                      toid:   roadLink.toid,
                      length: roadLink.length,
                      ps:     simplify(roadLink.ps, 1)
                    });
                }
              }
            }
            postMessage({
                message:  "tileLoaded",
                tileId:   pendingTileId,
                tileData: {
                  roadLinks: roadLinks,
                  roadNodes: res.roadNodes || []
                }
              });
          }
          pendingTileId = null;
          loadNextTile();
        });
    }
  }
}

onmessage = function (event) {
  switch (event.data.message) {
    case "setOrigin":
      origin = event.data.origin;
      break;
    case "queueTilesToLoad":
      queueTilesToLoad(event.data.tileIds);
      break;
    case "loadNextTile":
      loadNextTile();
      break;
  }
};