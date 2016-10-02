'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

// Require data json as string to be decoded in elm
var dataRes = require('raw!./data.json');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode, { dataRes: dataRes });