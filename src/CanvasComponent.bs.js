// Generated by BUCKLESCRIPT VERSION 5.0.0, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Main$ReactTemplate = require("./Main.bs.js");
var Euclidean$ReactTemplate = require("./Euclidean.bs.js");

function stringify_primitive(pr) {
  switch (pr.tag | 0) {
    case 0 : 
        var match = pr[0];
        var p2 = match[1];
        var p1 = match[0];
        return /* StringLine */Block.__(0, [/* tuple */[
                    /* tuple */[
                      p1[0].toString(),
                      p1[1].toString()
                    ],
                    /* tuple */[
                      p2[0].toString(),
                      p2[1].toString()
                    ]
                  ]]);
    case 1 : 
        var match$1 = pr[0];
        var pt = match$1[0];
        return /* StringCircle */Block.__(1, [/* tuple */[
                    /* tuple */[
                      pt[0].toString(),
                      pt[1].toString()
                    ],
                    match$1[1].toString()
                  ]]);
    case 2 : 
        var pt$1 = pr[0];
        return /* StringPoint */Block.__(2, [/* tuple */[
                    pt$1[0].toString(),
                    pt$1[1].toString()
                  ]]);
    
  }
}

function draw_ghost(param) {
  var spr = stringify_primitive(param[0]);
  switch (spr.tag | 0) {
    case 0 : 
        var match = spr[0];
        var p2 = match[1];
        var p1 = match[0];
        return React.createElement("line", {
                    stroke: "black",
                    x1: p1[0],
                    x2: p2[0],
                    y1: p1[1],
                    y2: p2[1]
                  });
    case 1 : 
        var match$1 = spr[0];
        var pt = match$1[0];
        return React.createElement("circle", {
                    cx: pt[0],
                    cy: pt[1],
                    fill: "none",
                    r: match$1[1],
                    stroke: "black"
                  });
    case 2 : 
        var pt$1 = spr[0];
        return React.createElement("circle", {
                    cx: pt$1[0],
                    cy: pt$1[1],
                    fill: "black",
                    r: "2"
                  });
    
  }
}

function draw_world(_$staropt$star, _w) {
  while(true) {
    var w = _w;
    var $staropt$star = _$staropt$star;
    var elems = $staropt$star !== undefined ? $staropt$star : /* [] */0;
    if (w) {
      var next_elem = draw_ghost(w[0]);
      var now_drawn = /* :: */[
        next_elem,
        elems
      ];
      _w = w[1];
      _$staropt$star = now_drawn;
      continue ;
    } else {
      var children = $$Array.of_list(elems);
      return ReactDOMRe.createElementVariadic("g", undefined, children);
    }
  };
}

var component = ReasonReact.reducerComponent("Canvas");

function make(_children) {
  var canvas_mousemove = function (e, self) {
    var dim = ( 'e.getBoundingClientRect();' );
    console.log(dim);
    var pt_000 = e.clientX;
    var pt_001 = e.clientY;
    var pt = /* tuple */[
      pt_000,
      pt_001
    ];
    Curry._1(self[/* send */3], /* MouseMove */Block.__(1, [pt]));
    return /* () */0;
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("div", undefined, React.createElement("svg", {
                              height: "1000",
                              width: "1000",
                              onClick: (function (_event) {
                                  return Curry._1(self[/* send */3], /* ClickCanvas */0);
                                }),
                              onMouseMove: Curry._1(self[/* handle */0], canvas_mousemove)
                            }, React.createElement("circle", {
                                  cx: self[/* state */1][/* tool_pos */4][0].toString(),
                                  cy: self[/* state */1][/* tool_pos */4][1].toString(),
                                  fill: "red",
                                  r: "3",
                                  stroke: "red"
                                }), draw_world(undefined, self[/* state */1][/* ghosts */0])));
            }),
          /* initialState */(function (param) {
              return /* record */[
                      /* ghosts : [] */0,
                      /* selected_tool : CircleTool */0,
                      /* tool_firstclick */undefined,
                      /* hovered_ghost */undefined,
                      /* tool_pos : tuple */[
                        0,
                        0
                      ]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                var pt = state[/* tool_pos */4];
                var pt$prime = Main$ReactTemplate.snap_cursor(pt, 20, state[/* ghosts */0]);
                var match = state[/* selected_tool */1];
                switch (match) {
                  case 0 : 
                      var match$1 = state[/* tool_firstclick */2];
                      if (match$1 !== undefined) {
                        var pt2 = match$1;
                        var r = Euclidean$ReactTemplate.distance(pt$prime, pt2);
                        var new_ghost = /* Ghost */[/* Circle */Block.__(1, [/* tuple */[
                                pt2,
                                r
                              ]])];
                        var new_ghosts = Main$ReactTemplate.append_ghost(new_ghost, state[/* ghosts */0]);
                        return /* Update */Block.__(0, [/* record */[
                                    /* ghosts */new_ghosts,
                                    /* selected_tool */state[/* selected_tool */1],
                                    /* tool_firstclick */undefined,
                                    /* hovered_ghost */state[/* hovered_ghost */3],
                                    /* tool_pos */state[/* tool_pos */4]
                                  ]]);
                      } else {
                        return /* Update */Block.__(0, [/* record */[
                                    /* ghosts */state[/* ghosts */0],
                                    /* selected_tool */state[/* selected_tool */1],
                                    /* tool_firstclick */pt$prime,
                                    /* hovered_ghost */state[/* hovered_ghost */3],
                                    /* tool_pos */state[/* tool_pos */4]
                                  ]]);
                      }
                  case 1 : 
                      var match$2 = state[/* tool_firstclick */2];
                      if (match$2 !== undefined) {
                        var new_ghost$1 = /* Ghost */[/* Line */Block.__(0, [/* tuple */[
                                match$2,
                                pt$prime
                              ]])];
                        var new_ghosts$1 = Main$ReactTemplate.append_ghost(new_ghost$1, state[/* ghosts */0]);
                        return /* Update */Block.__(0, [/* record */[
                                    /* ghosts */new_ghosts$1,
                                    /* selected_tool */state[/* selected_tool */1],
                                    /* tool_firstclick */undefined,
                                    /* hovered_ghost */state[/* hovered_ghost */3],
                                    /* tool_pos */state[/* tool_pos */4]
                                  ]]);
                      } else {
                        return /* Update */Block.__(0, [/* record */[
                                    /* ghosts */state[/* ghosts */0],
                                    /* selected_tool */state[/* selected_tool */1],
                                    /* tool_firstclick */pt$prime,
                                    /* hovered_ghost */state[/* hovered_ghost */3],
                                    /* tool_pos */state[/* tool_pos */4]
                                  ]]);
                      }
                  case 2 : 
                      var new_ghost$2 = /* Ghost */[/* Point */Block.__(2, [pt$prime])];
                      var new_ghosts$2 = Main$ReactTemplate.append_ghost(new_ghost$2, state[/* ghosts */0]);
                      return /* Update */Block.__(0, [/* record */[
                                  /* ghosts */new_ghosts$2,
                                  /* selected_tool */state[/* selected_tool */1],
                                  /* tool_firstclick */undefined,
                                  /* hovered_ghost */state[/* hovered_ghost */3],
                                  /* tool_pos */state[/* tool_pos */4]
                                ]]);
                  
                }
              } else if (action.tag) {
                var ipt = action[0];
                var pt_000 = ipt[0];
                var pt_001 = ipt[1];
                var pt$1 = /* tuple */[
                  pt_000,
                  pt_001
                ];
                var pt$prime$1 = Main$ReactTemplate.snap_cursor(pt$1, 10, state[/* ghosts */0]);
                return /* Update */Block.__(0, [/* record */[
                            /* ghosts */state[/* ghosts */0],
                            /* selected_tool */state[/* selected_tool */1],
                            /* tool_firstclick */state[/* tool_firstclick */2],
                            /* hovered_ghost */state[/* hovered_ghost */3],
                            /* tool_pos */pt$prime$1
                          ]]);
              } else {
                return /* Update */Block.__(0, [/* record */[
                            /* ghosts */state[/* ghosts */0],
                            /* selected_tool */action[0],
                            /* tool_firstclick */undefined,
                            /* hovered_ghost */state[/* hovered_ghost */3],
                            /* tool_pos */state[/* tool_pos */4]
                          ]]);
              }
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.stringify_primitive = stringify_primitive;
exports.draw_ghost = draw_ghost;
exports.draw_world = draw_world;
exports.component = component;
exports.make = make;
/* component Not a pure module */
