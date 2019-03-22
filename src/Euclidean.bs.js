// Generated by BUCKLESCRIPT VERSION 5.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function sqr(a) {
  return a * a;
}

function dot(a, b) {
  return a[0] * b[0] + a[1] * b[1];
}

function magnitude(a) {
  return Math.sqrt(dot(a, a));
}

function perpify(a) {
  return /* tuple */[
          -a[0],
          a[1]
        ];
}

function $plus$caret(a, b) {
  return /* tuple */[
          a[0] + b[0],
          a[1] + b[1]
        ];
}

function $neg$caret(a, b) {
  return /* tuple */[
          a[0] - b[0],
          a[1] - b[1]
        ];
}

function $star$caret(a, b) {
  return /* tuple */[
          a * b[0],
          a * b[1]
        ];
}

function $slash$caret(b, a) {
  return /* tuple */[
          b[0] / a,
          b[1] / a
        ];
}

function project(v, onto) {
  return $star$caret(dot(v, onto) / dot(onto, onto), onto);
}

var Not_implemented = Caml_exceptions.create("Euclidean-ReactTemplate.Not_implemented");

function is_identical(pr1, pr2) {
  throw Not_implemented;
}

function circle_circle_intersections(c1, c2) {
  var r2 = c2[1];
  var center2 = c2[0];
  var r1 = c1[1];
  var center1 = c1[0];
  var a = $neg$caret(center2, center1);
  var centerDistance = Math.sqrt(dot(a, a));
  if (centerDistance > r1 + r2) {
    return /* [] */0;
  } else if (centerDistance === r1 + r2) {
    return /* :: */[
            $plus$caret($slash$caret($star$caret(r1, $neg$caret(center2, center1)), centerDistance), center1),
            /* [] */0
          ];
  } else {
    var para = (centerDistance * centerDistance - r2 * r2 + r1 * r1) / (2.0 * centerDistance);
    var perp = Math.sqrt((-centerDistance + r2 - r1) * (-centerDistance - r2 + r1) * (-centerDistance + r2 + r1) * (centerDistance + r2 + r1)) / centerDistance;
    var paraVec = $slash$caret($neg$caret(center2, center1), centerDistance);
    var perpVec = perpify(paraVec);
    return /* :: */[
            $plus$caret($plus$caret($star$caret(para, paraVec), $star$caret(perp, perpVec)), center1),
            /* :: */[
              $plus$caret($neg$caret($star$caret(para, paraVec), $star$caret(perp, perpVec)), center1),
              /* [] */0
            ]
          ];
  }
}

function circle_line_intersections(c, l) {
  var radius = c[1];
  var center = $neg$caret(c[0], l[0]);
  var s = $neg$caret(l[1], l[0]);
  var paraVec = $neg$caret(project(center, s), center);
  var para = Math.sqrt(dot(paraVec, paraVec));
  if (radius * radius > para * para) {
    var perp = Math.sqrt(radius * radius - para * para);
    var perpVec = perpify(paraVec);
    return /* :: */[
            $plus$caret($plus$caret(paraVec, $star$caret(perp, perpVec)), l[0]),
            /* :: */[
              $plus$caret($neg$caret(paraVec, $star$caret(perp, perpVec)), l[0]),
              /* [] */0
            ]
          ];
  } else if (radius * radius === para * para) {
    return /* :: */[
            $plus$caret(paraVec, l[0]),
            /* [] */0
          ];
  } else {
    return /* [] */0;
  }
}

function circle_point_intersections(c, p) {
  var diff = $neg$caret(p, c[0]);
  var a = Math.sqrt(dot(diff, diff)) - c[1];
  if (a * a > 0.01 * 0.01) {
    return /* [] */0;
  } else {
    return /* :: */[
            p,
            /* [] */0
          ];
  }
}

function line_line_intersections(l1, l2) {
  var vx = $neg$caret(l1[1], l1[0]);
  var vy = $neg$caret(l2[1], l2[0]);
  var dv = $neg$caret(vy, vx);
  var dx = $neg$caret(l1[0], l2[0]);
  var factor = dx[0] / dv[0];
  if (factor * dv[1] - dx[1] > 0.01) {
    return /* [] */0;
  } else {
    return /* :: */[
            $plus$caret(l1[0], $star$caret(factor, $neg$caret(l1[1], l1[0]))),
            /* [] */0
          ];
  }
}

function line_point_intersections(l, p) {
  var v = $neg$caret(l[1], l[0]);
  var p$1 = $neg$caret(p, l[0]);
  var proj = project(v, p$1);
  var a = $neg$caret(v, proj);
  if (Math.sqrt(dot(a, a)) > 0.01) {
    return /* [] */0;
  } else {
    return /* :: */[
            $plus$caret(p$1, l[0]),
            /* [] */0
          ];
  }
}

function find_intersections(pr1, pr2) {
  switch (pr1.tag | 0) {
    case 0 : 
        var l1 = pr1[0];
        switch (pr2.tag | 0) {
          case 0 : 
              return line_line_intersections(l1, pr2[0]);
          case 1 : 
              return circle_line_intersections(pr2[0], l1);
          case 2 : 
              return line_point_intersections(l1, pr2[0]);
          
        }
    case 1 : 
        var c1 = pr1[0];
        switch (pr2.tag | 0) {
          case 0 : 
              return circle_line_intersections(c1, pr2[0]);
          case 1 : 
              return circle_circle_intersections(c1, pr2[0]);
          case 2 : 
              return circle_point_intersections(c1, pr2[0]);
          
        }
    case 2 : 
        var p1 = pr1[0];
        switch (pr2.tag | 0) {
          case 0 : 
              return line_point_intersections(pr2[0], p1);
          case 1 : 
              return circle_point_intersections(pr2[0], p1);
          case 2 : 
              return is_identical(/* Point */Block.__(2, [p1]), /* Point */Block.__(2, [pr2[0]]));
          
        }
    
  }
}

function nearest_point_on_circle(pt, c) {
  var vec = $neg$caret(pt, c[0]);
  var vec$1 = $slash$caret($star$caret(c[1], vec), Math.sqrt(dot(vec, vec)));
  var cpt = $plus$caret(vec$1, c[0]);
  var a = $neg$caret(pt, cpt);
  var dst = Math.sqrt(dot(a, a));
  return /* tuple */[
          dst,
          cpt
        ];
}

function nearest_point_on_line(pt, l) {
  var vec = $neg$caret(l[1], l[0]);
  var vec$1 = $slash$caret(vec, Math.sqrt(dot(vec, vec)));
  var dst = dot(vec$1, pt);
  return /* tuple */[
          dst,
          $plus$caret($star$caret(dst, vec$1), l[0])
        ];
}

function nearest_point_on_primitive(pt, pr) {
  switch (pr.tag | 0) {
    case 0 : 
        return nearest_point_on_line(pt, pr[0]);
    case 1 : 
        return nearest_point_on_circle(pt, pr[0]);
    case 2 : 
        var p = pr[0];
        var a = $neg$caret(pt, p);
        return /* tuple */[
                Math.sqrt(dot(a, a)),
                p
              ];
    
  }
}

var epsilon = 0.01;

exports.epsilon = epsilon;
exports.sqr = sqr;
exports.dot = dot;
exports.magnitude = magnitude;
exports.perpify = perpify;
exports.$plus$caret = $plus$caret;
exports.$neg$caret = $neg$caret;
exports.$star$caret = $star$caret;
exports.$slash$caret = $slash$caret;
exports.project = project;
exports.is_identical = is_identical;
exports.circle_circle_intersections = circle_circle_intersections;
exports.circle_line_intersections = circle_line_intersections;
exports.circle_point_intersections = circle_point_intersections;
exports.line_line_intersections = line_line_intersections;
exports.line_point_intersections = line_point_intersections;
exports.find_intersections = find_intersections;
exports.nearest_point_on_circle = nearest_point_on_circle;
exports.nearest_point_on_line = nearest_point_on_line;
exports.nearest_point_on_primitive = nearest_point_on_primitive;
/* No side effect */