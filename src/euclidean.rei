open Primitives

let epsilon: float;
let sqr: float => float;
let dot: (point, point) => float;
let magnitude: point => float;
let perpify: point => (float, float);
let ( +^ ): (point, point) => (float, float);
let ( -^ ): (point, point) => (float, float);
let ( *^ ): (float, point) => (float, float);
let ( /^ ): (point, float) => (float, float);
let project: (point, point) => (float, float);
let is_identical:
  (primitive, primitive) => bool;

let circle_circle_intersections: (circle, circle) => list((float, float));
let circle_line_intersections: (circle, line) => list((float, float));
let circle_point_intersections: (circle, point) => list(point);
let line_line_intersections: (line, line) => list((float, float));
let line_point_intersections:  (line, point) => list((float, float));
let find_intersections: primitive => primitive => list(point);

let nearest_point_on_circle: (point, circle) => (float, (float, float));
let nearest_point_on_line: (point, line) => (float, (float, float));
let nearest_point_on_primitive: (point, primitive) => (float, point);
