type point = (float, float);
type svg = string;

type line = (point, point);
type circle = (point, float);

type primitive = Line(line) | Circle(circle) | Point(point);
type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

exception Not_implemented;

let is_identical = (pr1 : primitive, pr2 : primitive) => raise(Not_implemented);

let circle_circle_intersections = (c1 : circle, c2 : circle) => 
    raise(Not_implemented);

let circle_line_intersections = (c : circle, l : line) =>
    raise(Not_implemented);

let circle_point_intersections = (c : circle, p : point) =>
    raise(Not_implemented);

let line_line_intersections = (l1 : line, l2 : line) => 
    raise(Not_implemented);

let line_point_intersections = (l : line, p: point) =>
    raise(Not_implemented);

let find_intersections = (pr1, pr2) => 
    switch pr1 {
    | Circle(c1) => 
        switch pr2 {
        | Circle(c2) => circle_circle_intersections(c1, c2)
        | Line(l2) => circle_line_intersections(c1, l2)
        | Point(p2) => circle_point_intersections(c1, p2)
        }
    | Line(l1) =>
        switch pr2 {
        | Circle(c2) => circle_line_intersections(c2, l1)
        | Line(l2) => line_line_intersections(l1, l2)
        | Point(p2) => line_point_intersections(l1, p2)
        }
    | Point(p1) => 
        switch pr2 {
        | Circle(c2) => circle_point_intersections(c2, p1)
        | Line(l2) => line_point_intersections(l2, p1)
        | Point(p2) => is_identical(Point(p1), Point(p2))
        }
    };

let nearest_point_on_circle = (pt: point, c:circle) => ()
let nearest_point_on_line = (pt: point, l: line) => ()

let nearest_point_on_primitive = (pt, pr) =>
    switch pr {
    | Circle(c) => ()
    | Line(l) => ()
    | Point(p) => (distance(pt, p), p)
    };

let rec nearest_ghost = (~best = ?, pt : point, w : ghostWorld) =>
    switch w {
    | [] => best
    | [g, ...remaining] => 
        let Ghost(pr) = g;
        let (d1, x1) = nearest_point_on_primitive(pt, pr);
        let candidate = (d1, x1, g);
        switch best {
        | None => nearest_ghost(~best = candidate, pt, remaining)
        | Some(incumbent) => 
            let winner = max(candidate, incumbent);
            nearest_ghost(~best = winner, pt, remaining)
        };
    };