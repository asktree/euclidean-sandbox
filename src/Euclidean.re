open Primitives

let epsilon = 0.01;



let sqr = (a: float) => a *. a;
let dot = (a: point, b: point) => (fst(a) *. fst(b)) +. (snd(a) *. snd(b));
let magnitude = (a: point) => sqrt(dot(a, a));
let perpify = (a: point) => (-.snd(a), fst(a)); 
// let perpify_left = (a: point) => (fst(a), -.snd(a));
let (+^) = (a: point, b: point) => (fst(a) +. fst(b), snd(a) +. snd(b));
let (-^) = (a: point, b: point) => (fst(a) -. fst(b), snd(a) -. snd(b));
let (*^) = (a: float, b: point) => (a *. fst(b), a *. snd(b));
let (/^) = (b: point, a: float) => (fst(b) /. a, snd(b) /. a)
let det = (a: float, b: float, c: float, d: float) => (a *. d) -. (b *. c);
let project = (v: point, onto: point) => (dot(v, onto) /. dot(onto, onto)) *^ onto;

let distance = (p1, p2) => magnitude(p1 -^ p2);

let epsilon_identical = (pr1 : primitive, pr2 : primitive) => pr1 == pr2;
/* TODO: add epsilon tolerance */

let circle_circle_intersections = (c1 : circle, c2 : circle) => {
    let (center1, r1) = c1;
    let (center2, r2) = c2;

    let centerDistance = magnitude(center2 -^ center1);

    if (centerDistance > r1 +. r2) {
        []
    } else if (centerDistance == r1 +. r2) {
        [((r1 *^ (center2 -^ center1)) /^ centerDistance) +^ center1]
    } else {
        let para = (sqr(centerDistance) -. sqr(r2) +. sqr(r1))/.(2.0 *. centerDistance);
        let perp = sqrt((4.0 *. sqr(centerDistance) *. sqr(r1)) -. sqr(sqr(centerDistance) -. sqr(r2) +. sqr(r1))) /. (2.0 *. centerDistance);

        let paraVec = (center2 -^ center1) /^ centerDistance;

        let perpVec = perpify(paraVec);

        [(para *^ paraVec) +^ (perp *^ perpVec) +^ center1, (para *^ paraVec) +^ center1 -^ (perp *^ perpVec)]
    }
};

let circle_line_intersections = (c : circle, l : line) => {
    let (center, radius) = c;
    let center = center -^ fst(l);
    let s = snd(l) -^ fst(l);
    //  -^ center
    let paraVec = project(center, s) -^ center;

    let para = magnitude(paraVec);

    if (sqr(radius) > sqr(para)) {
        let perp = sqrt(sqr(radius) -. sqr(para));

        let perpVec = perpify(paraVec) /^ para;

        [paraVec +^ (perp *^ perpVec) +^ fst(l) +^ center, paraVec -^ (perp *^ perpVec) +^ fst(l) +^ center]
    } else if (sqr(radius) == sqr(para)) {

        [paraVec +^ fst(l) +^ center]
    } else {

        []
    }
};

let circle_point_intersections = (c : circle, p : point) => {
    let (center, radius) = c;
    let diff = p -^ center;
    
    if (sqr(magnitude(diff) -. radius) > sqr(epsilon)) {
        []
    } else {
        [p]
    }
};

let line_line_intersections = (l1 : line, l2 : line) => {
    let ((x1, y1), (x2, y2)) = l1;
    let ((x3, y3), (x4, y4)) = l2;

    let x_num = det(det(x1, y1, x2, y2), det(x1, 1.0, x2, 1.0), det(x3, y3, x4, y4), det(x3, 1.0, x4, 1.0));
    let x_den = det(det(x1, 1.0, x2, 1.0), det(y1, 1.0, y2, 1.0), det(x3, 1.0, x4, 1.0), det(y3, 1.0, y4, 1.0));
    let y_num = det(det(x1, y1, x2, y2), det(y1, 1.0, y2, 1.0), det(x3, y3, x4, y4), det(y3, 1.0, y4, 1.0));
    let y_den = det(det(x1, 1.0, x2, 1.0), det(y1, 1.0, y2, 1.0), det(x3, 1.0, x4, 1.0), det(y3, 1.0, y4, 1.0));

    if ((abs_float(y_den) < epsilon) || (abs_float(x_den) < epsilon)) {
        []
    } else {
        [(x_num /. x_den, y_num /. y_den)]
    }
};

let line_point_intersections = (l : line, p: point) => {
    let v = snd(l) -^ fst(l);
    let p = p -^ fst(l);
    let proj = project(v, p);

    if (magnitude(v -^ proj) > epsilon) {
        []
    } else {
        [p +^ fst(l)]
    } 
};

let find_intersections = (pr1, pr2) => {
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
        | Point(p2) => 
            switch (epsilon_identical(Point(p1), Point(p2))) {
            | true => [p2]
            | false => []
            };
        };
    };
};

let nearest_point_on_circle = (pt: point, c: circle) => {
    let vec = pt -^ fst(c);
    let vec = snd(c) *^ vec /^ magnitude(vec);
    let cpt = vec +^ fst(c);
    let dst = magnitude(pt -^ cpt);
    (dst, cpt);
};

let nearest_point_on_line = (pt: point, l: line) => {
    let vec = snd(l) -^ fst(l);
    let pvec = pt -^ fst(l);
    let proj = ((dot(pvec, vec) /. dot(vec, vec)) *^ vec);
    let npt = proj +^ fst(l);
    let dst = magnitude(pt -^ npt);

    (dst, npt);
};

let nearest_point_on_primitive = (pt, pr) =>
    switch pr {
    | Circle(c) => nearest_point_on_circle(pt, c)
    | Line(l) => nearest_point_on_line(pt, l)
    | Point(p) => (magnitude(pt -^ p), p)
    };
