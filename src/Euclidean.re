open Primitives

let epsilon = 0.01;



let sqr = (a: float) => a *. a;
let dot = (a: point, b: point) => (fst(a) *. fst(b)) +. (snd(a) *. snd(b));
let magnitude = (a: point) => sqrt(dot(a, a));
let perpify = (a: point) => (-.fst(a), snd(a)); 
let (+^) = (a: point, b: point) => (fst(a) +. fst(b), snd(a) +. snd(b));
let (-^) = (a: point, b: point) => (fst(a) -. fst(b), snd(a) -. snd(b));
let (*^) = (a: float, b: point) => (a *. fst(b), a *. snd(b));
let (/^) = (b: point, a: float) => (fst(b) /. a, snd(b) /. a)
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
        let perp = sqrt(
            (-.centerDistance +. r2 -. r1) *. 
            (-.centerDistance -. r2 +. r1) *.
            (-.centerDistance +. r2 +. r1) *.
            (centerDistance +. r2 +. r1)
            )/.centerDistance;

        let paraVec = (center2 -^ center1) /^ centerDistance;
        let perpVec = perpify(paraVec);

        [para *^ paraVec +^ perp *^ perpVec +^ center1, para *^ paraVec -^ perp *^ perpVec +^ center1]
    }
};

let circle_line_intersections = (c : circle, l : line) => {
    let (center, radius) = c;
    let center = center -^ fst(l);
    let s = snd(l) -^ fst(l);

    let paraVec = project(center, s) -^ center;

    let para = magnitude(paraVec);

    if (sqr(radius) > sqr(para)) {
        let perp = sqrt(sqr(radius) -. sqr(para));

        let perpVec = perpify(paraVec);

        [paraVec +^ perp *^ perpVec +^ fst(l), paraVec -^ perp *^ perpVec +^ fst(l)]
    } else if (sqr(radius) == sqr(para)) {
        [paraVec +^ fst(l)]
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
    let (dv, dx) = {
        let vx = snd(l1) -^ fst(l1);
        let vy = snd(l2) -^ fst(l2);

        (vy -^ vx, fst(l1) -^ fst(l2))
    };

    let factor = fst(dx) /. fst(dv);

    if (factor *. snd(dv) -. snd(dx) > epsilon) {
        []
    } else {
        [fst(l1) +^ factor *^ (snd(l1) -^ fst(l1))]
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
    let vec = vec /^ magnitude(vec);
    let dst = dot(vec, pt);

    (dst, (dst *^ vec) +^ fst(l));
};

let nearest_point_on_primitive = (pt, pr) =>
    switch pr {
    | Circle(c) => nearest_point_on_circle(pt, c)
    | Line(l) => nearest_point_on_line(pt, l)
    | Point(p) => (magnitude(pt -^ p), p)
    };
