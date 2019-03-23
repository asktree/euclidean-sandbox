open Primitives;
open Euclidean;


type svg = string;

type ghost = Ghost(primitive);
type ghostWorld = list(ghost);

let rec check_duplicate = (g, w) => {
    switch (w) {
    | [] => false
    | [Ghost(h), ...k] =>
        let Ghost(pr) = g;
        switch (Euclidean.epsilon_identical(pr, h)) {
        | true => true
        | false => check_duplicate(g, k)
        };
    };
};

let append_ghost = (g, w) => {
    [g, ...w]
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


let rec ghosts_within = (~qualified = [], pt : point, r : float, w : ghostWorld) => {
    switch w {
        | [] => qualified
        | [g, ...remaining] => {
            let Ghost(pr) = g;
            let (dst, pt2) = Euclidean.nearest_point_on_primitive(pt, pr);

            if (dst <= r) {
                ghosts_within(~qualified = [(pt2, g), ...qualified], pt, r, remaining)
            } else {
                ghosts_within(~qualified = qualified, pt, r, remaining)
            }
        }
    };
};

let find_ghost_intersections = (Ghost(pr1), Ghost(pr2)) => find_intersections(pr1, pr2);


let rec list_intersections = (ghosts : list(ghost)) => {
    switch (ghosts) {
    | [] => []
    | [g, ...k] => 
        let f = find_ghost_intersections(g);
        List.flatten(List.map(f, k)) @ list_intersections(k)
    };
};

let rec epsilon_counter_increment = (p:point, counter:list((point, int))) => {
    switch (counter) {
    | [(p2, c), ...k] => 
        switch (Euclidean.epsilon_identical(Point(p), Point(p2))) {
        | true => [(p2, c+1), ...k]
        | false => [(p2, c), ...epsilon_counter_increment(p, k)]
        };
    | [] => [(p, 1)]
    };
};

let rec count_intersection_repeats = (~counter=[], intersections : list(point)) => {
    switch (intersections) {
    | [] => counter
    | [p, ...k] => 
        let counter' = epsilon_counter_increment(p, counter);
        count_intersection_repeats(~counter=counter', k)
    };
};

let rec find_biggest_intersections = (~candidates=[], counted:list((point, int))) => {
    switch (counted) {
    | [] => 
        let (pts, _) = List.split(candidates);
        pts
    | [(pt, c), ...k] =>
        switch (candidates) {
        | [] =>
            let candidates' = [(pt, c)];
            find_biggest_intersections(~candidates=candidates', k)
        | [(p2, c2), ...k] =>
            switch (c == c2) {
            | true => 
                let candidates' = [(pt, c), ...candidates];
                find_biggest_intersections(~candidates=candidates', k)
            | false =>
                switch (c > c2){
                | true => 
                    let candidates' = [(pt, c)];
                    find_biggest_intersections(~candidates=candidates', k)
                | false => 
                    find_biggest_intersections(~candidates=candidates, k)
                }
            }
        }
    }
}

let rec find_nearest_point = (~nearest=?, pt:point, pts:list(point)) => {
    switch pts {
    | [] => 
        switch (nearest) {
        | None => None
        | Some((_, pt2)) => Some(pt2)
        }
    | [pt2, ...k] => 
        let d2 = Euclidean.distance(pt, pt2);
        let candidate = (d2, pt2);
        switch nearest {
        | None => 
            find_nearest_point(~nearest=candidate, pt, k)
        | Some(incumbent) =>
            let nearest' = max(candidate, incumbent);
            find_nearest_point(~nearest=nearest', pt, k)
        };
    };
};

let snap_cursor = (pt : point, r : float, w : ghostWorld) => {
    let (nearby_pts, nearby_ghosts) = List.split(ghosts_within(pt, r, w));
    let intersections = list_intersections(nearby_ghosts);
    switch intersections {
    | [] => 
        switch (find_nearest_point(pt, nearby_pts)) {
        | None => pt
        | Some(pt') => pt'
        }
    | some =>
        let counted = count_intersection_repeats(some);
        let biggest = find_biggest_intersections(counted);
        let best = find_nearest_point(pt, biggest);
        switch best {
        | None => failwith("something is wrong with snap_cursor");
        | Some(pt') => pt'
        }
    }
};