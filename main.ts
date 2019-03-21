// See the console window at bottom.
console.log('Hello Wizzeh');
alert('hi sock')

type Point = [number, number];

type GhostWorld = Ghost[]

interface Ghost {
    draw();
    check_intersections(other: GhostLine): Point[];
    check_intersections(other: GhostCircle): Point[];
    check_intersections(other: GhostPoint): Point[];
    append(ghosts: GhostWorld): GhostWorld;
    nearest_point(point: Point): [Point, number];
    is_moused_over: boolean;
}

class GhostLine implements Ghost {
    draw() {

    }
    check_intersections(other: GhostLine) {
        let a: Point = [1,2];
        let b: Point = [3,4];
        return [a, b];
    }
    append(ghosts: GhostWorld){
        return ghosts;
    }
    nearest_point(point: Point) : [Point, number] {
        let nearest : Point = [1,2];
        let distance : number = 4;
        return [nearest, distance];
    }

}
class GhostCircle implements Ghost {}
class GhostPoint implements Ghost {}





function start_circle(center){
}

function start_line(origin){
}

function add_circle(center, outer){
    // adds a ghost circle
    // center = center point (x, y)
    // outer = outer point (x, y)
}

function add_line(origin, end) {
    // :) 
    make_line()
}

function add_node(point){
    // adds a point ghost object at (x, y). mostly not useful.
}

function add_ghost(ghost, ghosts){
    if (is_duplicate(ghost, ghosts)){
    console.log("duplicate")
    return ghosts;
    }
    let intersections = check_intersections(ghost, ghosts);
}

function is_duplicate(ghost, ghosts) {
    // ghost: a ghost line or circle
    // world: array of all other ghost objects
    // returns: true if an identical ghost object already exists
    return false
}

function check_intersections(ghost, ghosts) {
    // ghost: a ghost line or circle
    // world: array of all other ghost objects
    // returns: array of (x,y) points for each intersection of ghost with all other objects
}

function snap_cursor(cursor, world, edge_snap_range = 3, intersection_snap_range = 5, point_snap_range = 7) {
    // cursor: (x, y)
    // world: array of all ghost objects
    // returns: (x, y) of "snapped" cursor, and snapped-to object
}


