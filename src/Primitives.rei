type point = (float, float);
type line = (point, point);
type circle = (point, float);

type primitive = Line(line) | Circle(circle) | Point(point);