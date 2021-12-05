from collections import Counter
from math import gcd


def straight_line_filter(lines):
    return [line for line in lines
            if line[0][0] == line[1][0] or line[0][1] == line[1][1]]


def get_points_on_line(line):
    points = []
    start, end = line
    delx = end[0] - start[0]
    dely = end[1] - start[1]
    common_denominator = gcd(delx, dely)
    xslope = delx // common_denominator
    yslope = dely // common_denominator
    current = start
    while current != end:
        points.append(current)
        x, y = current
        current = (x + xslope, y + yslope)
    points.append(current)
    return points


def get_points_on_all_lines(lines):
    all_points = []
    for line in lines:
        all_points += get_points_on_line(line)
    return all_points


def find_colliding_points(points):
    return [point for point, count in Counter(points).items() if count > 1]


def parse_input_file():
    with open("./input.txt") as file:
        values = []
        for line in file.readlines():
            raw_start, raw_end = tuple(line.split('->'))
            start = tuple(int(x) for x in raw_start.strip().split(','))
            end = tuple(int(x) for x in raw_end.strip().split(','))
            values.append((start, end))
        return values


def main():
    all_lines = parse_input_file()
    filtered_lines = straight_line_filter(all_lines)

    all_points = get_points_on_all_lines(filtered_lines)
    colliding_points = find_colliding_points(all_points)
    print(f"Part 1: {len(colliding_points)}")

    all_points = get_points_on_all_lines(all_lines)
    colliding_points = find_colliding_points(all_points)
    print(f"Part 2: {len(colliding_points)}")


if __name__ == "__main__":
    main()
