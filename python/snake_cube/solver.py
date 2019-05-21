from collections import namedtuple

Point = namedtuple("Point", "x y z")
State = namedtuple("State", "MAX_X MIN_X MAX_Y MIN_Y MAX_Z MIN_Z")


def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)


Orientation = enum('X_PLUS', 'X_NEG', 'Y_PLUS', 'Y_NEG', 'Z_PLUS', 'Z_NEG')


def check(state):
    if (state.MAX_X - state.MIN_X) >= 4:
        return False

    if (state.MAX_Y - state.MIN_Y) >= 4:
        return False

    if (state.MAX_Z - state.MIN_Z) >= 4:
        return False

    return True


def check_points(old_point, new_point, occupied_points):
    if old_point.x < new_point.x:
        for i in range(1, new_point.x - old_point.x + 1):
            p = Point(old_point.x+i, old_point.y, old_point.z)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    if old_point.x > new_point.x:
        for i in range(1, old_point.x - new_point.x + 1):
            p = Point(old_point.x-i, old_point.y, old_point.z)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    if old_point.y < new_point.y:
        for i in range(1, new_point.y - old_point.y + 1):
            p = Point(old_point.x, old_point.y+i, old_point.z)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    if old_point.y > new_point.y:
        for i in range(1, old_point.y - new_point.y + 1):
            p = Point(old_point.x, old_point.y-i, old_point.z)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    if old_point.z < new_point.z:
        for i in range(1, new_point.z - old_point.z + 1):
            p = Point(old_point.x, old_point.y, old_point.z+i)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    if old_point.z > new_point.z:
        for i in range(1, old_point.z - new_point.z + 1):
            p = Point(old_point.x, old_point.y, old_point.z-i)
            if p in occupied_points:
                return False
            else:
                occupied_points.add(p)

    return True


def print_orientation(orientation):
    if orientation == Orientation.X_PLUS:
        print('X_PLUS')
    elif orientation == Orientation.X_NEG:
        print('X_NEG')
    elif orientation == Orientation.Y_PLUS:
        print('Y_PLUS')
    elif orientation == Orientation.Y_NEG:
        print('Y_NEG')
    elif orientation == Orientation.Z_PLUS:
        print('Z_PLUS')
    else:
        print('Z_NEG')

        
def copy_point(point, x_d, y_d, z_d):
    return Point(point.x + x_d, point.y + y_d, point.z + z_d)


def next_orientations(orientation):
    if orientation == Orientation.X_PLUS or orientation == Orientation.X_NEG:
        return [Orientation.Y_PLUS, Orientation.Y_NEG, Orientation.Z_PLUS, Orientation.Z_NEG]
    elif orientation == Orientation.Y_PLUS or orientation == Orientation.Y_NEG:
        return [Orientation.X_PLUS, Orientation.X_NEG, Orientation.Z_PLUS, Orientation.Z_NEG]
    else:
        return [Orientation.X_PLUS, Orientation.X_NEG, Orientation.Y_PLUS, Orientation.Y_NEG]
    

def go(cube, point, idx, orientation, state, occupied_points):
    # check current validity with current orientation
    l = cube[idx] - 1
    if orientation == Orientation.X_PLUS:
        new_point = copy_point(point, l, 0, 0)
        if new_point.x > state.MAX_X:
            state = State(new_point.x, state.MIN_X, state.MAX_Y, state.MIN_Y, state.MAX_Z, state.MIN_Z)
    elif orientation == Orientation.X_NEG:
        new_point = copy_point(point, (-l), 0, 0)
        if new_point.x < state.MIN_X:
            state = State(state.MAX_X, new_point.x, state.MAX_Y, state.MIN_Y, state.MAX_Z, state.MIN_Z)
    elif orientation == Orientation.Y_PLUS:
        new_point = copy_point(point, 0, l, 0)
        if new_point.y > state.MAX_Y:
            state = State(state.MAX_X, state.MIN_X, new_point.y, state.MIN_Y, state.MAX_Z, state.MIN_Z)
    elif orientation == Orientation.Y_NEG:
        new_point = copy_point(point, 0, (-l), 0)
        if new_point.y < state.MIN_Y:
            state = State(state.MAX_X, state.MIN_X, state.MAX_Y, new_point.y, state.MAX_Z, state.MIN_Z)
    elif orientation == Orientation.Z_PLUS:
        new_point = copy_point(point, 0, 0, l)
        if new_point.z > state.MAX_Z:
            state = State(state.MAX_X, state.MIN_X, state.MAX_Y, state.MIN_Y, new_point.z, state.MIN_Z)
    else:
        new_point = copy_point(point, 0, 0, (-l))
        if new_point.z < state.MIN_Z:
            state = State(state.MAX_X, state.MIN_X, state.MAX_Y, state.MIN_Y, state.MAX_Z, new_point.z)

    if not check(state):
        return False

    if not check_points(point, new_point, occupied_points):
        return False

    if idx == (len(cube)-1):
        print(point)
        print_orientation(orientation)
        return True

    for new_orientation in next_orientations(orientation):
        new_occupied_points = set(occupied_points)
        if go(cube, new_point, idx+1, new_orientation, state, new_occupied_points):
            print_orientation(new_orientation)
            return True

    return False
        

def solve():
    cube = [3,2,3,2,2,4,2,3,2,3,2,3,2,2,2,2,2,2,2,2,3,3,2,2,2,2,2,3,4,2,2,2,4,2,3,2,2,2,2,2,2,2,2,2,4,2]
    point = Point(0, 0, 0)
    state = State(0, 0, 0, 0, 0, 0)
    occupied_points = { point }

    for orientation in [Orientation.X_PLUS, Orientation.X_NEG, Orientation.Y_PLUS, Orientation.Y_NEG, Orientation.Z_PLUS, Orientation.Z_NEG]:
        if go(cube, point, 0, orientation, state, occupied_points):
            print(point)
            print_orientation(orientation)
            print("Sovled!!!")
            return


if __name__ == '__main__':
    solve()