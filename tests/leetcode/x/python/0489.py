import sys


class Robot:
    DR = (-1, 0, 1, 0)
    DC = (0, 1, 0, -1)

    def __init__(self, grid: list[str], sr: int, sc: int) -> None:
        self.grid = grid
        self.r = sr
        self.c = sc
        self.dir = 0
        self.cleaned: set[tuple[int, int]] = set()

    def move(self) -> bool:
        nr = self.r + self.DR[self.dir]
        nc = self.c + self.DC[self.dir]
        if 0 <= nr < len(self.grid) and 0 <= nc < len(self.grid[0]) and self.grid[nr][nc] == "1":
            self.r, self.c = nr, nc
            return True
        return False

    def turn_right(self) -> None:
        self.dir = (self.dir + 1) % 4

    def clean(self) -> None:
        self.cleaned.add((self.r, self.c))


def go_back(robot: Robot) -> None:
    robot.turn_right()
    robot.turn_right()
    robot.move()
    robot.turn_right()
    robot.turn_right()


def dfs(robot: Robot, x: int, y: int, direction: int, seen: set[tuple[int, int]]) -> None:
    seen.add((x, y))
    robot.clean()
    for i in range(4):
        nd = (direction + i) % 4
        nx = x + Robot.DR[nd]
        ny = y + Robot.DC[nd]
        if (nx, ny) not in seen and robot.move():
            dfs(robot, nx, ny, nd, seen)
            go_back(robot)
        robot.turn_right()


def solve(grid: list[str], sr: int, sc: int) -> int:
    robot = Robot(grid, sr, sc)
    dfs(robot, 0, 0, 0, set())
    return len(robot.cleaned)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        m = int(data[idx]); idx += 1
        sr = int(data[idx]); idx += 1
        sc = int(data[idx]); idx += 1
        grid = data[idx:idx + n]
        idx += n
        out.append(str(solve(grid, sr, sc)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
