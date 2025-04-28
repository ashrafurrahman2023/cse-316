from scipy.spatial.distance import euclidean


def count_inversions_slow(arr):
    n = len(arr)
    count = 0
    for i in range(n):
        for j in range(i + 1, n):
            if arr[i] > arr[j]:
                count += 1
    return count





class Board:
    def __init__(self, k, input_grid):
        self.k = k
        self.n = k * k - 1
        self.grid = [[input_grid[i][j] for j in range(k)] for i in range(k)]
        self.num_to_row_col_map = {}

        for i in range(k):
            for j in range(k):
                self.num_to_row_col_map[self.grid[i][j]] = (i, j)

    def get_row_col(self, num):
        return self.num_to_row_col_map.get(num, (-1, -1))

    def get_inv_count(self):
        a = []
        for i in range(self.k):
            for j in range(self.k):
                val = self.grid[i][j]
                if val != 0:
                    a.append(val)
        return count_inversions_slow(a)   # ok.....

    def __hash__(self):
        return hash(tuple(tuple(row) for row in self.grid))

    def __eq__(self, other):
        if not isinstance(other, Board):
            return False
        return self.k == other.k and self.grid == other.grid

    def is_valid(self, row, col):
        return 0 <= row < self.k and 0 <= col < self.k

    def copy_own_grid(self):
        return [row[:] for row in self.grid]

    def get_neighbor_boards(self, parent_board=None):
        neighbor_boards = []
        blank_row, blank_col = self.get_row_col(0)

        moves = [
            (1, 0),   # DOWN
            (-1, 0),  # UP
            (0, 1),   # RIGHT
            (0, -1)   # LEFT
        ]

        for move in moves:
            new_row = blank_row + move[0]
            new_col = blank_col + move[1]

            if self.is_valid(new_row, new_col):
                new_grid = self.copy_own_grid()
                new_grid[blank_row][blank_col]=new_grid[new_row][new_col]
                new_grid[new_row][new_col]=0

                new_board = Board(self.k, new_grid)

                if parent_board is None or new_board != parent_board:
                    neighbor_boards.append(new_board)

        return neighbor_boards

    # def __str__(self):
    #     lines = []
    #     for row in self.grid:
    #         line = ' '.join(" * " if val == 0 else f"{val:2d} " for val in row)
    #         lines.append(line)
    #     return '\n'.join(lines)

    def __str__(self):
        result = ""
        for row in self.grid:
            for cell in row:
                if cell == 0:
                    result += " * "
                else:
                    result += f"{cell:2d} "
            result += "\n"
        return result



from abc import ABC, abstractmethod

class Heuristic(ABC):
    def __init__(self):
        self.name = "Unnamed"

    @abstractmethod
    def calculate(self, current_board, target_board):
        pass




class Hamming(Heuristic):
    def __init__(self):
        super().__init__()
        self.name = "Hamming"

    def calculate(self, current_board, target_board):
        d = 0
        for i in range(current_board.k):
            for j in range(current_board.k):
                num = current_board.grid[i][j]

                if num == 0:
                    continue

                if num != target_board.grid[i][j]:
                    d += 1
        return d

class Manhattan(Heuristic):
    def __init__(self):
        super().__init__()
        self.name = "Manhattan"

    def calculate(self, current_board, target_board):
        d = 0
        for i in range(current_board.k):
            for j in range(current_board.k):
                num = current_board.grid[i][j]
                if num == 0:
                    continue
                expected_row, expected_col = target_board.get_row_col(num)
                d += abs(expected_row - i) + abs(expected_col - j)
        return d

import math

class Euclidean(Heuristic):
    def __init__(self):
        super().__init__()
        self.name = "Euclidean"

    def calculate(self, current_board, target_board):
        d = 0
        for i in range(current_board.k):
            for j in range(current_board.k):
                num = current_board.grid[i][j]
                if num == 0:
                    continue
                expected_row, expected_col = target_board.get_row_col(num)
                dx = expected_row - i
                dy = expected_col - j
                d += math.sqrt(dx * dx + dy * dy)
        return d

class LinearConflict(Heuristic):
    def __init__(self):
        super().__init__()
        self.name = "Linear Conflict"

    def calculate(self, current_board, target_board):
        manhattan = 0
        linear_conflict = 0
        k = current_board.k

        for i in range(k):
            for j in range(k):
                num = current_board.grid[i][j]
                if num == 0:
                    continue
                expected_row, expected_col = target_board.get_row_col(num)
                manhattan += abs(expected_row - i) + abs(expected_col - j)

                # Check for linear conflict in the row
                if i == expected_row:
                    for col in range(j + 1, k):
                        next_num = current_board.grid[i][col]
                        if next_num == 0:
                            continue
                        next_expected_row, next_expected_col = target_board.get_row_col(next_num)
                        if next_expected_row == i and expected_col > next_expected_col:
                            linear_conflict += 1

                # Check for linear conflict in the column
                if j == expected_col:
                    for row in range(i + 1, k):
                        next_num = current_board.grid[row][j]
                        if next_num == 0:
                            continue
                        next_expected_row, next_expected_col = target_board.get_row_col(next_num)
                        if next_expected_col == j and expected_row > next_expected_row:
                            linear_conflict += 1

        return manhattan + 2 * linear_conflict


from typing import List, Optional

class State:
    def __init__(self, board: Board, cost_so_far: float, parent_state: Optional['State']):
        self.board = board
        self.cost_so_far = cost_so_far
        self.parent_state = parent_state

    def get_priority(self, heuristic: Heuristic, target_board: Board) -> float:
        return self.cost_so_far + heuristic.calculate(self.board, target_board)

    def get_neighbor_states(self) -> List['State']:
        neighbor_states = []
        parent_board = self.parent_state.board if self.parent_state else None
        neighbor_boards = self.board.get_neighbor_boards(parent_board)
        for b in neighbor_boards:
            neighbor_states.append(State(b, self.cost_so_far + 1, self))
        return neighbor_states

    def get_ancestor_states(self) -> List['State']:
        ancestor_states = []
        s = self.parent_state
        while s is not None:
            ancestor_states.append(s)
            s = s.parent_state
        return ancestor_states



class Puzzle:
    def __init__(self, input_board: Board, target_board: Board = None):
        self.input_board = input_board

        if target_board:
            self.target_board = target_board
        else:
            k = input_board.k
            a = [[0]*k for _ in range(k)]
            c = 1
            for i in range(k):
                for j in range(k):
                    if c == k * k:
                        a[i][j] = 0
                    else:
                        a[i][j] = c
                    c += 1
            self.target_board = Board(k, a)

    def is_solvable(self) -> bool:
        input_inv_count = self.input_board.get_inv_count()
        target_inv_count = self.target_board.get_inv_count()

        k = self.input_board.k

        if k % 2 != 0:  # odd k
            input_invariant = input_inv_count % 2
            target_invariant = target_inv_count % 2
        else:  # even k
            input_blank_row_from_bottom = k - self.input_board.get_row_col(0)[0]
            target_blank_row_from_bottom = k - self.target_board.get_row_col(0)[0]

            input_invariant = (input_inv_count + input_blank_row_from_bottom) % 2
            target_invariant = (target_inv_count + target_blank_row_from_bottom) % 2

        return input_invariant == target_invariant




from queue import PriorityQueue

class Solver:
    def __init__(self, heuristic: Heuristic):
        self.heuristic = heuristic

    def solve(self, puzzle: Puzzle):
        print(f"{self.heuristic.name} Heuristic:")
        print("---------------------------------\n")

        use_closed_map = True
        closed_list = {}
        count=0

        # open list: priority queue
        open_list = PriorityQueue()
        initial_state = State(puzzle.input_board, 0, None)
        open_list.put((initial_state.get_priority(self.heuristic, puzzle.target_board),count, initial_state))
        count=count+1

        if use_closed_map:
            closed_list[initial_state.board] = True

        explored = 1
        expanded = 0
        final_state = None

        while not open_list.empty():
            _,_, current_state = open_list.get()
            closed_list[current_state.board] = True
            expanded += 1

            if current_state.board == puzzle.target_board:
                final_state = current_state
                break

            for neighbor in current_state.get_neighbor_states():
                if use_closed_map:
                    if neighbor.board not in closed_list:
                        open_list.put((neighbor.get_priority(self.heuristic, puzzle.target_board),count, neighbor))
                        explored += 1
                        count=count+1
                else:
                    open_list.put((neighbor.get_priority(self.heuristic, puzzle.target_board),count, neighbor))
                    explored += 1
                    count=count+1

        if final_state:
            print("\nSteps:\n")
            solution_path = final_state.get_ancestor_states()
            solution_path.reverse()
            solution_path.append(final_state)

            for state in solution_path:
                print(state.board)
                print()

            print(f"Number of Explored Nodes = {explored}")
            print(f"Number of Expanded Nodes = {expanded}")
            print(f"Number of steps needed = {len(solution_path) - 1}\n\n")






INVALID_INDICATOR = -1

def take_input():
    k = int(input("Enter the value of k: "))
    if k <= 0:
        print("Sorry! The value of k must be positive.")
        return None

    n = (k ** 2) - 1
    input_board = [[0] * k for _ in range(k)]

    errors = []

    print("Input the initial board: ")
    for i in range(k):
        row_input = input(f"Enter row {i+1}: ").strip()
        row_values = row_input.split()  # Split the space-separated values

        if len(row_values) != k:
            errors.append(f"Row {i+1} should have exactly {k} values.")
            continue

        for j in range(k):
            s = row_values[j]
            if s == "*":
                input_board[i][j] = 0
            elif s.isdigit():
                num = int(s)
                if num > n:
                    errors.append(f"Out of range number inputted at row {i}, column {j}")
                input_board[i][j] = num
            else:
                input_board[i][j] = INVALID_INDICATOR
                errors.append(f"Invalid string inputted at row {i}, column {j}")

    arr = [input_board[i][j] for i in range(k) for j in range(k)]
    arr.sort()

    board_error = False
    for i in range(k * k):
        if arr[i] != i:
            board_error = True
            break

    if errors:
        print("Sorry! Invalid Input.")
        for e in errors:
            print(e)
        return None

    if board_error:
        print("Sorry! The numbers do not form a valid board.")
        return None

    return Board(k, input_board)


def main():
    input_board = take_input()

    if input_board is None:
        return

    puzzle = Puzzle(input_board)

    if puzzle.is_solvable():
        print("\nThe puzzle is solvable.\n")
    else:
        print("\nSorry! The puzzle is not solvable.\n")
        return

    manhattan_solver = Solver(Manhattan())
    manhattan_solver.solve(puzzle)

    hamming_solver = Solver(Hamming())
    hamming_solver.solve(puzzle)

    euclidean_solver= Solver(Euclidean())
    euclidean_solver.solve(puzzle)

    lc_solver=Solver(LinearConflict())
    lc_solver.solve(puzzle)



if __name__ == "__main__":
    main()







