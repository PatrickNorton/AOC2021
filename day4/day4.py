def load_inputs(input_line):
    return [int(i) for i in input_line.split(",")]


def load_boards(lines):
    boards = []
    current_board = []
    for line in lines:
        if not line.strip():
            if current_board:
                boards.append(current_board)
                current_board = []
        else:
            current_board.append(
                [int(i) for i in line.strip().split(" ") if i.strip()]
            )
    return boards


def load_file(file):
    with open(file) as f:
        inputs = load_inputs(f.readline())
        boards = load_boards(f.readlines())
        return (inputs, boards)


def get_location(board, value):
    for i, row in enumerate(board):
        try:
            return (i, row.index(value))
        except ValueError:
            pass
    return None


def is_winner(board):
    for row in board:
        if all(x is None for x in row):
            return True
    for i, _ in enumerate(board[0]):
        if all(row[i] is None for row in board):
            return True
    return False


def score(board, value):
    print(board)
    print(value)
    total = 0
    for row in board:
        total += sum(i for i in row if i is not None)
    return total * value


def calc_winner(inputs, boards):
    for value in inputs:
        for board in boards:
            location = get_location(board, value)
            if location is not None:
                board[location[0]][location[1]] = None
                if is_winner(board):
                    return score(board, value)


def calc_loser(inputs, boards):
    new_boards = boards.copy()
    for value in inputs:
        for board in boards:
            if board not in new_boards:
                continue
            location = get_location(board, value)
            if location is not None:
                board[location[0]][location[1]] = None
                if is_winner(board):
                    print(score(board, value))
                    if len(new_boards) == 1:
                        return score(board, value)
                    new_boards.remove(board)


print(calc_loser(*load_file("./input.txt")))
