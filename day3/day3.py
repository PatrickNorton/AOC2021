def convert_bits(bits):
    result = 0
    for i, bit in enumerate(bits):
        result |= bit << i
    return result


def diagnostic_report(file):
    with open(file) as f:
        total_lines = 0
        sums = [0] * 12
        for line in f.readlines():
            line = line.strip()
            for i, bit in enumerate(line):
                if bit == '1':
                    sums[i] += 1
            total_lines += 1
        most_common = [int(x > total_lines // 2) for x in sums]
        least_common = [int(x <= total_lines // 2) for x in sums]
        return convert_bits(most_common[::-1]) * convert_bits(least_common[::-1])


def oxygen_rating(lines):
    for bit in range(12):
        if sum(line[bit] == '1' for line in lines) >= len(lines) / 2:
            lines = [line for line in lines if line[bit] == '1']
        else:
            lines = [line for line in lines if line[bit] == '0']
        if len(lines) == 1:
            return int(lines[0], 2)
    print(f"Lines has {len(lines)} elements!")
    return int(lines[0], 2)


def co2_rating(lines):
    for bit in range(12):
        if sum(line[bit] == '1' for line in lines) >= len(lines) / 2:
            lines = [line for line in lines if line[bit] == '0']
        else:
            lines = [line for line in lines if line[bit] == '1']
        if len(lines) == 1:
            return int(lines[0], 2)
    print(f"Lines has {len(lines)} elements!")
    return int(lines[0], 2)


def life_support(file):
    with open(file) as f:
        lines = list(line.strip() for line in f.readlines())
        return oxygen_rating(lines) * co2_rating(lines)


print(diagnostic_report("./input.txt"))
print(life_support("./input.txt"))
