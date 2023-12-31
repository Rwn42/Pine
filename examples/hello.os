main :: fn() {
    add_doubled(1, 2)
}

add_doubled :: fn(x: int, y: int) int {
    return x * 2 + (y * 2)
}