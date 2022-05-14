canvas(50, 50)
draw(y = (5 * x), [y > 10, x > 0], (255, 0, 0), 'simple')
draw(y = ((-2 * x) + 15), [X < 20, y > 0], (0, 255, 0), "simple")
draw(y = x, [x < 25], (0, 0, 255), "simple")