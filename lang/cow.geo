canvas(400, 400)

# cow body
draw(y = (200 + sqrt (((120^2) - ((x - 200)^ 2)) )), [], (0, 0, 0), "thick")
draw(y = (200 - sqrt (((120^2) - ((x - 200)^ 2)) )), [], (0, 0, 0), "thick")

# cow face
draw(y = (225 + sqrt (((50^2) - ((x - 200)^ 2)) )), [], (0, 0, 0), "thick")
draw(y = (225 - sqrt (((50^2) - ((x - 200)^ 2)) )), [x < 180], (0, 0, 0), "thick")
draw(y = (225 - sqrt (((50^2) - ((x - 200)^ 2)) )), [x > 215], (0, 0, 0), "thick")

# cow nose
draw(y = (185 + sqrt (((20^2) - ((x - 200)^ 2)) )), [], (255, 145, 207), "thick")
draw(y = (185 - sqrt (((20^2) - ((x - 200)^ 2)) )), [], (255, 145, 207), "thick")

draw(y = (185 + sqrt (((3^2) - ((x - 193)^ 2)) )), [], (255, 145, 207), "thick")
draw(y = (185 - sqrt (((3^2) - ((x - 193)^ 2)) )), [], (255, 145, 207), "thick")

draw(y = (185 + sqrt (((3^2) - ((x - 207)^ 2)) )), [], (255, 145, 207), "thick")
draw(y = (185 - sqrt (((3^2) - ((x - 207)^ 2)) )), [], (255, 145, 207), "thick")

# cow eye
draw(y = (230 + sqrt (((10^2) - ((x - 182)^ 2)) )), [], (0, 0, 0), "thick")
draw(y = (230 - sqrt (((10^2) - ((x - 182)^ 2)) )), [], (0, 0, 0), "thick")

draw(y = (230 + sqrt (((10^2) - ((x - 218)^ 2)) )), [], (0, 0, 0), "thick")
draw(y = (230 - sqrt (((10^2) - ((x - 218)^ 2)) )), [], (0, 0, 0), "thick")

# cow pupils
draw(y = (230 + sqrt (((3^2) - ((x - 182)^ 2)) )), [], (0, 0, 0), "funky")
draw(y = (230 - sqrt (((3^2) - ((x - 182)^ 2)) )), [], (0, 0, 0), "funky")

draw(y = (230 + sqrt (((3^2) - ((x - 218)^ 2)) )), [], (0, 0, 0), "funky")
draw(y = (230 - sqrt (((3^2) - ((x - 218)^ 2)) )), [], (0, 0, 0), "funky")

# cow left spot
draw(y = (210 + sqrt (((60^2) - ((x - 80)^ 2)) )), [x > 98], (0, 0, 0), "thick")
draw(y = (210 - sqrt (((60^2) - ((x - 80)^ 2)) )), [x > 87], (0, 0, 0), "thick")

# cow right spot
draw(y = (170 + sqrt (((55^2) - ((x - 320)^ 2)) )), [x < 316], (0, 0, 0), "thick")
draw(y = (170 - sqrt (((55^2) - ((x - 320)^ 2)) )), [x < 292], (0, 0, 0), "thick")