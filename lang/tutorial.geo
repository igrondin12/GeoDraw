canvas(300, 300, (204, 255, 255))
brush myBrush = [(0,0), (2.5,2.5), (5,5)] 
draw(y = 150, [x > 10, x < 290], (0,0,0), 'myBrush')
gridlines(50)