#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt

def mandelbrot(c, max_iter):
    z = 0
    n = 0
    while abs(z) <= 2 and n < max_iter:
        z = z*z + c
        n += 1
    if n == max_iter:
        return 0
    else:
        return n

def create_fractal(min_x, max_x, min_y, max_y, width, height, max_iter):
    x = np.linspace(min_x, max_x, width)
    y = np.linspace(min_y, max_y, height)
    image = np.zeros((height, width))
    for i in range(height):
        for j in range(width):
            c = x[j] + y[i]*1j
            image[i, j] = mandelbrot(c, max_iter)
    return image

mnx = input("Enter minimum x: ")
mxx = input("Enter maximum x: ")
mny = input("Enter minimum y: ")
mxy = input("Enter maximum y: ")
wdt = input("Enter width: ")
hgt = input("Enter height: ")
mit = input("Enter maximum iterations: ")

img = create_fractal(float(mnx), float(mxx), float(mny), float(mxy), int(wdt), int(hgt), int(mit))
plt.imshow(img, cmap='hot')
plt.axis('off')
plt.show()

# mnx:-2, mxx:1, mny:-1, mxy:1, wdt:500, hgt:500, mit:100
