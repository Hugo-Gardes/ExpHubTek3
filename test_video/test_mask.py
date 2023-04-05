import tkinter as tk
from PIL import Image, ImageTk
import cv2
import numpy as np

window = tk.Tk()
window.title("Webcam")
cap = cv2.VideoCapture(0)
def update_video():
    ret, frame = cap.read()
    img = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    img = Image.fromarray(img)
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    ret, thresh = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    contours, hierarchy = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    mask = np.zeros_like(gray)
    cv2.drawContours(mask, contours, -1, 255, -1)
    mask = cv2.dilate(mask, None, iterations=10)
    mask = cv2.erode(mask, None, iterations=10)
    mask = cv2.GaussianBlur(mask, (0,0), 10)
    mask = cv2.merge((mask,mask,mask))
    background = np.zeros_like(frame)
    background[:] = (0, 255, 0)
    frame = np.where(mask==255, frame, background)
    img = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    img = Image.fromarray(img)
    imgtk = ImageTk.PhotoImage(image=img)
    label_video.imgtk = imgtk
    label_video.configure(image=imgtk)
    window.after(10, update_video)

label_video = tk.Label(window)
label_video.pack()
update_video()
window.mainloop()
cap.release()
