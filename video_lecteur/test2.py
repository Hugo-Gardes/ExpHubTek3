#!/usr/bin/env python3
import cv2
import tkinter as tk

video_name = input("Nom de la vidéo: ")

class VideoPlayer:
    def __init__(self, video_name):
        self.cap = cv2.VideoCapture(video_name)
        self.paused = False
        self.create_gui()

    def create_gui(self):
        self.root = tk.Tk()
        self.root.title("Video Player")
        self.canvas = tk.Canvas(self.root)
        self.canvas.pack()

        self.btn_prev = tk.Button(self.root, text="Prev", command=self.prev_frame)
        self.btn_prev.pack(side=tk.LEFT)
        self.btn_play = tk.Button(self.root, text="Play", command=self.toggle_pause)
        self.btn_play.pack(side=tk.LEFT)
        self.btn_next = tk.Button(self.root, text="Next", command=self.next_frame)
        self.btn_next.pack(side=tk.LEFT)

        self.root.bind("<Left>", lambda event: self.prev_frame())
        self.root.bind("<Right>", lambda event: self.next_frame())
        self.root.bind("<space>", lambda event: self.toggle_pause())

        self.play_video()

    def play_video(self):
        ret, frame = self.cap.read()
        if not ret:
            return
        if not self.paused:
            self.photo = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
            self.photo = tk.PhotoImage(data=cv2.imencode('.png', self.photo)[1].tobytes())
            self.canvas.create_image(0, 0, image=self.photo, anchor=tk.NW)
        self.root.after(25, self.play_video)

    def toggle_pause(self):
        self.paused = not self.paused

    def prev_frame(self):
        self.cap.set(cv2.CAP_PROP_POS_FRAMES, self.cap.get(cv2.CAP_PROP_POS_FRAMES) - 10)

    def next_frame(self):
        self.cap.set(cv2.CAP_PROP_POS_FRAMES, self.cap.get(cv2.CAP_PROP_POS_FRAMES) + 10)

    def run(self):
        self.root.mainloop()

player = VideoPlayer(video_name)
player.run()
