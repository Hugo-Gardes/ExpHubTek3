#!/usr/bin/env python3

import cv2

video_name = input("Nom de la vidéo: ")

cap = cv2.VideoCapture(video_name)

while cap.isOpened():
    ret, frame = cap.read()
    if not ret:
        break
    cv2.imshow('Video', frame)
    if cv2.waitKey(25) & 0xFF == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
