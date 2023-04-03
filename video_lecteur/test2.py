#!/usr/bin/env python3

import cv2

video_name = input("Nom de la vidéo: ")
cap = cv2.VideoCapture(video_name)

while cap.isOpened():
    ret, frame = cap.read()
    if not ret:
        break
    cv2.imshow('Video', frame)
    key = cv2.waitKey(25) & 0xFF
    if key == ord('q'):
        break
    elif key == ord('a'):
        cap.set(cv2.CAP_PROP_POS_FRAMES, cap.get(cv2.CAP_PROP_POS_FRAMES)-10)
    elif key == ord('d'):
        cap.set(cv2.CAP_PROP_POS_FRAMES, cap.get(cv2.CAP_PROP_POS_FRAMES)+10)
    elif key == ord(' '):
        while True:
            key2 = cv2.waitKey(25) & 0xFF
            if key2 == ord(' '):
                break
            elif key2 == ord('q'):
                break
            elif key == ord('a'):
                cap.set(cv2.CAP_PROP_POS_FRAMES, cap.get(
                    cv2.CAP_PROP_POS_FRAMES)-10)
            elif key == ord('d'):
                cap.set(cv2.CAP_PROP_POS_FRAMES, cap.get(
                    cv2.CAP_PROP_POS_FRAMES)+10)

cap.release()
cv2.destroyAllWindows()
