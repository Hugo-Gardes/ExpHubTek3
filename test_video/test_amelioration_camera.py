import cv2

cap = cv2.VideoCapture(0)
if not cap.isOpened():
    print("Impossible d'ouvrir la caméra")
    exit()
brightness = 40
contrast = 100
while True:
    ret, frame = cap.read()
    if not ret:
        print("Erreur lors de la lecture de l'image")
        break
    frame = cv2.convertScaleAbs(frame, alpha=contrast/127.0, beta=brightness-contrast)
    cv2.imshow('Camera', frame)
    if cv2.waitKey(1) == ord('q'):
        break
cap.release()
cv2.destroyAllWindows()
