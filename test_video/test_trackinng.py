import cv2
import numpy as np

largeur_dessin, hauteur_dessin = 640, 480
rayon_point = 5
couleur_point = (255, 255, 255)
épaisseur_ligne = 2
couleur_ligne = (255, 255, 255)
capture = cv2.VideoCapture(0)
capture.set(cv2.CAP_PROP_FRAME_WIDTH, largeur_dessin)
capture.set(cv2.CAP_PROP_FRAME_HEIGHT, hauteur_dessin)
dessin = np.zeros((hauteur_dessin, largeur_dessin, 3), np.uint8)
zone_min = 10000
zone_max = 50000
détection_mouvement = False
x1, y1 = None, None

while True:
    ret, image = capture.read()
    if not ret:
        break
    image_gris = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    image_floue = cv2.GaussianBlur(image_gris, (5, 5), 0)
    _, seuil = cv2.threshold(
        image_floue, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    contours, _ = cv2.findContours(
        seuil, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    contour_main = None
    if len(contours) > 0:
        for contour in contours:
            zone = cv2.contourArea(contour)
            if zone_min < zone < zone_max:
                contour_main = contour
                break
    if contour_main is not None:
        x2, y2, w, h = cv2.boundingRect(contour_main)
        cv2.rectangle(image, (x2, y2), (x2 + w, y2 + h), (0, 255, 0), 2)
        if x1 is not None and y1 is not None:
            diff_x = x2 - x1
            diff_y = y2 - y1
            if abs(diff_x) > 5 or abs(diff_y) > 5:
                détection_mouvement = True
        cv2.circle(dessin, (x2, y2), rayon_point, couleur_point, -1)
        if détection_mouvement:
            cv2.line(dessin, (x1, y1), (x2, y2),
                     couleur_ligne, épaisseur_ligne)
        x1, y1 = x2, y2
    else:
        détection_mouvement = False
        x1, y1 = None, None
    dessin_affichage = cv2.addWeighted(image, 0.5, dessin, 0.5, 0)
    cv2.imshow("Dessin à la main", dessin_affichage)
    touche = cv2.waitKey(1)
    if touche == ord('q'):
        break

cv2.destroyAllWindows()
capture.release()