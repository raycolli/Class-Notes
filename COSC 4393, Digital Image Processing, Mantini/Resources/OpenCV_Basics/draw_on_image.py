import cv2
from create_image import load_display

def draw_rect():
    kenny = cv2.imread("kennysmall.jpg", 0)

    cv2.rectangle(kenny, (90,125),(250,325), (255), thickness=5)
    load_display(kenny)


def draw_ellipse():
    kenny = cv2.imread("kennysmall.jpg", 0)

    cv2.circle(kenny, (170, 200), 150, 255, thickness=3)
    load_display(kenny)

def draw_text():
    kenny = cv2.imread("kennysmall.jpg", 0)
    cv2.rectangle(kenny, (90, 125), (250, 325), (255), thickness=5)
    cv2.putText(kenny, "Kenny", (70, 125), cv2.FONT_HERSHEY_COMPLEX, 2, 255)
    load_display(kenny)

# draw_text()