import numpy as np
import cv2

def load_display(input_image):
    cv2.namedWindow("Lenna", cv2.WINDOW_AUTOSIZE)
    cv2.imshow("Lenna", input_image)
    cv2.waitKey(0)
    cv2.destroyWindow("Lenna")

def create_black_image():
    black_image = np.zeros((300,300), np.uint8)
    return black_image

def create_white_image():
    white_image = np.ones((300,300), np.uint8)*255
    return white_image


white = create_white_image()
load_display(white)