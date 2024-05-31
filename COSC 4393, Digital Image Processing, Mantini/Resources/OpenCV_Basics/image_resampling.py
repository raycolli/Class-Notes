import cv2
from create_image import load_display

def resample_times_two(image):
    resampled_image = cv2.resize(image, (0,0), fx=2,fy=2, interpolation=cv2.INTER_NEAREST)
    return resampled_image

kenny = cv2.imread("kennysmall.jpg",0)
load_display(resample_times_two(kenny))