import cv2

def load_display():

    #reading image
    lenna = cv2.imread("Lenna.png")

    #Create a window to display image
    cv2.namedWindow("Lenna", cv2.WINDOW_AUTOSIZE)

    #display image
    cv2.imshow("Lenna", lenna)

    #This function should be followed by waitKey function which displays the image for specified milliseconds.
    # Otherwise, it won’t display the image.
    cv2.waitKey(0)

    #destroy windows
    cv2.destroyWindow("Lenna")

def image_shape():
    # reading image
    #lenna = cv2.imread("Lenna.png")

    lenna = cv2.imread("Lenna.png", 0)

    #gets image shape
    print(lenna.shape)


def convert_to_greyscale():
    # reading image
    lenna = cv2.imread("Lenna.png")

    lenna_grey = cv2.cvtColor(lenna, cv2.COLOR_RGB2GRAY)

    cv2.imwrite("lenna_grey.png", lenna_grey)
    # Create a window to display image
    cv2.namedWindow("Lenna", cv2.WINDOW_AUTOSIZE)

    # display image
    cv2.imshow("Lenna", lenna_grey)

    # This function should be followed by waitKey function which displays the image for specified milliseconds.
    # Otherwise, it won’t display the image.
    cv2.waitKey(0)


image_shape()
