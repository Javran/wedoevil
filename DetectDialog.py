#!/usr/bin/env python3

import subprocess
import tempfile
import os
import numpy as np
import cv2
import sys

if __name__ == "__main__":
    (_, scrFP, patFP) = sys.argv
    scr = cv2.imread(scrFP)
    pat = cv2.imread(patFP)
    result = cv2.matchTemplate(scr,pat,cv2.TM_SQDIFF_NORMED)
    min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
    if result[min_loc[1],min_loc[0]] < 0.01:
        print(min_loc)
    else:
        print("Not found")
