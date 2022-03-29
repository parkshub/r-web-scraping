#####RUNNING THE SCRIPT AS ADMIN#####

# The script wouldn't run without elevated privileges
# Solution was found here: https://try2explore.com/questions/10215005

import os
import sys
import win32com.shell.shell as shell
ASADMIN = 'asadmin'

if sys.argv[-1] != ASADMIN:
    script = os.path.abspath(sys.argv[0])
    params = ' '.join([script] + sys.argv[1:] + [ASADMIN])
    shell.ShellExecuteEx(lpVerb='runas', lpFile=sys.executable, lpParameters=params)

print "I am root now."



#####AUTOMATED CLICKER#####

# For more information please visit the link below
# https://pyautogui.readthedocs.io/en/latest/cheatsheet.html#mouse-functions

import pyautogui
import time


print ("Please make sure you ran this program as the administrator.")
print ("After running the RStudio script, please hover your mouse over the stop button within 5 seconds.")

for i in range(1,6):
    time.sleep(1)
    print (i)

print ("Thank you. Please refrain from moving the RStudio window.")

a=pyautogui.position()
ix=a[0]
iy=a[1]

infinite = True

seconds = int(input ("Please specify the frequency of clicks in seconds(i.e. 2hrs=7200sec): "))

while infinite==True:
    time.sleep (seconds)
    pyautogui.click(ix, iy)