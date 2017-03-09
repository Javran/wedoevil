# wedoevil

If Chromium developers aren't doing any evil
putting an annoying popup just to prevent malicious softwares,
then it must be us that do the evil.

Dismiss "Disable developer mode extensions" popups automatically for X11 (linux users)

- Dependencies

    - [stack](https://docs.haskellstack.org/)
    - [xdotool](http://www.semicomplete.com/projects/xdotool)
    - [import](https://www.imagemagick.org/script/import.php)
    - [OpenCV (python)](http://opencv.org/)

## Usage

Switch to project root directory and execute `./DismissDialog`.
For every 2 seconds this script will try to detect the dialog and dismiss it.
Once the dialog is dismissed, this program exits automatically.

You can write a chrome wrapper in the following way to invoke this script
at each start:

```bash
pushd <path-to-this-project>
nohup ./DismissDialog.hs &  >/dev/null
popd

google-chrome "$@"
```

## When this script does not detect the dialog

Take a screenshot of the dialog, and replace `dialog.jpg` with it.
Also you need to edit [this line](https://github.com/Javran/wedoevil/blob/318059e8dbcbaf830cc284745a9c3b53ef10251d/DismissDialog.hs#L49) accordingly: while `(x,y)` means the detected top-left coordinate of `dialog.jpg`, `(x+458,y+123)` is the place where you can click `Cancel` button.
