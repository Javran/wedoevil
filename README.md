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
