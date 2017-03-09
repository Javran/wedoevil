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

1. Take a screenshot of the dialog. crop it properly so it only includes the content of the dialog
  and the `Cancel` button.
2. Locate the coordiate of the cancel button in your screenshot. (e.g. `(458,123)`)
3. Rename the screenshot into `<filename>_<x>_<y>.<ext>`:

    - `<filename>` can be anything you like
    - `<x>` and `<y>` are coordiates you found in previous step
    - `<ext>` is the extension name, currently only `png`, `jpg` and `bmp` are supported.

4. Store your screenshot under `sample` directory.
5. (optional) share the sample file by either making a Pull Request
  or uploading your sample file and let me know where it locates by [opening a new issue](https://github.com/Javran/wedoevil/issues/new).
