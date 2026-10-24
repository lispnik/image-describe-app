# Image Describer

A macOS application, written in Common Lisp, that says what is in each of a
folder of images and writes it onto the file, where Spotlight and Finder's
search find it. Choose a folder; Identify asks Apple's Vision framework about
every image in it, on the Mac and without a network, and proposes a
description — *Eagle, raptor, bird, animal* — which you accept as it is or edit
first. Nothing is written until it is accepted.

Built with SBCL, [objc](https://github.com/lispnik/objc) for AppKit and Vision,
[asdf-macos-app](https://github.com/lispnik/asdf-macos-app) for the bundle,
[im](https://github.com/lispnik/im) for reading the images, FiveAM for the
tests and ocicl for the dependencies. A sibling of
image-converter-app, and built the same way.

![The window after identifying a folder: the eagle selected, its proposed description ready to accept](doc/window.png)

## Using it

- **Folder** — the images to describe. Choose… opens a folder panel, or type a
  path (`~` works) and press Return. **Include subfolders** lists the images
  below it too; a Photos library or other package is not looked inside.
- **Identify** (⌘R) — asks Vision about every image that has no description
  yet, several at once, one per processor up to eight. Identify becomes Stop
  while it runs; Stop finishes the images in hand and starts no more, waiting
  up to two seconds for them — an image still being read after that is left
  unidentified, so a slow disk never keeps the window waiting. An image
  that already has a description — one this wrote, or a comment typed into
  Finder — is left alone unless you select it and choose **Identify Again**
  (⇧⌘R).
- **Read text in images** — Vision reads any text, and the description ends
  with it: *Sign, outdoor. Text: “STOP”*.
- **Minimum confidence** — how sure Vision must be of a label for it to be in
  the description. Moving it changes the proposals at once, without
  identifying again; descriptions you typed or accepted stay as they are. If
  nothing is that sure, the most likely thing is still proposed, provided it
  has a third of the minimum.

Select an image to see it and its description below the list. Edit the
description there and press **Return**: it is written onto the file and the
next image to review is selected, so a folder can be gone through with the
keyboard alone. **Accept** (⌘S) writes the selected images' descriptions, each
its own; **Accept All** (⇧⌘S) writes every one still to review. An empty
description, accepted, removes the one on the file, and so does **Remove
Description**. Double-click an image to show it in Finder.

The Status column says where each image is: *to review* (proposed, not
written), *edited*, *saved* (the file carries what is shown), *nothing found*,
or *error* — rest the pointer on it to see why. Quitting or closing the window
with descriptions not yet accepted asks first. The options are remembered
between launches.

### What is written

Two extended attributes, each a binary property list, both of which Spotlight
imports:

| Attribute | Holds |
|---|---|
| `com.apple.metadata:kMDItemFinderComment` | the description, as written |
| `com.apple.metadata:kMDItemKeywords` | its comma-separated pieces before any `Text:` that are a few words long — the names of things, not sentences |

So Finder's search field, Spotlight and `mdfind` find an image by any word of
its description, and `mdfind 'kMDItemKeywords == "eagle"'` by keyword:

```sh
mdls -name kMDItemFinderComment -name kMDItemKeywords photo.jpg
mdfind -onlyin ~/Pictures eagle
```

The image itself is not changed. Finder's **Get Info** window does not show
the comment: it reads comments from the folder's `.DS_Store`, which only Finder
writes. Spotlight reads the attribute.

## Building

You need SBCL, ocicl, Homebrew's `tecgraf-im` (`brew install
lispnik/tap/tecgraf-im`), and checkouts of [im](https://github.com/lispnik/im),
which is not in the ocicl registry, and
[asdf-macos-app](https://github.com/lispnik/asdf-macos-app). By default both
are looked for beside this directory, at `../im` and `../asdf-macos-app`.

```sh
make deps    # restore the dependencies ocicl.csv pins, into ./ocicl/
make test    # the FiveAM suite
make run     # run from source, unbundled
make app     # build/Image Describer.app
make icon    # draw res/icon.png again
make screenshot INPUT=/a/folder/of/images   # doc/window.png
```

```sh
make app IM_DIR=~/src/im                 # im from somewhere else
make app OBJC_DIR=~/Projects/common-lisp/objc MACOS_APP_DIR=~/src/asdf-macos-app
make app MACOS_APP_DIR=                  # the asdf-macos-app ocicl.csv pins
```

The Makefile runs SBCL without your init file and with a source registry of
this tree (plus `IM_DIR`) alone, so a dependency missing from `ocicl.csv` fails
the build rather than resolving from elsewhere on your machine. The bundle
carries its own copies of IM and everything it links against, HEIF and its
codecs included, in `Contents/Frameworks`, so it runs on a Mac without
Homebrew. It is signed ad hoc unless `MACOS_SIGNING_IDENTITY` names a
certificate; put that in `local.mk`, which is not committed.

The tests that need Vision to name something use the account pictures macOS
ships in `/Library/User Pictures` (a zebra, an eagle…), and skip if they are
not there; those that need the window skip without a window server.

### Checking the bundle from a script

Nothing can click in a script, so the application has a self test: given a
folder, it identifies every image in it and its subfolders as soon as the
window is up, and quits when done, with each image's description in
`~/Library/Logs/Image Describer.log`.

```sh
open -W --env IMAGE_DESCRIBE_SELFTEST=$PWD/samples "build/Image Describer.app"
```

With `IMAGE_DESCRIBE_SELFTEST_ACCEPT=1` it accepts them all too, writing them
onto the images. The self test neither reads nor changes the options
remembered from the window. `IMAGE_DESCRIBE_WORKERS=n` sets how many images
are identified at once.

## Layout

| Path | Contents |
|---|---|
| `src/plist.lisp` | Binary property lists: writing a string or a list of strings, and reading one back |
| `src/vision.lisp` | Reading an image with IM, handing it to Vision as a CGImage, and what Vision finds |
| `src/describe.lisp` | Vision's labels and text as a description, and a description's keywords |
| `src/xattr.lisp` | The description on the file, in its extended attributes |
| `src/scan.lisp` | Finding the images in a folder |
| `src/batch.lisp` | The rows, their states, and the worker pool |
| `src/macos/` | The window, the menu and `main` |
| `tests/` | FiveAM: property lists against `plutil`, descriptions, attributes against `xattr`, scanning, Vision, the worker pool, and the real window driven without clicks |
| `tools/` | The icon, drawn pixel by pixel with IM, and the screenshot tool |

## Notes

- **Vision, not a language model.** `VNClassifyImageRequest` names one of
  about 1,300 things — objects, animals, plants, food, scenes — and
  `VNRecognizeTextRequest` reads text. Both run on the Mac; no image leaves
  it. The names are nouns, not sentences, which suits search; and they are
  sometimes wrong (a drawing of a cactus is an ant), which is why nothing is
  written without being accepted.
- **Most particular first.** Vision answers a family of names together, with
  one confidence and in alphabetical order: *animal, bird, eagle, raptor*. The
  broad ones go to the end (`*broad-labels*` in `src/describe.lisp`), so that
  the description reads *Eagle, raptor, bird, animal*.
- **IM reads, Vision looks.** Each image is read with IM, made 8-bit RGB and
  no larger than 1600 pixels across, and given to Vision as a CGImage made
  with CoreGraphics' C functions; a 6000-pixel photograph would otherwise cost
  Vision a hundred megabytes. IM keeps colours in separate planes with the
  bottom row first, so the pixels are interleaved and turned over on the way.
  What IM cannot read — camera RAW, WebP, Photoshop — Vision is given by URL,
  and ImageIO reads it.
- **RGB to RGB is a copy of nothing.** `im:convert-color-space` between two
  RGB images leaves the new one black, so an image already 8-bit RGB is used
  as it is. (Found when every wallpaper came back as a night sky.)
- **Threads, and not Grand Central Dispatch.** As in image-converter-app: a
  stock SBCL stops the world for a collection by signalling every thread in
  Lisp, and macOS will not signal a GCD worker. The workers are SBCL threads;
  each calls IM, CoreGraphics and Vision through the Objective-C bridge, whose
  caches are synchronised, inside an autorelease pool of its own. None touches
  AppKit, and none touches a row's description: the main thread looks at what
  they have finished ten times a second and proposes the descriptions, so an
  edit made while a batch runs is never written over.
- **Slow disks.** IM reads some formats in pieces of a few hundred bytes, a
  system call each — its GIF reader walks every block of every frame to count
  them — and eight workers doing that on a USB hard disk keep its head
  seeking: large animated GIFs took minutes each, and Stop
  seemed to hang waiting for them. So each file is read once straight through
  before IM opens it, one file at a time (`read-ahead` in `src/vision.lisp`),
  and IM's small reads come from memory; and Stop and Quit no longer wait for
  an image that is slow, which is safe because the workers only read.
